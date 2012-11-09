package soba;

import java.io.File
import org.slf4j._
import soba.parser._
import com.google.common.base.Stopwatch
import java.util.concurrent.TimeUnit
import java.io.StringWriter
import collection.mutable.Map
import soba.node._
import soba.context._
import soba.extension._
import soba.exception._
import java.io.Writer
import scala.collection.JavaConversions._
/**
* The sona engine
*/
object SobaEngine  {
	//the soba engine logger
	private var log:Logger = LoggerFactory.getLogger(getClass)
	/**
	* Creates a new SobaEngine
	* @return a new SobaEngine
	*/
	def apply() = new SobaEngine()
}

/**
* The soba template engine
*
* The template engine manage the output from the parser in a cache, depending on the cache parameter
**/
class SobaEngine(){

	private val log:Logger = LoggerFactory.getLogger(getClass)

	lazy val parser = new TemplateParser()

	private var extensions = List[Extension](new ImportTagExtension())

	/**
	* Register a template engine extension
	* @param extension the extension to register
	*/
	def registerExtension(extension:Extension){
		log.debug("Extension is {}", extension)
		extensions = extension::extensions
	}

	/**
	* Render a file path as a string using a context
	*
	* @param file The file to render.
	* @param context The context to use to render.
	* @return The template rendered as a string.
	*/
   	def render (file:String, context:Context):String = {
   		val s = new StringWriter()
   		render(file , s, context)
   		s.toString
   	}
   	/**
	* Render a file path to a writer using a context.
	*
	* @param file The file to render.
	* @param stream The writer to which the node will be rendered.
	* @param context The context to use to render.
	*/
   	def render (file:String, stream:Writer, context:Context)
   	 {
   		var template = parser.evaluateFile(file)
   		render(template ,stream, context)
   	}

    /**
    * Render a string 
    * @param templateString  the string containing the template  to render
    * @param stream the stream on which the writer will be written
    * @param context The context used to render the template
    */
    def renderString(templateString:String, stream:Writer, context:Context) {
      var template = parser.evaluate(templateString)
      render(template, stream, context)
    }

   	/**
   	* Renders a node.
   	*
   	* @param Node the node to render.
   	* @param Stream the stream to wich the node will be renderered.
   	* @param Context the context used to render the node.
   	**/
    def render(node:AstNode, stream:Writer, context:Context){
   		//parse each eelment
   		node match {
   			case x:BlockNode => x.body match{
				case Some(x) => render(x, stream, context)
				case None => None
			}
   			case x:ForNode => renderFor(x, stream, context)
   			case x:TextNode => stream.write(x.text)
   			case x:EchoNode => stream.write(sanitize(x.expression.evaluate(context) match {
   				case null => ""
   				case x => x.toString
   				}))
   			case x:IfNode => renderIf(x, stream, context)
   			case x:ElseIfNode => x.template match {
				case Some(x) => render(x, stream, context)
				case None =>
			}
   			case x:ElseNode => x.template match {
				case Some(x) => render(x, stream, context)
				case None =>
			}

   			case x:ExtensionNode => renderExtension(x, stream, context)
   			case x:Template => {
   				deflate(x.nodes).foreach{
	   				x => render(x, stream, context)
	   			}
	   		}
			case x => log.error ("Unknown node {}", x)
		}
   	}

   	private def deflate(nodes:List[AstNode]):List[AstNode] = {
   		nodes match {
   			case node::tail => node match {
   				case n:ExtendsNode =>  deflateExtends(n, tail)
   				case x => x::deflate(tail)
   			}
   			case Nil => Nil
   		}

   	}

   	private def deflateInclude(node:IncludeNode):List[AstNode] = {
   		parser.evaluateFile(node.templatePath).nodes
   	}


   	private def deflateExtends(node:ExtendsNode, replacements:List[AstNode]):List[AstNode] = {
   		var template = parser.evaluateFile(node.templatePath)
   		deflate(deflateSubNodes(template.nodes, replacements))
   	}

   	/**
	* Deflates a sub node
	*
	* @param template the template node
	* @param the block used to deflate
	*/
	private def  deflateSubNodes(nodes:List[AstNode], blocks:List[AstNode]):List[AstNode] = {
		var subNodes:List[AstNode] = nodes map {
			case BlockNode(name, y) => {
				findBlock(BlockNode(name, y), blocks) match {
					case Some(x) => x
					case None => y match {
							case Some(t) => BlockNode(name, Some(Template(deflateSubNodes(t.nodes, blocks))))
							case None => BlockNode(name,y)
						}
				}
			}
			case x:AstNode => x
		}
		subNodes
	}
	/**
	* Find a block  in a list of blocks.
	* @param block the block to find
	* @param blocks the blocks to look for
	* @return a block replacement. If a block replacement cannot be found, it is returned.
	*/
	private def findBlock(block:BlockNode, blocks:List[AstNode]):Option[AstNode] = {
		log.debug("look for block {} in {}", block.name, blocks)
		blocks find (x => x match {
			case b:BlockNode =>  b.name == block.name
			case _ => false}
		)
	}

   	/**
   	* Sanitizes a String.
   	*
   	* Escape all html in a string  by replace < by &lt;, > by &gt; etc..
   	*
   	* @param s The string to sanitize.
   	* @return The sanitized string.
   	*
   	*/
   	private def sanitize(s:String):String = {
   		var reps = Map('<' -> "&lt;", '>' -> "&gt;", '"' -> "&quot;", '\'' -> "&acute;")
   		var result = new StringBuilder()
   		s.foreach( x => reps.get(x) match{
   			case Some(rep) => result.append(rep)
   			case None => result.append(x)
   		})
   		result.toString
   	}
   	/**
	* Renders an extension.
	*
	* @param node The extension Node to render.
	* @param stream The stream to which the node will be rendered.
	* @param context The context used to render the node.
	**/
   	private def renderExtension(node:ExtensionNode, stream:Writer, context:Context){
   		//look for the extension
   		extensions.find(e => (e.name == node.name)) match{
   			case Some(n) => n.execute(node, this, stream, context)
   			case None => throw new UnfoundExtensionException("Cannot found "+node.name+" extension");
   		}
   	}

   	/**
  	* Renders a for node.
  	*
  	* @param node The for Node to render.
  	* @param stream The stream to which the node will be rendered.
  	* @param context The context used to render the node.
  	**/
   	def renderFor(forNode:ForNode, stream:Writer, context:Context){
		forNode.body match{
			case Some(bn) =>{
        val value = forNode.name match {
          case x:String => context.getOrElse(x, List())

          case x:SimpleAttributeAccess => context.getAttributeVariable(x.name, x.attributeName)
           case l => throw new IllegalTypeException("Variable "+forNode.name+" is not a sequence "+l.getClass)
        }
        value match {
          case l:Seq[_] => iterate(l,forNode.cursor,bn, stream, context)
          case l:Array[_] =>  iterate(l,forNode.cursor,bn, stream, context)
          case l:java.util.Collection[_] =>  iterate(l.toList,forNode.cursor, bn, stream, context)
          case l => throw new IllegalTypeException("Variable "+forNode.name+" is not a sequence "+l.getClass)
        }
      }
			case None => None
		}
	}
	/**
	* Iterates over a list to render the body
	*
	* @param seq The sequence to iterate on
	* @param cursor The name of the cursor to put in the context for the body
	* @param body The body to render
	* @param stream The stream to which the body will be rendered
	* @param context The context to give to the body rendering
	*/
	def iterate(seq:Seq[_], cursor:String, body:Template, stream:Writer, context:Context){
		seq.toList.foreach(c =>{
			context.scope{
		   		context.put(cursor,c)
		   		render(body, stream, context)}
		   	}
		)
	}



	/**
	* Renders a If node.
	*
	* @param node The if node to render
	* @param stream The stream to which the body will be rendered.
	* @param context The context use to render the if node.
	**/
	def renderIf(node:IfNode, stream:Writer, context:Context ){
		if( node.expressionNode.evaluate(context)){
			node.template match {
				case Some(x) => context.scope{ render(x, stream, context)}
				case None =>
			}
		}
		else{
			node.elseifNodes.find(e => e.expressionNode.evaluate(context)) match {
				case Some(x) => {
					context.scope{
						render(x,stream, context)
					}
				}
				case None => node.elseNode match {
					case Some(x) => render(x,stream, context.stack())
					case None =>
				}
			}

		}
	}

}




