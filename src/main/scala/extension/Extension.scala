package soba.extension

import soba.context._
import soba.node._
import soba.SobaEngine
import java.io.Writer
import org.slf4j._

/**
* Defines an extension with a name. The name is used as an identifier for a block.
* <code>
* {% name param1 param2 %}
* The body
* {% endname %}
* </code>
*/

abstract class Extension(namep:String){

	val name = namep
	
	/**
	* Execute the function.
	* @param parameters The parameters used to execute the extension.
	* @param context The context used to execute the extension.
	*/
	def execute(node:ExtensionNode, engine:SobaEngine, stream:Writer ,context:Context)
}


class ImportTagExtension extends Extension("import"){

	var log:Logger = LoggerFactory.getLogger(getClass) 

	/**
	* Execute the function.
	* @param parameters The parameters used to execute the extension.
	* @param context The context used to execute the extension.
	*/
	override def execute(node:ExtensionNode, engine:SobaEngine, stream:Writer, context:Context){
		if(node.parameters.size > 0){
			node.parameters(0) match{
				case x:String => {
					node.body match {
						case Some(t) =>  {
							val template = deflateSubNodes(engine.parser.evaluateFile(x), t.nodes)
							engine.render(template, stream, context)
						}
						case None => engine.render(x, stream, context)
					}	
				}
				case x => log.error("Cannot execute node {} with {}", node.name, x)
			}

		}
	}
	/**
	* Deflates a sub node
	*
	* @param template the template node
	* @param the block used to deflate
	*/
	private def  deflateSubNodes(template:Template, blocks:List[AstNode]):Template = {
		var subNodes = template.nodes map {
			case BlockNode(name, y) => findBlock(BlockNode(name, y), blocks)
			case x:AstNode => x
		}
		var t = new Template(subNodes)
		log.debug("Render > {}", t)
		return t
	}
	/**
	* Find a block  in a list of blocks. 
	* @param block the block to find
	* @param blocks the blocks to look for
	* @return a block replacement. If a block replacement cannot be found, it is returned.
	*/
	private def findBlock(block:BlockNode, blocks:List[AstNode]):AstNode = {
		log.debug("Find block {} in block list", block.name)
		blocks find (x => x match {
			case b:BlockNode => b.name == block.name
			case _ => false}
		) match {
			case Some(x) => x
			case None => block
		}
	}
}