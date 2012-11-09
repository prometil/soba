package soba.parser

import org.slf4j._
import soba.expression._
import soba.node._
import soba.exception._
import soba.util._
import scala.util.parsing.combinator._;
import scala.io.Source;
import java.io.File;
import java.io.InputStream


object TemplateParser{

  def apply() = new TemplateParser();
}
/***
* The soba template parser, which return a Node tree.
*
**/
class TemplateParser extends JavaTokenParsers with ArithmeticExpressionParser with BooleanExpressionParser with StringExpressionParser {

  //the template parser.
	var log:Logger = LoggerFactory.getLogger(getClass)


  def template:Parser[Template] = rep1(raw  | block | forBlock | echoBlock | ifBlock | extendsBlock | extension) ^^ (Template(_))

 /* lazy val includeFunction:Parser[IncludeNode] = "{% include " ~> stringLiteral <~ "%}" ^^ {x => IncludeNode(x.slice(1, x.length-1))}*/

  private lazy val extendsBlock:Parser[ExtendsNode] = "{% extends " ~> stringLiteral <~ "%}" ^^ {x => ExtendsNode(x.slice(1, x.length-1))}

  private lazy val block:Parser[BlockNode] = "{% block " ~> name ~ "%}" ~ opt(template) <~ "{%" ~ "endblock" ~ "%}" ^^ {
    case    n ~ "%}" ~  e   => BlockNode(n, e)
  }

  private lazy val echoBlock:Parser[EchoNode] = "{{" ~> ( arithExpr | stringExpr) <~ "}}" ^^ {EchoNode(_)}

  //controle strucuture
  private lazy val forBlock:Parser[ForNode] = "{% for " ~> name ~ "in" ~  (attributeAccess | name) ~ "%}" ~ opt(template) <~ "{% endfor %}" ^^ {
    case    c ~ "in" ~ i ~ "%}" ~  e   => ForNode(c,i,e)
  }

  protected lazy val attributeAccess:PackratParser[SimpleAttributeAccess] = ident ~"."~ ident ^^ {case i1 ~"."~ i2 => SimpleAttributeAccess(i1, i2)}


  private lazy val ifBlock:Parser[IfNode] = "{% if " ~> boolExpr ~ "%}" ~ opt(template) ~ rep(elseifBlock) ~ opt(elseBlock) <~ "{% endif %}" ^^ {
      case n ~ "%}" ~ t ~ ei ~ e => IfNode(n , t, ei, e)
    }

  private lazy val elseifBlock:Parser[ElseIfNode] = "{% elseif " ~> boolExpr ~ "%}" ~ opt(template)   ^^ {
      case n ~ "%}" ~ t => ElseIfNode(n , t)
  }

  private lazy val elseBlock:Parser[ElseNode] = "{% else %}" ~> opt(template)  ^^ {ElseNode(_)}

  private lazy val name:Parser[String] = ident ^^ { case name => name.toString()}


  private lazy val extension:Parser[ExtensionNode] = "{%"~> ident ~ parameters ~ "%}" ^^ {
      case i ~ p ~ "%}" => ExtensionNode(i, p , None)
  }

  private lazy val parameters:Parser[List[Any]] = rep1(stringParameter | floatingPointNumber | attributeAccess | ident |  booleanLiteral | floatLiteral)

  private lazy val stringParameter:Parser[String] = stringLiteral ^^ { _.drop(1).dropRight(1)}
  /**
  * The raw text partser transformed as {@link TextNode}
  */
  private lazy val raw:Parser[TextNode] = text ^^ {new TextNode(_)}

  /**
  * Create a parser to parse text.
  *
  * This parser grab text until it encounter a start tag.
  *
  * @return a parser
  **/
  def text:Parser[String] = new Parser[String] {
      def apply(in: Input) = {
          val source = in.source
          val offset = in.offset
          var j = offset
          var found = false
          val limiter = List('{', '%', '#')
          while (j < source.length &&  !found ) {
            if (j+1 < source.length && (source.charAt(j) == '{' && limiter.contains(source.charAt(j+1)))){
              found = true;
            }
            else{
              j += 1
            }
          }
          if (j != offset){
            Success(source.subSequence(offset, j).toString, in.drop(j - offset))
          }
          else  {
            val found = if (offset == source.length()) "end of source" else "`"+source.charAt(offset)+"'"
            Failure("` { ' expected but "+found+" found", in.drop(offset - offset))
          }
      }
  }

  /**
  * Parse a file
  * @param file the file to parse
  */
  def evaluateFile(templatePath:String):Template = {
    ResourceLoader.load(templatePath) match {
      case Some(stream) =>   {
        val source = scala.io.Source.fromInputStream(stream)
        val lines = source.mkString
        source.close()
        evaluate(lines)
      }
      case None => throw new ResourceNotFoundException(templatePath);
    }

  }

  /**
  * Evaluattes a string template using a set of rules
  * @param templateString the string to evaluate
  * @return The evaluated template
  **/
  def evaluate(templateString:String):Template = {
  	var result = parseAll(template,templateString)
  		result match {
      case Success(x, _) => result.get
      case Failure(err, next) => throw ParsingException("failed to parse input " +"(line " + next.pos.line + ", column " + next.pos.column + "):\n" + err + "\n" + next.pos.longString);
    }
  }
}