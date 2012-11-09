package soba.node

import java.io.Writer
import soba.exception._
import collection.mutable.Map
import soba.expression._
import soba.context._
import scala.collection.JavaConversions._

//the top level node
abstract class AstNode

/**
* The template node
*
* The template node is a list of Nodes
* @param nodes the nodes list
*/
case class Template(nodes:List[AstNode]) extends AstNode

/**
* The include node enable the inclusion of a node at a given place
*
* @param templatePath the template path of the template to include
*/
case class IncludeNode(templatePath:String) extends AstNode

/**
* The extend node is used to create a child of a parent node
* When a template path is extends, only block definition should be extracted
*
* @param templatePath the template path of the template to include
*/
case class ExtendsNode(templatePath:String) extends AstNode

/**
* The block node
*
* The block node is used to define a block and its value
* If the node is node remplace on deflating, the default body is used
*
* @param the name of the block
* @param body the template to replace
*
**/
case class BlockNode(name:String, body:Option[Template]) extends AstNode

/**
* The for node
*
* The for node is a loop which takes a cursor, a variable and a body to render. It renders the body, for element of the variable, and place the variable in the cursor
*
* @param cursor the name of the cursor to put in the context
* @param name the name of the variable on which the loop is done
* @param body the body to render
*
*/
case class ForNode(cursor:String, name:Any, body:Option[Template]) extends AstNode

/**
* A text node
*
* A text node wrapps raw text
*
* @param the text of the node
**/
case class TextNode(text:String) extends AstNode

/**
* An arithmetic expression Node
*
* @param expression the expression to evaluate.
*/
case class EchoNode(expression:Expr[Any]) extends AstNode
/**
* Represents a If node
*
* A if node is composed of an expression, a list of else if and maybe a else node.

* @param expressionNode the boolean expression evaluated to execute the if
* @param template the template to render if the expression succeeds
* @param elseifNodes a list of elseif node that will be evaluated if the boolean expression of the if fails.
* @param elseNode a else node executed if the if fails.
**/
case class IfNode(expressionNode:Expr[Boolean], template:Option[Template],elseifNodes:List[ElseIfNode] , elseNode:Option[ElseNode]) extends AstNode
/**
* A else if node
*
* The else if node is composed of a boolean expression, and a template to be executed if the boolean expression is right
*
* @param expressionNode the boolean expression to evaluate
* @param template the template to render if the expression succeeds
*/
case class ElseIfNode(expressionNode:Expr[Boolean], template:Option[Template]) extends AstNode

/**
* A else node
*
* The else node is compose of a template, and is evaluate if any other alternative fails
*
* @param template the template to render
*/
case class ElseNode(template:Option[Template]) extends AstNode

/**
* The extension node
*/
case class ExtensionNode(name:String, parameters:List[Any], body:Option[Template]) extends AstNode

case class SimpleAttributeAccess(name:String, attributeName:String) extends AttributeAccess(name, attributeName)

