package soba.node
import soba.context._
import soba.exception._
import soba.expression._
import soba.node._
case class StringVariable(name:String) extends Variable(name) 
case class StringArrayAccess(name:String, index:Int) extends ArrayAccess(name,index)
case class StringAttributeAccess(name:String, attributeName:String) extends AttributeAccess(name, attributeName) 

case class StringValue(value:String) extends Expr[String]{

	/**
	*
	* @param context the context used to evaluate expression
	* @return the value of the expression (here the string)
	*/
	def evaluate(context:Context):String = { this.value match {
		case null => ""
		case x => x
	}}
}


