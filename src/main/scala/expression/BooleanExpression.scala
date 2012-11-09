package soba.expression


import soba.context._

/**
* The and operator.
* @param left The boolean expression used as left operand.
* @param right The boolean expression used as right operand.
*/
case class And(left:Expr[Boolean], right:Expr[Boolean])  extends BinOp[Boolean,Boolean](_ && _)(left, right)
/**
* The or operator.
* @param left The boolean expression used as left operand.
* @param right The boolean expression used as right operand.
*/
case class Or  (left:Expr[Boolean], right:Expr[Boolean]) extends BinOp[Boolean,Boolean](_ || _)(left, right)
/**
* The greater or equal operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Gte (left:Expr[Double],  right:Expr[Double])  extends BinOp[Boolean,Double](_ >= _)(left, right)
/**
* The greater operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Gt  (left:Expr[Double], right:Expr[Double])   extends BinOp[Boolean,Double](_ > _)(left, right)
/**
* The lesser operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Lt  (left:Expr[Double], right:Expr[Double])   extends BinOp[Boolean,Double](_ < _)(left, right)
/**
* The lesser or equal operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Lte (left:Expr[Double], right:Expr[Double])   extends BinOp[Boolean,Double](_ <= _)(left, right)
/**
* The equal operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Eq  (left:Expr[Double], right:Expr[Double])   extends BinOp[Boolean,Double](_ == _)(left, right)

//case class EqBoolean  (left:Expr[Boolean], right:Expr[Boolean])   extends BinOp[Boolean,Boolean](_ == _)(left, right)
/**
* The not equal operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Ne  (left:Expr[Double], right:Expr[Double])   extends BinOp[Boolean,Double](_ != _)(left, right)
/**
* The not operator.
* @param right The boolean expression used as right operand.
*/
case class Not (right:Expr[Boolean]) extends UnOp[Boolean](_.unary_!)(right)

/**
* The boolean value object.
*/
object BooleanValue{
	/**
	* Implicitly converts a boolean to a BooleanValue.
	* @param value The value to convert.
	* @return A new boolean value instance.
	*/
	implicit def booleanToValue(value:Boolean):BooleanValue = {new BooleanValue(value)}
}
/**
* Defines a boolean value.
*
* @param value the boolean value wrapped in the class.
*/
case class BooleanValue(value:Boolean) extends Expr[Boolean] {
	/**
	* Evaluate the boolean value expression.
	*
	* @param context The context used to evaluate the boolean value.
	* @return the value
	*/
	def evaluate(context:Context):Boolean = value
}
/**
* Defines a boolean attribute access value.
*
* @param name The name of the variable to look for.
* @param attribute The name of the attribute to look for.
*/
case class BooleanAttributeAccess(name:String, attribute:String) extends AttributeAccess(name, attribute)
/**
* Defines a boolean array access value.
*
* @param name The name of the array variable to look for.
* @param index The index of the attribute to look for.
*/
case class BooleanArrayAccess(name:String, index:Int) extends ArrayAccess(name,index)

/**
* Defines a boolean variable.
*
* @param name The name of the variable to look for.
*/
case class BooleanVariable(name:String) extends Variable(name)

