package soba.expression

import soba.context._

/**
* The plus operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Plus (left:Expr[Double], right:Expr[Double]) extends BinOp[Double,Double](_ + _)(left, right)
/**
* The minus operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Minus (left:Expr[Double], right:Expr[Double]) extends BinOp[Double,Double](_ - _)(left, right)
/**
* The mult operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Mult (left:Expr[Double], right:Expr[Double]) extends BinOp[Double,Double](_ * _)(left, right)
/**
* The div operator.
* @param left The arithmetic expression used as left operand.
* @param right The arithmetic expression used as right operand.
*/
case class Div (left:Expr[Double], right:Expr[Double]) extends BinOp[Double,Double](_ / _)(left, right)
/**
* The minus sign operator.
* @param right The arithmetic expression used as right operand.
*/
case class MinusSign (right:Expr[Double]) extends UnOp[Double](_.unary_-)(right)

/**
* The artih value companion object.
*/
object ArithValue{
	/**
	* Convertes a double to an arith value.
	*
	* @param value The value to convert.
	* @return A new arithmetic value.
	*/
	implicit def doubleToValue(value:Double):ArithValue = {new ArithValue(value)}
	/**
	* Convertes an integer to an arith value.
	*
	* @param value The value to convert.
	* @return A new arithmetic value.
	*/
	implicit def intToValue(value:Int):ArithValue = {new ArithValue(value.toDouble)}
}
/**
* Defines a arithmetic value.
*
* @param value the boolean value wrapped in the class.
*/
case class ArithValue(value:Double) extends Expr[Double] {
	/**
	* Evaluate the arithmetic value expression.
	*
	* @param context The context used to evaluate the boolean value.
	* @return the value
	*/
	def evaluate(context:Context):Double = value
}
case class ArithAttributeAccess(name:String, attributeName:String) extends AttributeAccess(name, attributeName)

/**
* Defines a artihmetic array access value.
*
* @param name The name of the array variable to look for.
* @param index The index of the attribute to look for.
*/
case class ArithArrayAccess(name:String, index:Int) extends ArrayAccess(name,index)

/**
* Defines an Arithmetic variable.
*
* @param name The name of the variable to look for.
*/
case class ArithVariable(name:String) extends Variable(name)