package soba.expression

import soba.exception._
import soba.context._
import soba.node._
import collection.mutable.Map

/**
* Defines an expression 
*/
abstract class Expr[+T] {
	def evaluate(context:Context):T
}

/**
* Defines an array access. 
*
* This class defines an access for a given index to an array-like structure.
* 
* @param name the name of the array to access.
* @param index the index to access.
*
* @throws IllegalTypeException if the variable it tries to access is not an array-like structure.
*/
abstract class ArrayAccess[T <: AnyRef : Manifest](name:String, index:Int) extends Expr[T]{
	def evaluate(context:Context):T = {
		context.get(name) match {
			case x:Seq[_] => cast(x(index))
			case x:Array[_] => cast(x(index))
			case _ => throw new IllegalTypeException("The variable is a not a sequence")
		}
	}
	def cast(a:Any):T = manifest[T].erasure.cast(a).asInstanceOf[T]
}
/**
* Defines an abstract  attribute access. 
*
* This class defines an attribute for a given attribute and a given name.
* 
* @param name the name of the array to access.
* @param attributeName the name of the attribute to access.
*/
abstract class AttributeAccess[T <: AnyRef : Manifest](name:String, attributeName:String) extends Expr[T ] {
	def evaluate(context:Context):T = cast(context.getAttributeVariable(name, attributeName))
	def cast(a : Any) : T  = manifest[T].erasure.cast(a).asInstanceOf[T]
}
/**
* Defines an abstract variable access. 
*
* This class defines a variable acess.
* 
* @param name the name of the variable to access.
*/
abstract class Variable[T <: AnyRef : Manifest](name:String) extends Expr[T] {
	/**
	* Return the variable value
	* @param context the context into which the variable is searched
	* @return the variable value
	*/
	def evaluate(context:Context):T = cast(context.get(name))

	def cast(a : Any) : T  = manifest[T].erasure.cast(a).asInstanceOf[T]
}
/**
* Defines an abtract binary operation.
*
* This class defines a binary operation with a left and a right operand
* 
* @param f the operation to apply
* @param left the left operand of the binary operation
* @param right the right operand of the binary operation
*/
abstract class BinOp[T,C](f:(C,C) => T)(left:Expr[C], right:Expr[C]) extends Expr[T]{
	/**
	* Evaluate the application of the operator on the left and right evaluation results.
	*
	* @param context the context into which the variables are searched
	* @return the operation result when applied on the left and the right operand
	*/
	def evaluate(context:Context):T = f(left.evaluate(context), right.evaluate(context))
}
/**
* Defines an abstract unary operation
*
* @param f the operation to apply
* @param right the right operand of the unary operation
**/
abstract class UnOp[T](f:T => T)(right:Expr[T]) extends Expr[T] {
	/**
	* Evaluate the application of the operator on the right evaluation result.
	*
	* @param context The context into which the variables are searched.
	* @return The operation result when applied on the right operand.
	*/
	def evaluate(context:Context):T = f(right.evaluate(context))
}