package soba.node

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.expression._
import soba.context._
import soba.expression.ArithValue._
import soba.exception._


class ArithmeticExpressionTest extends FlatSpec with ShouldMatchers{

	it should  "sum value" in {
		var result = Plus(1,2).evaluate(Context())
		result should equal (3)
	}

	it should  "mult value" in {
		var result = Mult(3,2).evaluate(Context())
		result should equal (6)
	}

	it should  "div value" in {
		var result = Div(1,2).evaluate(Context())
		result should equal (0.5)
	}

	it should  "minus value" in {
		var result = Minus(1, 2).evaluate(Context())
		result should equal (-1)
	}

	it should  "negate a value" in {
		var result = MinusSign(1).evaluate(Context())
		result should equal (-1)
	}

	it should  "access a variable from context" in {
		var result = Plus(1, ArithVariable("varDouble")).evaluate(Context("varDouble" -> 2d))
		result should equal (3)
	}

	it should  "call an attribute of an object from the context" in {
		var result = Plus(1, ArithAttributeAccess("object","name")).evaluate(Context("object" -> new ObjectDoubleWithAttribute (2d)))
		result should equal (3)
	}

	it should  "fail to call an inexistant attribute on an  object" in {
		evaluating {
			var result = Plus(1, ArithAttributeAccess("object","nonpresentfield")).evaluate(Context("object" -> new ObjectDoubleWithAttribute (2d)))
		} should produce [java.lang.NoSuchFieldException]
	}

	it should  "fail to call an inexistant object on an object" in {
		evaluating {
			var result = Plus(1, ArithAttributeAccess("nonpo","nonpresentfield")).evaluate(Context())
		} should produce [VariableNotFoundException]
	}

	it should  "fail to access an object index that is not a list or an array" in {
		evaluating {
			var result = Plus(1, ArithArrayAccess("object",2)).evaluate(Context("object" -> new ObjectDoubleWithAttribute (2d)))

		} should produce [soba.exception.IllegalTypeException]
	}

	it should  "access an array value" in {
		var result = Plus(1, ArithArrayAccess("var",2)).evaluate(Context("var" -> Array(1d,2d,3d)))
		result should equal (4)
	}

	it should  "access an seq value" in {
		var result = Plus(1d, ArithArrayAccess("var",2)).evaluate(Context("var" -> List(1d,2d,3d)))
		result should equal (4)
	}
}

class ObjectDoubleWithAttribute(var name:Double)