package soba.parser


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.expression._
import soba.exception._
import soba.expression.BooleanValue._
import soba.expression.ArithValue._
import soba.expression.ArithVariable._
import soba.expression.BooleanVariable._
import collection.mutable.Map

class ArithmeticExpressionParserTest extends FlatSpec with ShouldMatchers{

	val parser = TemplateParser();

	it should("parse binary plus") in {
		var result = parser.evaluateArithmeticExpression("19787 + 27876")
		result should be (Plus(19787,27876))

	}

	it should("parse unary plus") in {
		var result = parser.evaluateArithmeticExpression("+2")
		result should be (ArithValue(2))
	}

	it should("parse unary plus with ident") in {
		var result = parser.evaluateArithmeticExpression("+var")
		result should be ( ArithVariable("var"))
	}
	it should("parse binary plus with ident and number  ") in {
		var result = parser.evaluateArithmeticExpression("1 + var")
		result should be (Plus(1,ArithVariable("var")))
	}

	it should("parse binary minus") in {
		var result = parser.evaluateArithmeticExpression("1 - 2")
		result should be (Minus(1,2))
	}

	it should("parse binary minus with ident and number  ") in {
		var result = parser.evaluateArithmeticExpression("1 - var")
		result should be (Minus(1,ArithVariable("var")))
	}
	
	it should("parse unary minus") in {
		var result = parser.evaluateArithmeticExpression("-2")
		result should be (MinusSign(2))
	}

	it should("parse unary minus with ident") in {
		var result = parser.evaluateArithmeticExpression("-var")
		result should be (MinusSign(ArithVariable("var")))
	}

	it should("parse binary div") in {
		var result = parser.evaluateArithmeticExpression("1 / 2")
		result should be (Div(1,2))
	}
	
	it should("parse binary div with ident and number  ") in {
		var result = parser.evaluateArithmeticExpression("1 / var")
		result should be (Div(1,ArithVariable("var")))
	}

	it should("parse binary mult") in {
		var result = parser.evaluateArithmeticExpression("1 * 2")
		result should be (Mult(1, 2))
	}

	it should("parse multiple binary mult") in {
		var result = parser.evaluateArithmeticExpression("1 * (2 * 3)")
		result should be (Mult(1,Mult(2, 3)))
	}
	
	it should("parse binary mult with ident and number ") in {
		var result = parser.evaluateArithmeticExpression("1 * var")
		result should be (Mult(1, ArithVariable("var")))
	}

	it should("parse mixed div and mult") in {
		var result = parser.evaluateArithmeticExpression("1 * 2 / 3")
		result should be (Div(Mult(1, 2),3))
	}

	it should("parse mixed mult add") in {
		var result = parser.evaluateArithmeticExpression("(1 + 2) * 3")
		result should be (Mult(Plus(1, 2),3))
	}

	it should("parse single number") in {
		var result = parser.evaluateArithmeticExpression("3098")
		result should be (ArithValue(3098))
	}
	it should("parse single float") in {
		var result = parser.evaluateArithmeticExpression("3.14215125")
		result should be (ArithValue(3.14215125))
	}

	it should("parse attribute access") in {
		var result = parser.evaluateArithmeticExpression("var.attribute")
		result should be (ArithAttributeAccess("var","attribute"))
	}
	it should("parse array access") in {
		var result = parser.evaluateArithmeticExpression("a[1]")
		result should be (ArithArrayAccess("a",1))
	}
}