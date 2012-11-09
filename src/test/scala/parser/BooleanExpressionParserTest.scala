package soba.parser


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.expression._
import soba.exception._
import collection.mutable.Map
import soba.expression.BooleanValue._
import soba.expression.ArithValue._
import soba.expression.ArithVariable._
import soba.expression.BooleanVariable._

class BooleanExpressionParserTest extends FlatSpec with ShouldMatchers{


	val parser = new TemplateParser()

	it should("parse attribute access") in {
		var result = parser.evaluateBooleanExpression("variable.attribute")
		result should be (BooleanAttributeAccess("variable","attribute"))
	}
	it should("parse array access") in {
		var result = parser.evaluateBooleanExpression("a")
		result should be (BooleanVariable("a"))
	}
	it should("parse variable access") in {
		var result = parser.evaluateBooleanExpression("a[1]")
		result should be (BooleanArrayAccess("a",1))
	}

	it should("parse false") in {
		var result = parser.evaluateBooleanExpression("false")
		result should be (BooleanValue(false))
	}

	it should("parse true") in {
		var result = parser.evaluateBooleanExpression("true")
		result should be (BooleanValue(true))
	}

	it should("parse or") in {
		var result = parser.evaluateBooleanExpression("a or b")
		result should be (Or(BooleanVariable("a"),BooleanVariable("b")))
	}

	it should("parse and") in {
		var result = parser.evaluateBooleanExpression("a or b")
		result should be (Or(BooleanVariable("a"),BooleanVariable("b")))
	}

	it should("parse and and or with or first") in {
		var result = parser.evaluateBooleanExpression("a or b and c")
		result should be ( Or(BooleanVariable("a"),And(BooleanVariable("b"), BooleanVariable("c"))))
	}

	it should("parse and and or with and first") in {
		var result = parser.evaluateBooleanExpression("a and b or c")
		result should be (Or(And(BooleanVariable("a"),BooleanVariable("b")),BooleanVariable("c")) )
	}

	it should("parse mult or") in {
		var result = parser.evaluateBooleanExpression("a or b or c")
		result should be (Or(Or(BooleanVariable("a"),BooleanVariable("b")),BooleanVariable("c")) )
	}

	it should("parse mult and") in {
		var result = parser.evaluateBooleanExpression("a and b and c")
		result should be (And(And(BooleanVariable("a"),BooleanVariable("b")),BooleanVariable("c")))
	}

	it should("parse not") in {
		var result = parser.evaluateBooleanExpression("not a")
		result should be (Not(BooleanVariable("a")))
	}

	it should("parse not with and ") in {
		var result = parser.evaluateBooleanExpression("not a and b")
		result should be ( Not(And(BooleanVariable("a"),BooleanVariable("b"))))
	}

	it should("parse not with and, with parenthesis") in {
		var result = parser.evaluateBooleanExpression("(not a) and b")
		result should be (And(Not(BooleanVariable("a")),BooleanVariable("b")))
	}


	it should("parse greater than or equal") in {
		var result = parser.evaluateBooleanExpression("1 >= 2")
		result should be (Gte(1,2))
	}

	it should("parse greater than ") in {
		var result = parser.evaluateBooleanExpression("1 > 2")
		result should be (Gt(1,2))
	}

	it should("parse lesser than or equal") in {
		var result = parser.evaluateBooleanExpression("1 <= 2")
		result should be (Lte(1,2))
	}


	it should("parse lesser than") in {
		var result = parser.evaluateBooleanExpression("1 < 2")
		result should be (Lt(1,2))
	}

	it should("parse equal with double") in {
		var result = parser.evaluateBooleanExpression("1 == 2")
		result should be (Eq(1,2))
	}
	it should("parse equal with double and variable") in {
		var result = parser.evaluateBooleanExpression("2 == a")
		result should be (Eq(ArithValue(2.0),ArithVariable("a")))
	}

	it should("parse equal with variable and variable") in {
		var result = parser.evaluateBooleanExpression("a == b")
		result should be (Eq(ArithVariable("a"),ArithVariable("b")))
	}

	it should("parse equal with double and variable inversed") in {
		var result = parser.evaluateBooleanExpression("a == 2")
		result should be (Eq(ArithVariable("a"),ArithValue(2.0)))
	}

	/*it should("parse equal with boolean") in {
		var result = parser.evaluateBooleanExpression("true == false")
		result should be (EqBoolean(BooleanValue(true),BooleanValue(false)))
	}*/

	it should("parse not equal with double") in {
		var result = parser.evaluateBooleanExpression("1 != 2")
		result should be (Ne(1,2))
	}

	/*it should("parse not equal boolean") in {
		var result = parser.evaluateBooleanExpression("true != false")
		result should be (Ne(true,false))
	}*/
}