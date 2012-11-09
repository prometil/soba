package soba.node

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.expression._

import soba.exception._
import soba.expression.ArithValue._
import soba.expression.BooleanValue._
import soba.context._

import collection.mutable.Map

class BooleanExpressionTest extends FlatSpec with ShouldMatchers{

	
	it should  "resolve a and" in {
		var result = And(true, false).evaluate(Context())
		result should equal (false)
	}

	it should  "resolve a or" in {
		var result = Or(true, false).evaluate(Context())
		result should equal (true)
	}

	it should  "resolve a not" in {
		var result = Not(true).evaluate(Context())
		result should equal (false)
	}

	it should  "resolve a == b with a not equals b" in {
		var result = Eq(3, 2).evaluate(Context())
		result should equal (false)
	}
	it should  "resolve a == b with a equals b" in {
		var result = Eq(2, 2).evaluate(Context())
		result should equal (true)
	}

	it should  "resolve a != b with a equals b" in {
		var result = Ne(2, 2).evaluate(Context())
		result should equal (false)
	}
		it should  "resolve a != b with a not equals b" in {
		var result = Ne(2, 1).evaluate(Context())
		result should equal (true)
	}

	it should  "resolve a >= b with with a greater than b " in {
		var result = Gte(2, 1).evaluate(Context())
		result should equal (true)
	}

	it should  "resolve a >= b with with a equals b " in {
		var result = Gte(3, 4).evaluate(Context())
		result should equal (false)
	}
	

	it should  "resolve a >= b with with a lesser than b " in {
		var result = Gte(2, 2).evaluate(Context())
		result should equal (true)
	}
	

	it should  "resolve a > b with a greater than b" in {
		var result = Gte(2, 1).evaluate(Context())
		result should equal (true)
	}

	it should  "resolve a > b with a equals b" in {
		var result = Gt(2, 2).evaluate(Context())
		result should equal (false)
	}

	it should  "resolve a > b with a lesser than b" in {
		var result = Gt(2, 3).evaluate(Context())
		result should equal (false)
	}

	it should  "resolve a <= b with a greater than b" in {
		var result = Lte(4, 1).evaluate(Context())
		result should equal (false)
	}
	it should  "resolve a <= b with a equals b" in {
		var result = Lte(2, 2).evaluate(Context())
		result should equal (true)
	}
	it should  "resolve a <= b with a lesser than b" in {
		var result = Lte(2, 4).evaluate(Context())
		result should equal (true)
	}
	it should  "resolve a < b with a equals b " in {
		var result = Lt(2, 2).evaluate(Context())
		result should equal (false)
	}
	it should  "resolve a < b with a greater than b" in {
		var result = Lt(3, 2).evaluate(Context())
		result should equal (false)
	}

	it should  "resolve a < b with a lesser than b" in {
		var result = Lt(2, 3).evaluate(Context())
		result should equal (true)
	}

	it should  "resolve mix of logical operator and boolean operator" in {
		var result = And(Lt(2, 3),Lt(10, 30)).evaluate(Context())
		result should equal (true)
	}

	it should  "evaluate a boolean variable access" in {
		var result:Boolean = BooleanVariable("var").evaluate(Context("var" ->true))
		result should equal (true)
	}

	it should  "fail to evaluate a boolean unfound variable access" in {
		evaluating {
			BooleanVariable("var2").evaluate(Context("var" ->true))
		} should produce [VariableNotFoundException]
	}

	it should  "evaluate a boolean array access" in {
		var result:Boolean = BooleanArrayAccess("var", 2).evaluate(Context("var" -> Array(true, true,false)))
		result should equal (false)
	}

	it should  "fail to access an object index that is not a list or an array" in {
		evaluating {
			var result = BooleanArrayAccess("object",2).evaluate(Context("object" -> new ObjectBooleanWithAttribute (true)))

		} should produce [soba.exception.IllegalTypeException]
	}



	it should  "evaluate a boolean variable attribute access" in {
		var result:Boolean = BooleanAttributeAccess("object", "name").evaluate(Context("object" -> new ObjectBooleanWithAttribute (true)))
		result should equal (true)
	}


	it should  "fail to call an inexistant attribute on an  object" in {
		evaluating {
			var result = BooleanAttributeAccess("object","nonpresentfield").evaluate(Context("object" -> new ObjectBooleanWithAttribute (true)))
		} should produce [java.lang.NoSuchFieldException]
	}


}
class ObjectBooleanWithAttribute(var name:Boolean)