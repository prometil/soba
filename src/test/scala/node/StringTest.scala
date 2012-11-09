package soba.node

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.expression._
import soba.context._
import soba.expression.ArithValue._
import soba.exception._

class StringTest extends FlatSpec with ShouldMatchers{


	it should  "evaluate a string value" in {
		var result = StringValue("test").evaluate(Context())
		result should equal ("test")
	}

	it should  "evavalue a string variable access" in {
		var result:String = StringVariable("var").evaluate(Context("var" ->"test"))
		result should equal ("test")
	}
	

	it should  "evavalue a string array access" in {
		var result:String = StringArrayAccess("var", 2).evaluate(Context("var" -> Array("a","b","c")))
		result should equal ("c")
	}

	it should  "evavalue a string variable attribute access" in {
		var result:String = StringAttributeAccess("object", "name").evaluate(Context("object" -> new ObjectWithStringAttribute("test")))
		result should equal ("test")
	}


	it should  "fail to call an inexistant attribute on an  object" in {
		evaluating {
			var result = StringAttributeAccess("object","nonpresentfield").evaluate(Context("object" -> new ObjectWithStringAttribute("test")))
		} should produce [java.lang.NoSuchFieldException]
	}
	it should  "fail to access an object index that is not a list or an array" in {
		evaluating {
			var result =  BooleanArrayAccess("object",2).evaluate(Context("object" -> new ObjectWithStringAttribute("test")))

		} should produce [soba.exception.IllegalTypeException]
	}


	
}
class ObjectWithStringAttribute(var name:String)