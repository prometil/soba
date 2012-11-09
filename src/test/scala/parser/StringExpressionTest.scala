package soba.parser


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.exception._
import collection.mutable.Map

class StringExpressionText extends FlatSpec with ShouldMatchers{

	val parser = TemplateParser()

	it should("parse a string with double quotes") in {
		var result = parser.evaluateStringExpression("\"hello world\"")
		result should be (StringValue("hello world"))

	}

	it should("parse a string with double quotes with quotes in it") in {
		var result = parser.evaluateStringExpression("\"hello 'the' world\"")
		result should be (StringValue ("hello 'the' world"))

	}

	it should("parse a string with double quotes with double quotes in it") in {
		var result = parser.evaluateStringExpression("\"hello \\\"the\\\" world\"")
		result should be (StringValue ("hello \"the\" world"))

	}

	it should("parse a string with double quotes with controle in it") in {
		var result = parser.evaluateStringExpression("\"hello the\n world\"")
		result should be (StringValue ("hello the\n world"))

	}
}