package soba.context

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.expression._
import soba.context._
import soba.expression.ArithValue._
import soba.exception._


class ContextTest extends FlatSpec with ShouldMatchers{

  it should("throw an exception when trying to access and illegal element") in {
    var context = Context();
    evaluating {
     context.get("myvariable");
    } should produce [VariableNotFoundException]
  }
  it should("return the default value if the element is not found") in {
    var context = Context();
    var result = context.getOrElse("myvariable", List());
    result should be (List())
  }
  it should("return thevalue if the element is found with a get or else") in {
    var context = Context();
    context.put("myvar", List(1,2,3))
    var result = context.getOrElse("myvar", List());
    result should be (List(1,2,3))
  }
  it should("return thevalue if the element is found ") in {
    var context = Context();
    context.put("myvar", List(1,2,3))
    var result = context.get("myvar");
    result should be (List(1,2,3))
  }
}