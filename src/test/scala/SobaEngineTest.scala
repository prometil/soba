package soba

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.expression._
import soba.context._
import soba.expression.ArithValue._
import soba.exception._
import java.io.StringWriter;



class SobaEngineTest extends FlatSpec with ShouldMatchers{
  var engine = SobaEngine()


  it should("Render a for without variable") in {
    var context = Context()
    var string = "{% for error in errors %} {{error}} {% endfor %}"
    var writer = new StringWriter()
    engine.renderString(string, writer, context)
    writer.toString() should be("")
  }


  it should("Render a for with an array") in {
    var context = Context()
    context.put("myarray", Array("a", "b", "c"))
    var string = "{% for a in myarray %}{{a}}{% endfor %}"
    var writer = new StringWriter()
    engine.renderString(string, writer, context)
    writer.toString() should be("abc")
  }
  
   

}