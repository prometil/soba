package soba.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import soba.node._
import soba.exception._
import soba.extension._
import collection.mutable.Map

class ParserTest extends FlatSpec with ShouldMatchers{

	val parser = TemplateParser()

	it should  "parse include tag" in {
		val returnValue = parser.evaluate("{% include \"a.html\" %}");
		returnValue.nodes.length should equal (1);
		val subNode = returnValue.nodes(0)
		subNode should be (ExtensionNode("include",List("a.html"),None))
	}

	it should  "not parse unended include tag" in {
		evaluating {
			val returnValue = parser.evaluate("{% include \"a.html\"");
		} should produce [ParsingException]

	}

	it should  "not parse include tag without path" in {
		evaluating {
			val returnValue = parser.evaluate("{% include %}");
		} should produce [ParsingException]

	}



	it should  "parse extends tag" in {
		val returnValue = parser.evaluate("{% extends \"a.html\" %}");
		returnValue.nodes.length should equal (1);
		val subNode = returnValue.nodes(0)
		subNode should be (ExtendsNode("a.html"))
	}

	it should  "not parse unended extends tag" in {
		evaluating {
			val returnValue = parser.evaluate("{% extends \"a.html\"");
		} should produce [ParsingException]

	}

	it should  "not parse extends tag without path" in {
		evaluating {
			val returnValue = parser.evaluate("{% extends %}");
		} should produce [ParsingException]

	}


	it should  "parse block" in {
		val returnValue = parser.evaluate("{% block test %} template {% endblock %}");
		returnValue.nodes.length should equal (1);
		val subNode = returnValue.nodes(0)
		subNode should be (BlockNode("test",Some(Template(List(TextNode(" template "))))))
	}

	it should  "parse block without content" in {
		val returnValue = parser.evaluate("{% block test %}{% endblock %}");
		returnValue.nodes.length should equal (1);
		val subNode = returnValue.nodes(0)
		subNode should be (BlockNode("test",None))
	}

	it should  "parse block with strange endblock" in {
		var returnValue = parser.evaluate("{% block test %} template {% endblock %}");
		returnValue.nodes.length should equal (1);
		var subNode = returnValue.nodes(0)
		subNode should be (BlockNode("test",Some(Template(List(TextNode(" template "))))))

		returnValue = parser.evaluate("{% block test %}{%endblock %}");
		returnValue.nodes.length should equal (1);
		subNode = returnValue.nodes(0)
		subNode should be (BlockNode("test",None))
	}

	it should  "not parse block without name" in {
		evaluating {
			val returnValue = parser.evaluate("{% block %} template {% endblock %}");
		} should produce [ParsingException]
		evaluating {
			val returnValue = parser.evaluate("{% block %}{% endblock %}");
		} should produce [ParsingException]



	}

	it should  "not parse for node with variable access include tag" in {
			val returnValue = parser.evaluate("{% for c in a.item %}{% endfor %}");
			returnValue should be (Template(List(ForNode("c", SimpleAttributeAccess("a", "item"), None))))
	}

	it should "not parse unended variable" in {
		evaluating {
			val returnValue = parser.evaluate("{{ test ");
		} should produce [ParsingException]

	}

	it should "parse function call" in {
		val returnValue = parser.evaluate ("{% fct \"test\" %}")
		returnValue should be (Template(List(ExtensionNode("fct",List("test"),None))))
	}

	it should "parse function call with attribute access" in {
		val returnValue = parser.evaluate("{% fct a.b %}")
		returnValue should be (Template(List(ExtensionNode("fct",List(SimpleAttributeAccess("a","b")),None))))
	}

}