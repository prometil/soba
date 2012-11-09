package soba.parser

import soba.exception._
import soba.expression._
import soba.node._
import collection.mutable.Map
import scala.util.parsing.combinator._
import scala.util.matching.Regex

trait StringExpressionParser extends JavaTokenParsers{
	
  	/**
  	* The string expression parser.
  	*/
	lazy val stringExpr:Parser[Expr[String]] =  string
	/**
  	* The ident expression  parser.
  	*/
 	//lazy val stringIdt:Parser[StringExpression] = ident ^^ { x => StringVariable(x)}

 	/**
 	* The simple quoted or double quoted expression
 	*/
 	lazy val string:Parser[StringValue] = "\"" ~> rawText <~ "\"" ^^ { x => StringValue(x.toString())}


 	def rawText:Parser[String] = new Parser[String] {
      def apply(in: Input) = {
      		val result = new StringBuilder()
          	val source = in.source
          	val offset = in.offset
          	var j = offset
          	var found = false
          	var previous = 'a'
          	while (j < source.length &&  !found ) {
	          	var current = source.charAt(j)
	          	if( previous != '\\'){
	          		if(current == '"'){
	          			found = true;
	          		}
	          	}
	          	j +=1
	          	if(current != '\\' && !found){
	          		result.append(current)
	          	}
	          	previous = current
	          	
          	}
          	if (j != offset){
           	 	Success(result.toString, in.drop(j - offset - 1))
          	}
         	else  {
            	val found = if (offset == source.length()) "end of source" else "`"+source.charAt(offset)+"'"
          	 	Failure("` { ' expected but "+found+" found", in.drop(offset - offset))
          	}
      }
  }

 	/**
	* Evaluate the string expression
	*
	* @param expression the string expression to evaluate
	* @param the evaluated boolean expression 
	*/
	def evaluateStringExpression(expression:String):Expr[String] = {

		var result = parseAll(stringExpr, expression)
	    result match {
	      case Success(x, _) => result.get
	      case Failure(err, next) => throw ParsingException("failed to parse input " +"(line " + next.pos.line + ", column " + next.pos.column + "):\n" + err + "\n" + next.pos.longString);
	    } 
	}
}