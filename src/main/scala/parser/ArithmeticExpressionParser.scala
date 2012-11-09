package soba.parser


import soba.expression._
import soba.node._
import soba.exception._
import scala.util.parsing.combinator._;

/**
* An arithmetic expression parser
*/
trait ArithmeticExpressionParser extends JavaTokenParsers with PackratParsers{

  /**
  * A boolean identifier converted to an {@link BooleanVariable}.
  */
  protected lazy val arithVariable:PackratParser[ArithVariable] = ident ^^ { x => ArithVariable(x)}

  /**
  * A boolean attribute access (a.b) converted to an {@link BooleanAttributeAccess}.
  */
  protected lazy val arithAttributeAccess:PackratParser[ArithAttributeAccess] = ident ~"."~ ident ^^ {case i1 ~"."~ i2 => ArithAttributeAccess(i1, i2)}

  /**
  * A boolean array access (a[1]) converted to an {@link BooleanArrayAccess}.
  */
  protected lazy val arithArrayAccess:PackratParser[ArithArrayAccess] = ident ~ "[" ~ wholeNumber ~"]" ^^ { case i ~ "[" ~ index ~ "]" => ArithArrayAccess(i, index.toInt) }
  
  /**
  * A number literal converted to an {@link ArithValue}.
  */
  lazy val numberLiteral:Parser[ArithValue] = wholeNumber ^^ { x => ArithValue(x.toInt)}
  /**
  * The float literal converted to an {@link ArithValue}.
  */  
  lazy val floatLiteral:Parser[ArithValue] = floatingPointNumber ^^ { x => ArithValue(x.toDouble)}
  /**
  * The expression (minus, plus or term) parser.
  */
 	lazy val arithExpr:PackratParser[Expr[Double]] =  plus |  minus | term 
  /**
  * The plus parser converted to an {@link BinaryOperator}.
  */  
  lazy val plus: PackratParser[Plus] = (arithExpr <~ "+") ~ term ^^ { case lhs~rhs => Plus(lhs,rhs) }
  /**
  * The minus parser converted to an {@link BinaryOperator}.
  */
  lazy val minus: PackratParser[Minus] = (arithExpr <~ "-") ~ term ^^ { case lhs~rhs => Minus(lhs, rhs) } 
  /**
  * The term parser converted to an {@link BinaryOperator}.
  */
  lazy val term: PackratParser[Expr[Double]] =  mult | div | factor 
  /**
  * The mult parser converted to an {@link BinaryOperator}.
  */
  lazy val mult:PackratParser[Mult] = (term <~ "*") ~ factor ^^ { case lhs~rhs => Mult(lhs, rhs) }
  /**
  * The div parser converted to an {@link BinaryOperator}.
  */
  lazy val div:PackratParser[Div] =  (term <~ "/") ~ factor ^^ { case lhs~rhs => Div(lhs, rhs) } 

  /**
  * The factor parser converted to an {@link ArithExpression}.
  */
  lazy val factor : PackratParser[Expr[Double]] =   "(" ~> arithExpr <~ ")" |  shortFactor ^^ {x => x } 

  /**
  * Parse a sign followed by another expression
  */
  lazy val sign:Parser[Option[String]] = opt("+" | "-") ^^ { x => x }


  /**
  * A shortfactor
  */
  private def shortFactor: Parser[Expr[Double]] = sign ~ (arithArrayAccess | arithAttributeAccess | arithVariable |  floatLiteral |   numberLiteral | "(" ~> arithExpr <~ ")" ) ^^ { 
    case Some("+") ~ v => v
    case Some("-") ~ v => MinusSign(v)
    case None ~ v => v
  }


  /**
  * Parsee an arithmetic Expression 
  * 
  * @param formula the string to parse.
  */
  def evaluateArithmeticExpression(formula: String) = { 
    var result = parseAll(arithExpr,formula)
    result match {
      case Success(x, _) => result.get
      case Failure(err, next) =>throw ParsingException("failed to parse input " +"(line " + next.pos.line + ", column " + next.pos.column + "):\n" + err + "\n" + next.pos.longString);
    } 
  }


 
}