package soba.parser

import soba.exception._
import soba.expression._
import soba.node._
import scala.util.parsing.combinator._;
/**
* A boolean arithExpression parser
*/
trait BooleanExpressionParser  extends JavaTokenParsers with ArithmeticExpressionParser{
  /**
  * A boolean identifier converted to an {@link BooleanVariable}.
  */
  protected lazy val booleanVariable:Parser[BooleanVariable] = ident ^^ { x => BooleanVariable(x)}

  /**
  * A boolean attribute access (a.b) converted to an {@link BooleanAttributeAccess}.
  */
  protected lazy val booleanAttributeAccess:Parser[BooleanAttributeAccess] = ident ~"."~ ident ^^ {case i1 ~"."~ i2 => BooleanAttributeAccess(i1, i2)}

  /**
  * A boolean array access (a[1]) converted to an {@link BooleanArrayAccess}.
  */
  protected lazy val booleanArrayAccess:Parser[BooleanArrayAccess] = ident ~ "[" ~ wholeNumber ~"]" ^^ { case i ~ "[" ~ index ~ "]" => BooleanArrayAccess(i, index.toInt) }
  
  /**
  * A boolean literal containing true or false converted to an {@link BooleanValue}.
  */
  protected lazy val booleanLiteral:Parser[BooleanValue] = ("true" | "false") ^^ { x => BooleanValue(x.toBoolean)}
  /**
  * a boolean arithExpression converted to an {@link BooleanOperator}.
  */
  protected lazy val boolExpr:PackratParser[Expr[Boolean]] =  or | booleanTerm
  /**
  * A or arithExpression converted to an {@link BooleanOperator}.
  */
  private lazy val or: PackratParser[Or] = (boolExpr <~ "or") ~ booleanTerm ^^ { case lhs~rhs => Or(lhs,rhs) }
  /**
  * A boolean term (and, not) converted to an {@link BooleanExpression}.
  */
  private lazy val booleanTerm: PackratParser[Expr[Boolean]] =  not | and | booleanFactor 

  /**
  * A boolean and converted to an {@link BooleanOperator}.
  */
  private lazy val and:PackratParser[And] = (booleanTerm <~ "and") ~ booleanFactor ^^ { case lhs~rhs => And(lhs, rhs) }

  /**
  * A boolean not converted to an {@link NotOperator}.
  */
  private lazy val not:PackratParser[Not] = "not" ~> booleanTerm ^^ { case rhs => Not(rhs) }

  /** 
  * The greater or equal arithExpression that compare two artihmetic arithExpression converted to an {@link BinaryOperator}.
  */
  private lazy val gte:PackratParser[Gte] = (arithExpr ~ ">=" ~ arithExpr) ^^ { case lhs ~ ">=" ~ rhs => Gte(lhs, rhs) }

  /**
  * The greater or equal arithExpression that compare two artihmetic arithExpression converted to an {@link BinaryOperator}.
  */  
  private lazy val gt:PackratParser[Gt] = arithExpr ~ ">" ~ arithExpr ^^ { case lhs ~ ">" ~ rhs => Gt(lhs, rhs) }

  /**
  * The lesser or equal arithExpression that compare two artihmetic arithExpression converted to an {@link BinaryOperator}.
  */  
  private lazy val lte:PackratParser[Lte] = arithExpr ~ "<=" ~ arithExpr ^^ { case lhs ~ "<=" ~ rhs => Lte(lhs, rhs) }
 
  /**
  * The lesser arithExpression converted to an {@link BinaryOperator}.
  */ 
  private lazy val lt:PackratParser[Lt] = arithExpr ~ "<" ~ arithExpr ^^ { case lhs ~ "<" ~ rhs => Lt(lhs, rhs) }
 
  /**
  * The not equal arithExpression converted to an {@link BinaryOperator}.
  */ 
  private lazy val ne:PackratParser[Ne] = arithExpr ~ "!=" ~ arithExpr ^^ { case lhs ~ "!=" ~ rhs => Ne(lhs, rhs) }

  /**
  * The equal arithExpression converted to an {@link BinaryOperator}.
  */
  private lazy val eq:PackratParser[Eq] = arithExpr ~ "==" ~ arithExpr ^^ { case lhs ~ "==" ~ rhs => Eq(lhs, rhs) }

//  private lazy val eqBoolean:PackratParser[EqBoolean] = boolExpr ~ "==" ~ boolExpr ^^ { case lhs ~ "==" ~ rhs => EqBoolean(lhs, rhs) }

  /**
  * Boolean factor (boolexpr) converted to an {@link Expr}.
  */
  private lazy val booleanFactor:PackratParser[Expr[Boolean]] =   "(" ~> boolExpr <~ ")" |  shortBooleanFactor ^^ {x => x } 

  /**
  * The short boolean factor converted to an {@link Expr}.
  */
  private def shortBooleanFactor:Parser[Expr[Boolean]] = ( gte | gt | lte | lt  | ne | eq | booleanArrayAccess | booleanAttributeAccess | booleanLiteral | booleanVariable |    "(" ~> boolExpr <~ ")" ) ^^ { v => v }


  /**
  * Evaluate the boolean expression
  *
  * @param arithExpression the string arithExpression to evaluate
  * @param the evaluated boolean arithExpression 
  */
  def evaluateBooleanExpression(expression:String):Expr[Boolean] = {

    var result = parseAll(boolExpr, expression)
    result match {
      case Success(x, _) => result.get
      case Failure(err, next) => throw ParsingException("failed to parse input " +"(line " + next.pos.line + ", column " + next.pos.column + "):\n" + err + "\n" + next.pos.longString);
    } 
  }
}