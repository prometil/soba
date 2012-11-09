package soba.exception

/**
* An exception thrown when a error on parsing occured.
* @param the text of the exception
*/
case class ParsingException(text:String) extends Exception(text)

/**
* An exception thrown when a variable cannot be found in warning.
* @param the text of the exception
*/
case class VariableNotFoundException(text:String) extends Exception(text)

/**
* An Exception thrown chen a variable is of the suitable type.
* @param the text of the exception
*/
case class IllegalTypeException (text:String) extends Exception(text)


case class UnfoundExtensionException(text:String) extends Exception(text)
