package soba.context

import soba.exception._
import scala.collection.mutable.Map
import scala.collection.immutable.Stack
import scala.collection.JavaConversions._


/**
* The context companion object
*/
object Context{
	/**
	* Create a new context from an empty map.
	* 
	* @return a new context with no data in it.
	*/
	def apply() = new Context()

	/**
	* Creates a new context from a map.
	* 
	* @param map the map used to create the new context.
	* @return a new context.
	*/
	def apply(map:Map[String,Any])= new Context(map)


	/**
	* Creates a new context from a java map.
	* 
	* @param map the map used to create the new context.
	* @return a new context.
	*/
	def apply(map:java.util.Map[String,Any]) = new Context(map)

	/**
	* Create a new context from a set of values.
	*
	* @param values the list of tuples to construct the context with.
	* @return a new context containing all the value.
	**/
	def apply( values:(String, Any))= new Context(Map(values))
}


/**
* The soba engine rendering context
* @param map the context data
*/
class Context (map:Map[String,Any] = Map()){

	//the stack of scopes
	private var scopes = Stack[Map[String,Any]](map);


	/**
	* Stacks a new context.
	* @return the context
	**/
	def stack():Context = {
		scopes = scopes.push(Map[String,Any]())
		this
	}
	/**
	* Removes the current context
	* @return the context
	**/
	def pop():Context={
		scopes.pop
		this
	}
	/**
	* Set a value for a given key
	*
	* @param key The name of the variable to put.
	* @param value The value to put in the context
	* @return The context itself.
	*/
	def put(key:String, value:Any):Context = {
		scopes.head.put(key, value)
		this
	}
	/**
	* Executes a function in a new scope
	* 
	* @param f The function to execute.
	*/
	def scope(f: =>Any){
		stack()
		f
		pop()
	}

	/**
	* Returns an attribute of a given variable.
	* @param name The name of the variable.
	* @param attribute the attribute name to access. If the object does not contain with this name, of a an attribute with this name a NoSuchFieldException is launched
	* @return the variable field value.
	*
	* @throws VariableNotFoundException if the variable cannot be found.
	* @throws java.lang.NoSuchFieldException if the method cannot be found.
	*/
	def getAttributeVariable(name:String, attribute:String):Any = {

		var x = get(name) 
		var field = x.getClass.getDeclaredField(attribute)
		
		//this is bad, bad, bad...
		field.setAccessible(true);
		if (field.isAccessible){
			field.get(x);
		}
		else{
			var method = x.getClass.getDeclaredMethod(attribute)
			method.invoke(x)
		}
	}
	/**
	* Returns a variable value.
	*
	* If the variable is not at in the current scope it looks upward in the parents scope.
	* 
	* @param name of the variable to get
	* @return the variable value
	* @throws VariableNotFoundException if the variable cannot be found
	*/
	def get(name:String):Any = {
			scopes.find(x => {x.contains(name)})  match{
					case Some(x) =>x.get(name).get
					case None => throw new VariableNotFoundException("Cannot find object "+name)
			}
	}

	/**
	* Returns a variable value, and if the variable is not found return the default value
	* @param name the variable value
	*	@param value the default value
	*/
	def getOrElse(name:String, value:Any) = {
		scopes.find(x => {x.contains(name)})  match{
					case Some(x) =>x.get(name).get
					case None => value
			}
	}

}