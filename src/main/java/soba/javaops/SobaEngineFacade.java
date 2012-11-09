package soba.javaops;

import scala.collection.JavaConversions;
import soba.context.Context;
import soba.extension.Extension;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Map;
/**
* This class is a wrapper around the real soba engine in scala to easy to integration with java.
*
*/
public class SobaEngineFacade{
	
	//the soba engine
	private soba.SobaEngine engine; 

	/**
	* Constructs a soba engine that wrap the scala sobaengine.
	*/
	public SobaEngineFacade(){
		engine = new soba.SobaEngine();
	}


	public void registerExtension(Extension extension){
		engine.registerExtension(extension);
	}

	/**
	* Render a file path as a string using a context
	* 
	* @param file The file to render.
	* @param bindings a list of bindings to use.
	* @return The template rendered as a string.
	*/
   	public String render (String file, Map<String,Object>  bindings){
   		Writer writer = new StringWriter();
   		render(file , writer, bindings);
   		return writer.toString();
   	}
   	/**
	* Render a file path to a writer using a context.
	* 
	* @param file The file to render.
	* @param stream The writer to which the node will be rendered.
	* @param bindings a list of bindings to use.
	*/
   	public void render (String file, Writer stream, Map<String,Object>  bindings){
   	 	Context context = new Context(JavaConversions.mapAsScalaMap(bindings));
   		engine.render(file ,stream, context);
   	}
}