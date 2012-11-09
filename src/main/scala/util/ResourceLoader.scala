package soba.util

import java.io.File
import java.net.URL
import java.net.URISyntaxException
import java.io.FileInputStream
import java.io.InputStream
/**
*
*/
object ResourceLoader {
	/**
	* Load a resources from a uri
	*
	* Look for the file in the classpath and on the disk
	*
	* @param uri The uri of the file to load
	* @return None if Some the file has been found, else None
	* @throws ResourceNotFoundException if the file has been found but cannot be read
	*/
	def load(uri:String):Option[InputStream] = {
		if(uri != null){
			val file = new File(uri)
			if (file != null && file.exists && file.isFile) {
        		if (!file.canRead) {
        			throw new ResourceNotFoundException(uri)
        		}
        		return Some(new FileInputStream(file))
			}
			var url = Thread.currentThread.getContextClassLoader.getResourceAsStream(uri)
      if (url == null) {
      	url = getClass.getClassLoader.getResourceAsStream(uri)
    	}
      if (url != null) {
        return Some(url)
    	}
  	}
   	None
	}
}

/**
* Generate when a resource cannot be found.
*/
class ResourceNotFoundException(resource:String) extends Exception("Cannot find resource "+resource)