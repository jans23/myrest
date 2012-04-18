package code
package lib

import net.liftweb._
import http.rest.{RestHelper, JsonSelect, XmlSelect}
import json._
import common.{Box,Full, Empty}
import scala.xml._

object IssuerREST extends RestHelper {
  
    trait Container {
	  
	    /**
	     * Convert the object to XML
	     */
	    def toXml(): Node = <xml>{Xml.toXml(this.toJson)}</xml>
	    
		/**
	    * Convert the object to JSON format.
	    */
		implicit val formats = net.liftweb.json.DefaultFormats
		def toJson(): JValue = Extraction.decompose(this)	
	}
    
    case class CDD (standard_Id: String, signature: String) extends Container
	private val cdd1: CDD = CDD("ABCD", "fr56t7z8u9ih")	
	def getCdd: Box[CDD] = Full(cdd1)

	/**
	 * Here's how we convert from an Item
	 * to JSON or XML depending on the request's
	 * Accepts header
	 */
	implicit def convert: JxCvtPF[Container] = {
		case (JsonSelect, c, _) => c.toJson
		case (XmlSelect, c, _) => c.toXml
	}

	serveJx[CDD] {
		"api" / "test" prefixJx {
		    case Get("cdd" :: Nil, _) => getCdd
		}
	}
}
