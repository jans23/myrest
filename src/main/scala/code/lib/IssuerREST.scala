package code
package lib

import net.liftweb._
import http.rest.{RestHelper, JsonSelect, XmlSelect}
import json._
import common.{Box,Full, Empty}
import scala.xml._
import util.Helpers

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
    
    case class Receipt (value: Boolean) extends Container
    case class Car (id: String, name: String, power: Int) extends Container {
	      	
	  /**
	   * Convert a JValue to an Car if possible
	   */
	  def apply(in: JValue): Box[Car] = Helpers.tryo{in.extract[Car]}
	
	  /**
	   * Extract a JValue to an Car
	   */
	  def unapply(in: JValue): Option[Car] = apply(in)
	
	  /**
	   * The default unapply method for the case class.
	   * We needed to replicate it here because we
	   * have overloaded unapply methods
	   */
	/*  def unapply(in: Any): Option[(String, String, Int)] = {
        in match {
	      case i: Car => Some((i.id, i.name, i.power))
	      case _ => None
	    }
	  } */
    }
    
	def add(c: Car): Box[Receipt] = {
	  //For this example, receipts are always positive
	  Full(Receipt(true))
	}

	/**
	 * Here's how we convert from an Item
	 * to JSON or XML depending on the request's
	 * Accepts header
	 */
	implicit def convert: JxCvtPF[Container] = {
		case (JsonSelect, c, _) => c.toJson
		case (XmlSelect, c, _) => c.toXml
	}

	serveJx[Container] {
		// The POST request should match the Car class. Several Cars can be submitted.
		// All submitted cars should be added. A receipt will be replied.
	    case Post("submit" :: Nil, request) =>
	    	Car(mergeJson(request, json)).map(add(_): JValue)
	}
}
