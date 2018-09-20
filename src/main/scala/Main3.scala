
import java.io.{File, PrintWriter}

import scala.xml._
import scala.io.Source
import java.nio.file.Paths


object Test2 extends App{

  val myXML = XML.loadFile("/Users/naveen/Desktop/LOtte.xsd")
  val schema = myXML \\ "schema"

val x = schema.flatMap(parent =>parent.child.flatMap(child => loop(child,parent,"complexType"))).filter(x => x.schemaname!=null)

  x.filter(_.maxOccurs=="unbounded").distinct.foreach(x => println(x.schemaname,x.atttributename))


  def loop(node:Node,parentNode: Node, parentType:String):Seq[SchemaAttributes] = node.label match{
    case "#PCDATA" => Seq.empty
    case "import" => Seq.empty
    case "annotation" => Seq.empty
    case "complexType" => if (node.attribute("name").isDefined) node.child.flatMap(child => loop(child, node, "complexType")) else node.child.flatMap(child => loop(child, parentNode, "complexType"))
    case "element" if(node.child.map(_.label).contains("simpleType")) => Seq(getsimple(node,parentNode, "element"))
    case "element" if(node.child.map(_.label).contains("complexType")) => node.child.flatMap(child => loop(child, node, "element")) ++ Seq(getelement(node,parentNode, "element"))
    case "sequence" => node.child.flatMap(child => loop(child, parentNode, parentType))
    case "choice" => node.child.map(child => getchoice(child,parentNode,"choice"))
    case "element" => Seq(getelement(node, parentNode, "element"));
    case "attribute" => Seq(getattribute(node,parentNode, "attribute"))
    case "simpleType" => Seq(getsimple(node,parentNode, "simpleType"))
  }

  case class SchemaAttributes(schemaname:String, schematype:String, atttributename:String, attributetype:String, attributelength:String, minOccurs:String, maxOccurs:String, default:String,text:String){

    override def toString: String = schemaname +","+
      schematype +","+
      atttributename +","+
      attributetype +","+
      attributelength +","+
      minOccurs +","+
      maxOccurs +","+
      default +","+
      text
  }
  def checkColon(value:String):String = {
    val typ = if(value.contains(":"))
      value.split(":")(1)
    else value
    typ.toLowerCase
  }

  def getsimple(node: Node, parent:Node, parenttype:String) = {
    val simpleType = node \ "simpleType" \ "restriction"
    val name = (node \ "@name").toString.toLowerCase.replaceAll("([0-9]).([0-9])","$1_$2")
    val typ = checkColon((simpleType\ "@base").toString)
    SchemaAttributes((parent \ "@name").toString.toLowerCase.replaceAll("([0-9]).([0-9])","$1_$2"),
      parenttype,
      name,
      typ,
      (simpleType \ "maxLength" \ "@value").toString,
      (node \ "@minOccurs").toString,
      (node \ "@maxOccurs").toString,
      (node \ "@default").toString,
      (node \ "annotation" \"documentation").text)
  }

  def getelement(node:Node, parent:Node, parenttype:String) = {
    val elementType = node

    if(elementType.child.map(_.label).contains("simpleType"))
      getsimple(node,parent, parenttype)
    else{
      val name = (elementType \ "@name").toString.toLowerCase.replaceAll("([0-9]).([0-9])","$1_$2")
      var typ = if((elementType \ "@type").toString.isEmpty) name else (elementType \ "@type").toString
      typ = checkColon(typ)
    SchemaAttributes((parent \ "@name").toString.toLowerCase.replaceAll("([0-9]).([0-9])","$1_$2"),
      parenttype,
      name,
      typ,
      "",
      (elementType \ "@minOccurs").toString,
      (elementType \ "@maxOccurs").toString,
      (elementType \ "@default").toString ,
      (node \ "annotation" \"documentation").text)
    }
  }

  def getchoice(node:Node, parent:Node, parenttype:String) = {
    var name  = node.attribute("ref").getOrElse("").toString.toLowerCase.replaceAll("([0-9]).([0-9])","$1_$2")
    name = checkColon(name)
    name  = if (name.isEmpty) name
    else  name+"_type"
    SchemaAttributes((parent \ "@name").toString.toLowerCase.replaceAll("([0-9]).([0-9])","$1_$2"),
      parenttype,
      name,
      name,"","","","","")
  }

  def getattribute(node:Node, parent:Node, parenttype:String) = {
    val name = (node \ "@name").toString.toLowerCase.replaceAll("([0-9]).([0-9])","$1_$2")
      val typ = checkColon((node \ "@type").toString)
    SchemaAttributes((parent \ "@name").toString.toLowerCase.replaceAll("([0-9]).([0-9])","$1_$2"),
      parenttype,
      name,
      typ,
      "",
      "","","","")
  }

  generateProto()

    def printImport(messageTypes: Set[String], values:Seq[SchemaAttributes]) = {
      values.map( value => if(messageTypes.contains(value.attributetype)) {
        val filename = value.attributetype
        """import """"+ filename+ """.proto";"""
      }
      else ""
      ).filter(!_.isEmpty)
    }

  def generateProto() = {
    val path = "/Users/naveen/Desktop/GIT/dsh/Parser/src/test/resources/proto/"
    val complexTypes = x.groupBy(_.schemaname).keySet.filter(!_.isEmpty)
    val choiceTypes = x.filter(_.schematype=="choice").map(_.schemaname).toSet

    x.groupBy(_.schemaname).filter(!_._1.isEmpty).foreach(x =>  {
      var i = 0
      val writer = new PrintWriter(new File(path + x._1+".proto"))
      writer.println("""syntax = "proto3";""")
      writer.println("""package com.kpn.dsh.iuc.messages.proto3;""")
      writer.println("""option java_package = "com.kpn.dsh.iuc.messages.proto3";""")
      writer.println(""" """)
      //x._2.foreach(a => println(a.schemaname, a.schematype,a.atttributename))
      printImport(complexTypes,x._2).distinct.foreach(writer.println)
      if(choiceTypes.contains(x._1))
        printOneOf(x._1,x._2,writer)
      else
      printMessage(x._1,x._2,writer)
      writer.close()
    } )
  }

  def printOneOf(messageName:String,attributes:Seq[SchemaAttributes], writer:PrintWriter) = {
    var i = 0
    writer.println(""" """)
    writer.println("message" +" "+ messageName +" { ")
    writer.println("oneof" +" "+ messageName +" { ")
    attributes.distinct.filter(!_.atttributename.isEmpty).foreach{ att =>
      i = i+1
      writer.println(PRINT(att, i))
    }
    writer.println("}")
    writer.println("}")
  }

  def printMessage(messageName:String,attributes:Seq[SchemaAttributes], writer:PrintWriter) = {
    var i = 0
    writer.println(""" """)
    writer.println("message" +" "+ messageName +" { ")

    attributes.distinct.filter(!_.atttributename.isEmpty).foreach{ att =>
      i = i+1
      writer.println(PRINT(att, i))
    }
    writer.println("}")
  }

  def protoType(xmlType:String) = {
    val typ = if (xmlType.contains(":")) xmlType.toLowerCase.split(":")(1) else xmlType
     typ match {
      case "boolean" => "bool"
      case "unsignedbyte" => "bytes"
      case "positiveinteger" | "integer" => "int32"
      case "datetime" | "nonemptystring" => "string"
      case "long" => "int64"
      case _ => typ
    }
  }
  def getrepeted(maxOccurs:String) = maxOccurs.toLowerCase.trim match{
    case "unbounded" => "repeated"
    case _ => ""
  }
  def PRINT(att:SchemaAttributes, index:Int): String = {
    val prototype = protoType(att.attributetype)
    val ret = getrepeted(att.maxOccurs) +" "+prototype +" " +
      att.atttributename + " = " +
      index + " ;"
    ret
  }
}

