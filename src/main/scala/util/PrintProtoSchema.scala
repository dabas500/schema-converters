package util

import java.io.{File, PrintWriter}
import model.SchemaAttributes


object PrintProtoSchema {

  def getrepeted(maxOccurs:String) = maxOccurs.toLowerCase.trim match{
    case "unbounded" => "repeated"
    case _ => ""
  }

  def getAttributeDefinition(att:SchemaAttributes, index:Int): String = {
    val prototype = protoType(att.attributetype)
    val ret = getrepeted(att.maxOccurs) +" "+prototype +" " +
      att.atttributename + " = " +
      index + " ;"
    ret
  }

  def printImport(messageTypes: Set[String], values:Seq[SchemaAttributes]) = {
    values.map( value => if(messageTypes.contains(value.attributetype)) {
      val filename = value.attributetype
      """import """"+ filename+ """.proto";"""
    }
    else ""
    ).filter(!_.isEmpty)
  }

  def generateProto(schemaAttributes:Seq[SchemaAttributes], path: String) = {
    //val path = "/Users/naveen/Desktop/GIT/dsh/Parser/src/test/resources/proto/"

    val complexTypes = schemaAttributes.groupBy(_.schemaname).keySet.filter(!_.isEmpty)
    val choiceTypes = schemaAttributes.filter(_.schematype=="choice").map(_.schemaname).toSet

    schemaAttributes.groupBy(_.schemaname).filter(!_._1.isEmpty).foreach(x =>  {
      var i = 0
      val writer = new PrintWriter(new File(path + x._1+".proto"))
      writer.println("""syntax = "proto3";""")
      writer.println("""package com.kpn.dsh.iuc.messages.proto3;""")
      writer.println("""option java_package = "com.kpn.dsh.iuc.messages.proto3";""")
      writer.println(""" """)

      printImport(complexTypes,x._2).distinct.foreach(writer.println)
      if(choiceTypes.contains(x._1))
        printMessageWithOneOf(x._1,x._2,writer)
      else
        printMessage(x._1,x._2,writer)
      writer.close()
    } )
  }

  def printProtoHeader() = ???

  def printMessageWithOneOf(messageName:String,attributes:Seq[SchemaAttributes], writer:PrintWriter) = {
    var i = 0
    writer.println(""" """)
    writer.println("message" +" "+ messageName +" { ")
    writer.println("oneof" +" "+ messageName +" { ")
    attributes.distinct.filter(!_.atttributename.isEmpty).foreach{ att =>
      i = i+1
      writer.println(getAttributeDefinition(att, i))
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
      writer.println(getAttributeDefinition(att, i))
    }
    writer.println("}")
  }
}
