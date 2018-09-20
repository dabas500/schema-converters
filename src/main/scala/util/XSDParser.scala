package util

import scala.xml.{Node, XML}
import model.SchemaAttributes

object XSDParser {

  def getParsedSchema(xsdPath: String) = {
    val myXML = XML.loadFile(xsdPath)
    val schema = myXML \\ "schema"
    schema.flatMap(parent =>
                            parent.child
                              .flatMap(child => loop(child,parent,"complexType")))
      .filter(x => x.schemaname!=null)
  }

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

}
