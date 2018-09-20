package object util {


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
}
