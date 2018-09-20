package model

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
