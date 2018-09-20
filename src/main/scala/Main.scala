import util.{PrintProtoSchema, XSDParser}

object Main {

   def main(args:Array[String]) = {
     if (args.length <2) {
       println("call with: Main pathoofxsd protodir")
       System.exit(0)
     }
      val xsdpath = args(0)
      val protoDir = args(1)
      val schemaAttributes = XSDParser.getParsedSchema(xsdpath)
      PrintProtoSchema.generateProto(schemaAttributes, protoDir)
  }
}

