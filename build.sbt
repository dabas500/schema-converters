name := "Parser"

version := "0.1"

scalaVersion := "2.12.6"
crossScalaVersions := Seq("2.11.12", "2.12.6")
val protobufVersion   = "3.4.0"


resolvers ++= Seq(
  "Apache Releases Repository" at "https://repository.apache.org/content/repositories/releases/",
  "Sonatype OSS Snapshots"     at "https://oss.sonatype.org/content/repositories/releases",
  "spray repo" at "http://repo.spray.io",
  Resolver.mavenLocal
)

enablePlugins(SbtTwirl)


libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.0"
libraryDependencies += "com.typesafe.play" %% "twirl-api" % "1.2.1"
libraryDependencies += "org.scalatra" %% "scalatra" % "2.6.3"

enablePlugins(ProtobufPlugin)

ProtobufConfig / version              := protobufVersion
ProtobufConfig / sourceDirectory      := baseDirectory.value / "src" / "test" / "resources" / "proto"
ProtobufConfig / protobufIncludePaths ++= Seq(baseDirectory.value / "src" / "test" / "resources" / "proto")
ProtobufConfig / javaSource           := target.value / "protobuf_generated"