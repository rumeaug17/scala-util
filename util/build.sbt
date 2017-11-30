
name := "util"

organization := CommonBuild.PakageOrganizationName  

version := CommonBuild.PackageVersion

scalaVersion := CommonBuild.ScalaVersion

scalacOptions ++= CommonBuild.ScalaOptions

resolvers ++= CommonBuild.ScalaResolvers

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "junit" % "junit" % "4.12" % "test"
)

enablePlugins(JavaAppPackaging)

// after publish-local 
// libraryDependencies += "org.agdf" %% "util" % "1.0-SNAPSHOT"

scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value+"/root-doc.txt")

scalacOptions in (Compile, doc) ++= Seq("-doc-title", s"Util Module, version ${version}")
