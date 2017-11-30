import sbt._
import Keys._

// https://github.com/harrah/xsbt/wiki/Full-Configuration

//
/*
The version number you select must end with SNAPSHOT, or you must change the version number each time you publish. Ivy maintains a cache, and it stores even local projects in that cache. If Ivy already has a version cached, it will not check the local repository for updates, unless the version number matches a changing pattern, and SNAPSHOT is one such pattern.
*/

object CommonBuild {
  val PackageRootName = "util"
  val PakageOrganizationName  = "org.agdf"
  val PackageVersion = "1.0-SNAPSHOT"

  val ScalaVersion = "2.12.1"
  val ScalaOptions = Seq("-deprecation", "-feature")
  val ScalaResolvers = Seq (
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "SonaType" at "https://oss.sonatype.org/content/groups/public"
  )
}

