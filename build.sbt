organization := "edu.berkeley.cs"

version := "1.0"

name := "zscale"

scalaVersion := "2.11.6"

val chipSettings = packAutoSettings ++ Seq(
  /*addons := {
    val a = sys.env.getOrElse("ROCKETCHIP_ADDONS", "")
    println(s"Using addons: $a")
    a.split(" ")
  },*/
  unmanagedSourceDirectories in Compile += baseDirectory.value / "rocket-chip/src/main/scala",
  unmanagedSourceDirectories in Compile += baseDirectory.value / "rocket-chip/src/main/scala/junctions",
  scalaSource in Compile := baseDirectory.value / "src",
  mainClass in (Compile, run) := Some("rocketchip.Generator")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
//libraryDependencies ++= (Seq("chisel3", "junctions", "uncore", "rocket").map {
//  dep: String => sys.props.get(dep + "Version") map { "edu.berkeley.cs" %% dep % _ }}).flatten
  
//unmanagedSourceDirectories in Compile += baseDirectory.value / "rocket-chip/src"

//lazy val root = (project in file("."))
  //.aggregate(rocket)

lazy val chisel = (project in file("rocket-chip/chisel3"))
//lazy val hardfloat = (project in file("rocket-chip/hardfloat")).dependsOn(chisel)
//lazy val rocket = (project in file("rocket-chip")).settings(chipSettings).dependsOn(chisel, hardfloat)
lazy val root = project.settings(chipSettings).dependsOn(chisel)
