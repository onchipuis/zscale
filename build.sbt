// Provide a managed dependency on X if -DXVersion="" is supplied on the command line ?

val zscaleSettings = packAutoSettings ++ Seq(
  organization := "edu.berkeley.cs",
  version := "1.0",
  name := "zscale",
  scalaVersion := "2.11.6",
  scalacOptions += "-feature -deprecation"
  scalaSource in Compile := baseDirectory.value / "src",
  unmanagedSourceDirectories in Compile += baseDirectory.value / "rocket-chip/src",
  unmanagedSourceDirectories in Compile += baseDirectory.value / "rocket-chip/hardfloat/src"
)

lazy val chisel = (project in file("rocket-chip/chisel3"))
//lazy val hardfloat = (project in file("rocket-chip/hardfloat")).dependsOn(chisel)
//lazy val rocketchip = (project in file("rocket-chip"))
lazy val root = (project in file(".")).settings(zscaleSettings).dependsOn(chisel)
