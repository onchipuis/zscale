// See LICENSE for license details.

package zscale

/** A Generator for platforms containing zscale stuff */
object Generator extends util.GeneratorApp {

  /*override def addTestSuites {
    // TODO: Fill me with something
  }*/

  val longName = names.topModuleProject + "." + names.configs
  generateFirrtl
  generateTestSuiteMakefrags
  generateArtefacts
}
