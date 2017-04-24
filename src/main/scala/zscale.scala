// See LICENSE for license details.

package zscale

import Chisel._
import Chisel.ImplicitConversions._
import config._
import junctions._
import HastiConstants._
import util.ParameterizedBundle // IMPORTANT: Before uncore.util
import uncore._
import rocket._

// TODO: For XLen and CoreParams (check)
import tile._

trait HasZscaleParameters {
  implicit val p: Parameters
  val xLen = 32 //p(XLen) // TODO: Forced to this value. Do now know why they extract it from Parameters class
  val coreInstBits = /*if (useCompressed) 16 else */32
  val fastMulDiv = false // p(FastMulDiv)

  // these should become parameters, rather than constants
  val haveMExt = true
  val haveEExt = false
}

abstract class ZscaleModule(implicit val p: Parameters) extends Module
  with HasZscaleParameters
abstract class ZscaleBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasZscaleParameters

class Zscale(resetSignal: Bool = null)(implicit val p: Parameters) extends Module(_reset = resetSignal)
    with HasZscaleParameters {
  val io = new Bundle {
    val imem = new HastiMasterIO
    val dmem = new HastiMasterIO
    //val prci = new PRCITileIO().flip
  }

  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)

  io.imem <> ctrl.io.imem
  io.imem <> dpath.io.imem
  io.dmem <> ctrl.io.dmem
  io.dmem <> dpath.io.dmem
  ctrl.io.dpath <> dpath.io.ctrl

  //ctrl.io.prci <> io.prci
  //dpath.io.prci <> io.prci
}
