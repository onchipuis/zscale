// See LICENSE for license details.

package zscale

import Chisel._
import Chisel.ImplicitConversions._
import config._
import junctions._
import HastiConstants._
import util.ParameterizedBundle // IMPORTANT: Before uncore.util
import uncore._

import tile.XLen 

trait HasZscaleParameters {
  implicit val p: Parameters
  val xLen = p(XLen)
  val coreInstBits = if (p(useCExt)) 16 else 32
  val fastMulDiv = p(FastMulDiv)

  // these should become parameters, rather than constants
  val haveMExt = p(useMExt)
  val haveEExt = p(useEExt)
  val haveCExt = p(useCExt)
}

abstract class ZscaleModule(implicit val p: Parameters) extends Module
  with HasZscaleParameters
abstract class ZscaleBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasZscaleParameters

class Zscale(resetSignal: Bool = null)(implicit val p: Parameters) extends Module(_reset = resetSignal)
    with HasZscaleParameters {
  val io = new Bundle {
    val mem = new MemIO
    //val prci = new PRCITileIO().flip
  }

  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)

  io.mem.imem <> ctrl.io.mem.imem
  io.mem.imem <> dpath.io.mem.imem
  io.mem.dmem <> ctrl.io.mem.dmem
  io.mem.dmem <> dpath.io.mem.dmem
  dpath.io.ctrl <> ctrl.io.dpath

  //ctrl.io.prci <> io.prci
  //dpath.io.prci <> io.prci
}
