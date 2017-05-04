// See LICENSE.SiFive for license details.

package zscalechip

import Chisel._
import zscale._
import config._
import junctions._
import diplomacy._
import coreplex._
import jtag.JTAGIO
import rocketchip.{IncludeJtagDTM, JtagDTMConfig, JtagDTMKey, JtagDTMKeyDefault, DebugTransportModuleJTAG}
import uncore.devices.ClockedDMIIO

// INFO: Just imitated the instantiation because IDK about scala/chisel
class Debug(implicit p: Parameters) extends BaseTop
  with PeripheryDebug {
  override lazy val module = new DebugModule(this, () => new DebugBundle(this))
}
class DebugBundle[+L <: Debug](_outer: L) extends BaseTopBundle(_outer)
  with PeripheryDebugBundle
class DebugModule[+L <: Debug, +B <: DebugBundle[L]](_outer: L, _io: () => B) extends BaseTopModule(_outer, _io)
    with PeripheryDebugModule

class ExampleTopZscaleBundle(implicit p: Parameters) extends Bundle {
  val mem = new MemIO
  // INFO: I'm the bad cop, this is I do not want DM if there is no config about this
  val dbgio = new Bundle {
      val debug = if(!p(IncludeJtagDTM) && p(useDM)) Some(new ClockedDMIIO().flip) else None

      val jtag        = if(p(IncludeJtagDTM) && p(useDM)) Some(new JTAGIO(hasTRSTn = false).flip) else None
      val jtag_reset  = if(p(IncludeJtagDTM) && p(useDM)) Some(Bool(INPUT)) else None
      val jtag_mfr_id = if(p(IncludeJtagDTM) && p(useDM)) Some(UInt(INPUT, 11)) else None

      val ndreset = if(p(useDM)) Bool(OUTPUT) else None
      val dmactive = if(p(useDM)) Bool(OUTPUT) else None
    }
  val trap = Bool(OUTPUT)
}

class ExampleTopZscale()(implicit p: Parameters) extends Module {
  val io = new ExampleTopZscaleBundle

  // Zscale core module
  val dut = Module(new Zscale)
  dut.reset := reset
  
  io.mem.imem <> dut.io.mem.imem
  io.mem.dmem <> dut.io.mem.dmem
  
  // Debug module
  // NOTE: This module implements DMI/JTAG automatically
  if(p(useDM)) 
  {
    val debug = Module(LazyModule(new Debug).module)
    io.dbgio <> debug.io
  }
  
  // PLIC
  // TODO: A PLIC please but with AHB
  
  // Misc
  io.trap := Bool(false)
}

