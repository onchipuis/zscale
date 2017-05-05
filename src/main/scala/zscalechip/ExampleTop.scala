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

class ExampleTopZscaleBundle(implicit p: Parameters) extends Bundle {

  // Mandatory Ports
  val mem = new MemIO
  val trap = Bool(OUTPUT)
  
  // INFO: This bundle holds all optional ports
  val opt_port = new Bundle {
    val dbgio = if(p(useDM)) Some[PeripheryDebugBundle](new PeripheryDebugBundle) else None
  }
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
    val debug = Module(new PeripheryDebugModule)
    io.opt_port.dbgio.foreach { dbgio => dbgio <> debug.io.dbgio }
  }
  
  // PLIC
  // TODO: A PLIC please but with AHB
  
  // Misc
  io.trap := Bool(false)
}

