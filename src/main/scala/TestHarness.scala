// See LICENSE.SiFive for license details.

package zscale

import Chisel._
import config._
import junctions._
import diplomacy._
import coreplex._

class ExampleTopZscale()(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val mem = new MemIO
    val success = Bool(OUTPUT)
  }

  val dut = Module(new Zscale)
  dut.reset := reset
  
  io.mem.imem <> dut.io.mem.imem
  io.mem.dmem <> dut.io.mem.dmem
  
  io.success := Bool(false)
}

