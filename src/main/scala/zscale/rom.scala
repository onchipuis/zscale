package zscale

import Chisel._
import Chisel.ImplicitConversions._
import config._
import junctions._
import HastiConstants._

class HastiROM(contents: Seq[Byte])(implicit p: Parameters) extends HastiModule()(p) {
  val io = new HastiSlaveIO

  val rows = (contents.size + 3) / 4
  val rom = Vec.tabulate(rows) { i =>
    val slice = contents.slice(i*4, (i+1)*4)
    UInt(slice.foldRight(BigInt(0)) { case (x,y) => (y << 8) + (x.toInt & 0xFF) }, 32)
  }

  val raddr = io.haddr >> UInt(2)
  val ren = io.hsel && (io.htrans === HTRANS_NONSEQ || io.htrans === HTRANS_SEQ)
  val rdata = Reg(UInt(width = 32))
  
  rdata := rom(raddr).asUInt()

  io.hrdata := rdata
  io.hready := Bool(true)
  io.hresp := HRESP_OKAY
}
