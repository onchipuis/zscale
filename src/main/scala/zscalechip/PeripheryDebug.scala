package zscalechip

import Chisel._
import zscale._
import rocketchip.{IncludeJtagDTM, JtagDTMConfig, JtagDTMKey, JtagDTMKeyDefault, DebugTransportModuleJTAG}
import config._
import diplomacy._
import uncore.devices._
import zscaleuncore.devices._
import util._
import junctions._
import jtag.JTAGIO
import tile.XLen

//import coreplex._

// System with DMI or JTAG interface based on a parameter

class DebugComBundle(implicit p: Parameters) extends Bundle {
  val debug = if(!p(IncludeJtagDTM)) Some(new ClockedDMIIO().flip) else None

  val jtag        = if(p(IncludeJtagDTM)) Some(new JTAGIO(hasTRSTn = false).flip) else None
  val jtag_reset  = if(p(IncludeJtagDTM)) Some(Bool(INPUT)) else None
  val jtag_mfr_id = if(p(IncludeJtagDTM)) Some(UInt(INPUT, 11)) else None

  val ndreset = Bool(OUTPUT)
  val dmactive = Bool(OUTPUT)
}

class PeripheryDebugBundle(implicit p: Parameters) extends Bundle {
  // frontend debug IO
  val dbgio = new DebugComBundle
  
  // TODO: Add the core native debug interface
  val core = new Bundle
  {
    val ndreset = Bool(INPUT)
    val dmactive = Bool(INPUT)
  }
  
  // Memory interface for debug module
  val mem = if(p(IncludeDMMem)) Some(new ZscaleDebugMemBundle) else None
}

class PeripheryDebugModule(implicit p: Parameters) extends Module {
  val io = new PeripheryDebugBundle
  
  val zscale_debug = Module(LazyModule(new ZscaleDebugModule).module)

  // Case if there is no JTAG DMI (simply bypass the thing)
  io.dbgio.debug.foreach { debug => zscale_debug.io.dmi <> debug}
  
  // Case if there is JTAG DMI
  val dtm = if (io.dbgio.jtag.isDefined) Some[DebugTransportModuleJTAG](Module (new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey)))) else None
  dtm.foreach { dtm =>
    dtm.io.jtag <> io.dbgio.jtag.get

    dtm.clock          := io.dbgio.jtag.get.TCK
    dtm.io.jtag_reset  := io.dbgio.jtag_reset.get
    dtm.io.jtag_mfr_id := io.dbgio.jtag_mfr_id.get
    dtm.reset          := dtm.io.fsmReset

    zscale_debug.io.dmi.dmi <> dtm.io.dmi
    zscale_debug.io.dmi.dmiClock := io.dbgio.jtag.get.TCK
    zscale_debug.io.dmi.dmiReset := ResetCatchAndSync(io.dbgio.jtag.get.TCK, io.dbgio.jtag_reset.get, "dmiResetCatch")
  }

  // Simply bypass this two
  io.dbgio.ndreset  := io.core.ndreset
  io.dbgio.dmactive := io.core.dmactive

}
