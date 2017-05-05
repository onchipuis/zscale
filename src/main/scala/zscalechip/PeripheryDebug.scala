// NOTE: This is a clone of rocket-chip/src/main/scala/rocketchip/RISCVPlatform.scala
// Then is necessary to update this thing often

// last updated: dd85d7e

package zscalechip

import Chisel._
import rocketchip.{IncludeJtagDTM, JtagDTMConfig, JtagDTMKey, JtagDTMKeyDefault, DebugTransportModuleJTAG}
import config._
import diplomacy._
import uncore.devices._
import util._
import jtag.JTAGIO
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
  val dbgio = new DebugComBundle
  
  val dmi = new ClockedDMIIO().flip
  // TODO: Add the core interface
  // TODO: Add the master memory interface
}

class PeripheryDebugModule(implicit p: Parameters) extends Module {
  val io = new PeripheryDebugBundle

  // Case if there is no JTAG DMI (simply bypass the thing)
  io.dbgio.debug.foreach { debug => debug <> io.dmi}
  
  // Case if there is JTAG DMI
  val dtm = if (io.dbgio.jtag.isDefined) Some[DebugTransportModuleJTAG](Module (new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey)))) else None
  dtm.foreach { dtm =>
    dtm.io.jtag <> io.dbgio.jtag.get

    dtm.clock          := io.dbgio.jtag.get.TCK
    dtm.io.jtag_reset  := io.dbgio.jtag_reset.get
    dtm.io.jtag_mfr_id := io.dbgio.jtag_mfr_id.get
    dtm.reset          := dtm.io.fsmReset

    io.dmi <> dtm.io.dmi
    //outer.coreplex.module.io.debug.dmiClock := io.jtag.get.TCK
    //outer.coreplex.module.io.debug.dmiReset := ResetCatchAndSync(io.jtag.get.TCK, io.jtag_reset.get, "dmiResetCatch")
  }

  //io.ndreset  := outer.coreplex.module.io.ndreset
  //io.dmactive := outer.coreplex.module.io.dmactive

}
