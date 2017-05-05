package zscaleuncore.devices

import Chisel._
import uncore.devices._
import junctions._
import util._
import regmapper._
import tile.XLen
import config._
import diplomacy._

class ZscaleDebugModule(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("debug-controller", Seq("uis,debug-013","riscv,debug-013")){
    override val alwaysExtended = true
  }
  
  lazy val module = new LazyModuleImp(this) {
    //val nComponents = intnode.bundleOut.size

    val io = new Bundle {
      //val ctrl = new DebugCtrlBundle(nComponents)
      val dmi = new ClockedDMIIO().flip
      //val in = node.bundleIn
      //val debugInterrupts = intnode.bundleOut
    }

    /*dmOuter.module.io.dmi <> io.dmi.dmi
    dmOuter.module.reset := io.dmi.dmiReset
    dmOuter.module.clock := io.dmi.dmiClock

    dmInner.module.io.innerCtrl    := dmOuter.module.io.innerCtrl
    dmInner.module.io.dmactive     := dmOuter.module.io.ctrl.dmactive
    dmInner.module.io.debugUnavail := io.ctrl.debugUnavail

    io.ctrl <> dmOuter.module.io.ctrl*/

  }
  
  // TODO: Nothing yet
}

