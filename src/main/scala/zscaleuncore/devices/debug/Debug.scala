package zscaleuncore.devices

import Chisel._
import uncore.devices._
import junctions._
import util._
import regmapper._
import tile.XLen
import config._
import diplomacy._

case object IncludeDMMem extends Field[Boolean]

class ZscaleDebugMemBundle(implicit p: Parameters) extends Bundle {
  val mem = new HastiMasterIO()(p.alterPartial({
    case HastiId => "00002"
    case HastiKey("00002") => 
      HastiParameters(
        dataBits=p(XLen),
        addrBits=p(XLen)
      )
  }))
}

class ZscaleDebugModule(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("debug-controller", Seq("uis,debug-013","riscv,debug-013")){
    override val alwaysExtended = true
  }

  //val node = TLInputNode()
  //val intnode = IntOutputNode()

  val dmOuter = LazyModule(new ZscaleDebugModuleOuterAsync(device)(p))
  val dmInner = LazyModule(new ZscaleDebugModuleInnerAsync(device, () => {intnode.bundleOut.size})(p))
  
  //dmInner.dmiNode := dmOuter.dmiInnerNode
  //dmInner.tlNode := node
  //intnode :*= dmOuter.intnode
  
  lazy val module = new LazyModuleImp(this) {
    //val nComponents = intnode.bundleOut.size

    val io = new Bundle {
      val nComponents = 1 // TODO: Just 1 component?
      //val in = node.bundleIn
      //val debugInterrupts = intnode.bundleOut
      val dmi = new ClockedDMIIO().flip
      val mem = if(p(IncludeDMMem)) Some(new ZscaleDebugMemBundle) else None
      val ctrl = new DebugCtrlBundle(nComponents)
    }

    dmOuter.module.io.dmi <> io.dmi.dmi
    dmOuter.module.reset := io.dmi.dmiReset
    dmOuter.module.clock := io.dmi.dmiClock

    dmInner.module.io.innerCtrl    := dmOuter.module.io.innerCtrl
    dmInner.module.io.dmactive     := dmOuter.module.io.ctrl.dmactive
    dmInner.module.io.debugUnavail := io.ctrl.debugUnavail

    io.ctrl <> dmOuter.module.io.ctrl

  }
  
  // TODO: Nothing yet
}

