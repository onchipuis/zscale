package zscaleuncore.devices

import Chisel._
import uncore.devices._
import junctions._
import util._
import regmapper._
import tile.XLen
import config._
import diplomacy._
import HastiConstants._

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

/** From rocket-chip:
  * Create a version of the ZscaleDebugModule which includes a synchronization interface
  * internally for the DMI. This is no longer optional outside of this module
  *  because the Clock must run when tlClock isn't running or tlReset is asserted.
  */

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

/** Convert DMI to AHB-Lite (Hasti). Avoids using special DMI synchronizers and register accesses
  *  
  */

class DMIToHasti(implicit p: Parameters) extends LazyModule {

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val dmi = new DMIIO()(p).flip()
      val out = new HastiMasterIO()(p.alterPartial({
        case HastiId => "debug"
        case HastiKey("debug") => 
          HastiParameters(
            dataBits=p(XLen),
            addrBits=p(XLen)
          )
      }))
    }
    
    // CKDUR: Well, not into TileLink, but it works like a DecoupledIO, so we are going to
    // just imitate this for now

    val src  = 0.U
    val addr = (io.dmi.req.bits.addr << 2)
    val size = (log2Ceil(DMIConsts.dmiDataSize / 8)).U

    val gbits_addr = addr
    val pfbits_addr = addr
    val pfbits_data = io.dmi.req.bits.data

    // We force DMI NOPs to go to CONTROL register because
    // Inner  may be in reset / not have a clock,
    // so we force address to be the one that goes to Outer.
    // Therefore for a NOP we don't really need to pay the penalty to go
    // across the CDC.
    
    val nbits_addr = (DMI_RegAddrs.DMI_DMCONTROL << 2).U

    when (io.dmi.req.bits.op === DMIConsts.dmi_OP_WRITE)       { io.out.haddr := pfbits_addr // Welp, address/data assign seems to be here
                                                                 io.out.hwdata := pfbits_data
                                                                 io.out.hwrite := Bool(true)
                                                                 
    }.elsewhen  (io.dmi.req.bits.op === DMIConsts.dmi_OP_READ) { io.out.haddr := gbits_addr 
                                                                 io.out.hwdata := 0.U
                                                                 io.out.hwrite := Bool(false)
                                                                 
    }.otherwise {                                                io.out.haddr := nbits_addr 
                                                                 io.out.hwdata := 0.U
                                                                 io.out.hwrite := Bool(false)
    }
    
    // Captured data
    val cap_data = Reg(init = UInt("h0", p(XLen)))
    val valid = Reg(init = Bool(false))
    when(io.out.hready & !io.dmi.resp.ready & !valid) // If there is data, but is not ready the DMI to receive it, then capture it and wait
    {
      cap_data := io.out.hrdata
      valid := Bool(true)
    }.elsewhen(valid & io.dmi.req.valid & io.dmi.resp.ready)
    {
      cap_data := 0.U
      valid := Bool(false)
    }
    
    io.out.hsize := size
    io.out.hburst := HBURST_SINGLE
    // TODO: This is ok? We are holding because the ready status for the response
    io.out.htrans := Mux(io.dmi.req.valid & io.dmi.resp.ready, HTRANS_NONSEQ, HTRANS_IDLE)
    
    io.dmi.req.ready := io.out.hready

    io.dmi.resp.valid      := io.out.hready       // TODO: This is ok?
    io.dmi.resp.bits.resp  := DMIConsts.dmi_RESP_SUCCESS // TODO: Always success? If not, return DMIConsts.dmi_RESP_FAILURE
    io.dmi.resp.bits.data  := Mux(valid, cap_data, io.out.hrdata) 
    io.out.hprot := UInt("b0011")
    io.out.hmastlock := Bool(false)
  }
}

