// NOTE: This is a clone of rocket-chip/src/main/scala/rocketchip/RISCVPlatform.scala
// Then is necessary to update this thing often

// last updated: dd85d7e

package zscalechip

import Chisel._
import rocketchip.HasPeripheryParameters
import config._
import junctions._
import diplomacy._
import uncore.devices._
import util._
import rocket._

/** BareTop is the root class for creating a top-level RTL module */
abstract class BareTop(implicit p: Parameters) extends LazyModule {
  ElaborationArtefacts.add("graphml", graphML)
}

abstract class BareTopBundle[+L <: BareTop](_outer: L) extends GenericParameterizedBundle(_outer) {
  val outer = _outer
  implicit val p = outer.p
}

abstract class BareTopModule[+L <: BareTop, +B <: BareTopBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = _io ()
}

/** HasTopLevelNetworks provides buses that will serve as attachment points,
  * for use in sub-traits that connect individual agents or external ports.
  */
trait HasTopLevelNetworks extends HasPeripheryParameters {
  val module: HasTopLevelNetworksModule

  // TODO: Provide buses? (Just ripped-off the TL)
}

trait HasTopLevelNetworksBundle extends HasPeripheryParameters {
  val outer: HasTopLevelNetworks
}

trait HasTopLevelNetworksModule extends HasPeripheryParameters {
  val outer: HasTopLevelNetworks
  val io: HasTopLevelNetworksBundle
}

/** Base Top class with no peripheral devices or ports added */
class BaseTop(implicit p: Parameters) extends BareTop
    with HasTopLevelNetworks {
  override lazy val module = new BaseTopModule(this, () => new BaseTopBundle(this))
}

class BaseTopBundle[+L <: BaseTop](_outer: L) extends BareTopBundle(_outer)
    with HasTopLevelNetworksBundle

class BaseTopModule[+L <: BaseTop, +B <: BaseTopBundle[L]](_outer: L, _io: () => B) extends BareTopModule(_outer, _io)
    with HasTopLevelNetworksModule
