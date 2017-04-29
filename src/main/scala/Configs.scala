// See LICENSE for license details.

package zscale

import Chisel._
import junctions._
import rocket._
import diplomacy._
import uncore.agents._
import uncore.tilelink2._
import uncore.devices._
import uncore.converters._
import util._
import coreplex._
import scala.math.max
import scala.collection.mutable.{LinkedHashSet, ListBuffer}
import scala.collection.immutable.HashMap
import rocketchip.DefaultTestSuites._
import config._

import tile.XLen // Replacing: case object XLen extends Field[Int]
case object FastMulDiv extends Field[Boolean]
case object useMExt extends Field[Boolean]
case object useEExt extends Field[Boolean]
case object useCExt extends Field[Boolean]
case object zscaleID extends Field[Int]

class BasePlatformConfig extends Config((site, here, up) => {
  // DTS descriptive parameters (TODO: Not used yet)
  case DTSModel => "ucbbar-uis,zscale-unknown"
  case DTSCompat => Nil
  case DTSTimebase => BigInt(1000000) // 1 MHz
})

class BaseCoreplexConfig extends Config ((site, here, up) => {
  // Seems that are only used by CSR/ALU (tile and core params)
  case PAddrBits => 32
  case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
  case ASIdBits => 0
  // General
  case XLen => 64 
  case `useEExt` => false
  case `useCExt` => false
  case `useMExt` => true
  case `FastMulDiv` => true
  case `zscaleID` => 12345
})

/** Actual elaboratable target Configs */

class BaseConfig extends Config(new BaseCoreplexConfig ++ new BasePlatformConfig)
class DefaultConfig extends Config(new BaseConfig)

// Tiniest config possible
class TinyConfig extends Config(
  new BaseConfig().alter((site, here, up) => {
    case XLen => 32 
  case `useEExt` => false
  case `useCExt` => false
  case `useMExt` => false
  case `FastMulDiv` => false
  }))


