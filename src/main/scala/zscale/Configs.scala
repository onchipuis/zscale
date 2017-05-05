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
case object useAExt extends Field[Boolean]
case object zscaleID extends Field[Int]
case object useDM extends Field[Boolean]
import rocketchip.IncludeJtagDTM
import rocketchip.{JtagDTMKey,JtagDTMKeyDefault}
import uncore.devices.DMKey
case object IncludeSPIDTM extends Field[Boolean]
case object IncludeI2CDTM extends Field[Boolean]
import rocketchip.RTCPeriod
import uncore.devices.NTiles
import tile.MaxHartIdBits

class BasePlatformConfig extends Config((site, here, up) => {
  // Debug Module
  case DMKey => DefaultDebugModuleConfig(site(XLen))
  case JtagDTMKey => new JtagDTMKeyDefault()
  // RTC module configuration (TODO: Not implemented yet)
  case RTCPeriod => BigInt(10000) // 10 KHz
  // Debug module configurations
  case `useDM` => true
  case IncludeJtagDTM => true
  // TODO: Unsupported, put always in false
  case `IncludeSPIDTM` => false
  case `IncludeI2CDTM` => false
})

class BaseCoreplexConfig extends Config ((site, here, up) => {
  // Seems that are only used by CSR/ALU (tile and core params)
  case PAddrBits => 32
  case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
  case ASIdBits => 0
  case NTiles => 1  // TODO: only 1 tile
  case MaxHartIdBits => log2Up(site(NTiles))
  // General
  case XLen => 64 
  case `useMExt` => true
  case `useEExt` => false
  case `FastMulDiv` => true
  case `zscaleID` => 12345
  // TODO: Unsupported, put always in false
  case `useCExt` => false
  case `useAExt` => false
})

/** Actual elaboratable target Configs */

class BaseConfig extends Config(new BaseCoreplexConfig ++ new BasePlatformConfig)
class DefaultConfig extends Config(new BaseConfig)

// Just a placeholder
class Default64Config extends Config(new BaseConfig)
class Default32Config extends Config(
  new BaseConfig().alter((site, here, up) => {
    case XLen => 32 
  }))

// Tiniest config possible
class TinyConfig extends Config(
  new BaseConfig().alter((site, here, up) => {
    case XLen => 32 
    case `useEExt` => true
    case `useMExt` => false
    case `FastMulDiv` => false
  }))
  



