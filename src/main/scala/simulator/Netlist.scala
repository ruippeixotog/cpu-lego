package simulator

import java.util.IdentityHashMap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import core.*

/** Flat, integer-indexed netlist: the shared input for simulation engines and netlist analyses.
  *
  * Built from a [[core.Component]] hierarchy in a single iterative pass (deep hierarchies cannot blow the stack). Every
  * [[core.Port]] is interned to a dense integer id by identity; ports joined by wires are merged into nets with a
  * union-find (path compression and union by rank) and compacted to dense net ids. Net 0 is reserved for the
  * [[core.High]] constant, net 1 for [[core.Low]].
  *
  * Primitives are stored in struct-of-arrays form, holding net ids (except `clockHalfPeriod`, which holds ticks):
  *   - NAND: `nandIn1`, `nandIn2`, `nandOut`
  *   - Switch: `switchIn`, `switchEn`, `switchOut`
  *   - Clock: `clockHalfPeriod`, `clockOut`
  *
  * Per-net driver and reader lists are CSR arrays (`driverOffsets`/`driverEntries`, `readerOffsets`/`readerEntries`).
  * Entries pack a primitive reference; see [[Netlist.packRef]]. Drivers are NAND and Clock outputs plus Switch outputs
  * (switches are tri-state drivers: `GateProcessor.setup` drives `out` to `in` while enabled and to `None` otherwise);
  * readers are NAND inputs and Switch in/enable inputs. The constant nets carry no driver entries.
  *
  * `names` maps every hierarchical port name (`a.b.c`, `bus[i]`, following [[Index]]'s scheme) to its net id, so any
  * named port stays reachable even when several merge into one net; `nameOf` derives a display name per net. `pins` are
  * the top-level component's `namedPorts` with directions and net ids (buses expanded to one pin per element).
  *
  * All arrays are treated as immutable after construction.
  */
final class Netlist private (
    /** Port id -> original [[core.Port]]. Dense ids `0` until `portCount`. */
    val ports: Array[Port],
    /** Port id -> dense net id. */
    val netOfPort: Array[Int],
    /** Number of nets, including the two reserved constant nets. */
    val netCount: Int,
    /** Representative port id per net (the lowest port id on the net). */
    val netRepresentative: Array[Int],
    /** NAND input net ids. */
    val nandIn1: Array[Int],
    /** NAND input net ids. */
    val nandIn2: Array[Int],
    /** NAND output net ids. */
    val nandOut: Array[Int],
    /** Switch input net ids. */
    val switchIn: Array[Int],
    /** Switch enable net ids. */
    val switchEn: Array[Int],
    /** Switch output net ids. */
    val switchOut: Array[Int],
    /** Clock half periods in ticks. */
    val clockHalfPeriod: Array[Int],
    /** Clock output net ids. */
    val clockOut: Array[Int],
    /** CSR offsets into `driverEntries`, length `netCount + 1`. */
    val driverOffsets: Array[Int],
    /** Packed primitive references driving each net (NAND outputs, Switch outputs and Clock outputs). */
    val driverEntries: Array[Int],
    /** CSR offsets into `readerEntries`, length `netCount + 1`. */
    val readerOffsets: Array[Int],
    /** Packed primitive references reading each net (NAND inputs, Switch in/enable). */
    val readerEntries: Array[Int],
    /** Hierarchical name -> net id, for every named port. */
    val names: Map[String, Int],
    /** Hierarchical names in first-encountered order, for display-name derivation. */
    private val orderedNames: Vector[(String, Int)],
    /** The top-level component's named ports, with directions and net ids. */
    val pins: Vector[Netlist.Pin],
    /** Summary counts. */
    val stats: Netlist.Stats,
    private val portIds: IdentityHashMap[Port, Integer]
) {

  /** Number of interned ports. */
  def portCount: Int = ports.length

  /** Reserved net id of the [[core.High]] constant. */
  val highNet: Int = 0

  /** Reserved net id of the [[core.Low]] constant. */
  val lowNet: Int = 1

  /** Dense id of `port`, or -1 if the port is not in this netlist. */
  def portIdOf(port: Port): Int = {
    val id = portIds.get(port)
    if (id == null) -1 else id.intValue()
  }

  /** Dense net id of `port`, or -1 if the port is not in this netlist. */
  def netOf(port: Port): Int = {
    val pid = portIdOf(port)
    if (pid < 0) -1 else netOfPort(pid)
  }

  /** Packed driver references driving `net`. See [[Netlist.refKind]] and [[Netlist.refIndex]]. */
  def driversOf(net: Int): Array[Int] = driverEntries.slice(driverOffsets(net), driverOffsets(net + 1))

  /** Packed reader references reading `net`. See [[Netlist.refKind]] and [[Netlist.refIndex]]. */
  def readersOf(net: Int): Array[Int] = readerEntries.slice(readerOffsets(net), readerOffsets(net + 1))

  /** A display name for `net`: the first hierarchical name recorded for it, if any. */
  def nameOf(net: Int): Option[String] = displayNames.get(net)

  private lazy val displayNames: Map[Int, String] = {
    val m = mutable.LinkedHashMap[Int, String]()
    orderedNames.foreach { case (name, net) => m.getOrElseUpdate(net, name) }
    m.toMap
  }

  private lazy val membersByNet: Map[Int, Set[Port]] = {
    val m = mutable.HashMap[Int, mutable.HashSet[Port]]()
    var pid = 0
    while (pid < ports.length) {
      m.getOrElseUpdate(netOfPort(pid), mutable.HashSet()) += ports(pid)
      pid += 1
    }
    m.view.mapValues(_.toSet).toMap
  }

  /** Canonical representative port of `port`'s net, if the port is known. */
  def representativeOf(port: Port): Option[Port] = {
    val id = portIds.get(port)
    if (id == null) None else Some(ports(netRepresentative(netOfPort(id))))
  }

  /** Every port on the same net as `port`, if the port is known. */
  def netPorts(port: Port): Option[Set[Port]] = {
    val id = portIds.get(port)
    if (id == null) None else Some(membersByNet(netOfPort(id)))
  }
}

object Netlist {

  /** Primitive kind ids, matching the struct-of-arrays tables. */
  val NandKind = 0
  val SwitchKind = 1
  val ClockKind = 2

  /** Pack a (kind, index) primitive reference into a single Int for the driver/reader lists. Supports up to 2^28
    * primitives per kind.
    */
  def packRef(kind: Int, index: Int): Int = (kind << 28) | index

  /** The primitive kind of a packed reference (one of [[NandKind]], [[SwitchKind]], [[ClockKind]]). */
  def refKind(ref: Int): Int = ref >>> 28

  /** The index into the kind's struct-of-arrays table of a packed reference. */
  def refIndex(ref: Int): Int = ref & 0x0fffffff

  /** A named pin of the top-level component: hierarchical name, optional direction, dense net id. */
  case class Pin(name: String, direction: Option[Direction], net: Int)

  /** Summary counts over the netlist. */
  case class Stats(
      nands: Int,
      switches: Int,
      clocks: Int,
      nets: Int,
      ports: Int,
      maxFanout: Int
  )

  /** Flatten `root` into a netlist, merging `extraWires` first. The traversal is iterative and linear in the size of
    * the hierarchy; components keep depth-first traversal order and wires keep their original pairing.
    */
  def apply(root: Component, extraWires: List[(Port, Port)] = Nil): Netlist = {
    val b = new Builder
    traverse(root, extraWires, b)
    assemble(b)
  }

  /** Like [[apply]], additionally returning the flattened components and wires in traversal order. */
  private[simulator] def parts(
      root: Component,
      extraWires: List[(Port, Port)]
  ): (Netlist, List[BaseComponent], List[(Port, Port)]) = {
    val b = new Builder
    traverse(root, extraWires, b)
    (assemble(b), b.components.toList, b.wires.toList)
  }

  private def traverse(root: Component, extraWires: List[(Port, Port)], b: Builder): Unit = {
    root match {
      case cc: CompositeComponent =>
        cc.namedPorts.foreach {
          case (name, (dir, p: Port)) => b.addPin(name, dir, p)
          case (name, (dir, bus: Bus)) =>
            bus.zipWithIndex.foreach { case (p, i) => b.addPin(s"$name[$i]", dir, p) }
        }
      case _ => // a bare primitive has no pins
    }
    extraWires.foreach { case (a, c) => b.addWire(a, c) }

    // Iterative depth-first traversal; children pushed reversed so they pop in `components` order.
    val stack = new java.util.ArrayDeque[(Component, String)]()
    stack.push((root, ""))
    while (!stack.isEmpty) {
      val (comp, path) = stack.pop()
      comp match {
        case n: NAND => b.addNand(n)
        case c: Clock => b.addClock(c)
        case s: Switch => b.addSwitch(s)
        case cc: CompositeComponent =>
          val prefix = if (path.isEmpty) "" else path + "."
          cc.namedPorts.foreach {
            case (name, (_, p: Port)) => b.addName(p, prefix + name)
            case (name, (_, bus: Bus)) =>
              bus.zipWithIndex.foreach { case (p, i) => b.addName(p, s"$prefix$name[$i]") }
          }
          cc.wires.foreach { case (a, c) => b.addWire(a, c) }
          val kids = cc.components.toList
          var k = kids.size - 1
          while (k >= 0) {
            val (cname, c) = kids(k)
            stack.push((c, if (path.isEmpty) cname else path + "." + cname))
            k -= 1
          }
      }
    }
  }

  /** Build from an already-flat component and wire list. */
  private[simulator] def fromFlat(components: List[BaseComponent], wires: List[(Port, Port)]): Netlist = {
    val b = new Builder
    components.foreach {
      case n: NAND => b.addNand(n)
      case c: Clock => b.addClock(c)
      case s: Switch => b.addSwitch(s)
    }
    wires.foreach { case (a, c) => b.addWire(a, c) }
    assemble(b)
  }

  private final class Builder {
    val portIds = new IdentityHashMap[Port, Integer]()
    val portList = new ArrayBuffer[Port]()
    val components = new ArrayBuffer[BaseComponent]()
    val wires = new ArrayBuffer[(Port, Port)]()
    val primKind = new ArrayBuffer[Int]()
    val primA = new ArrayBuffer[Int]()
    val primB = new ArrayBuffer[Int]()
    val primC = new ArrayBuffer[Int]()
    val primAux = new ArrayBuffer[Int]()
    val wireA = new ArrayBuffer[Int]()
    val wireB = new ArrayBuffer[Int]()
    val names = new ArrayBuffer[(Int, String)]()
    val pins = new ArrayBuffer[(String, Option[Direction], Int)]()

    def idOf(port: Port): Int = {
      val existing = portIds.get(port)
      if (existing != null) existing.intValue()
      else {
        val id = portList.size
        portList += port
        portIds.put(port, Integer.valueOf(id))
        id
      }
    }

    def addNand(n: NAND): Unit = {
      components += n
      primKind += NandKind
      primA += idOf(n.in1)
      primB += idOf(n.in2)
      primC += idOf(n.out)
      primAux += 0
    }

    def addSwitch(s: Switch): Unit = {
      components += s
      primKind += SwitchKind
      primA += idOf(s.in)
      primB += idOf(s.out)
      primC += idOf(s.enable)
      primAux += 0
    }

    def addClock(c: Clock): Unit = {
      components += c
      primKind += ClockKind
      primA += -1
      primB += idOf(c.out)
      primC += -1
      primAux += c.freq
    }

    def addWire(a: Port, c: Port): Unit = {
      wires += ((a, c))
      wireA += idOf(a)
      wireB += idOf(c)
    }

    def addName(port: Port, name: String): Unit =
      names += ((idOf(port), name))

    def addPin(name: String, dir: Option[Direction], port: Port): Unit =
      pins += ((name, dir, idOf(port)))
  }

  private def assemble(b: Builder): Netlist = {
    val highId = b.idOf(High)
    val lowId = b.idOf(Low)
    val portCount = b.portList.size

    // Union-find with path compression and union by rank.
    val parent = Array.tabulate(portCount)(i => i)
    val rank = new Array[Byte](portCount)

    def find(x: Int): Int = {
      var r = x
      while (parent(r) != r) r = parent(r)
      var c = x
      while (c != r) {
        val nxt = parent(c)
        parent(c) = r
        c = nxt
      }
      r
    }

    def union(a: Int, c: Int): Unit = {
      val ra = find(a)
      val rb = find(c)
      if (ra != rb) {
        if (rank(ra) < rank(rb)) parent(ra) = rb
        else if (rank(ra) > rank(rb)) parent(rb) = ra
        else {
          parent(rb) = ra
          rank(ra) = (rank(ra) + 1).toByte
        }
      }
    }

    var w = 0
    while (w < b.wireA.size) {
      union(b.wireA(w), b.wireB(w))
      w += 1
    }

    // Compact to dense net ids. Net 0 is the High constant, net 1 the Low constant.
    val highRoot = find(highId)
    val lowRoot = find(lowId)
    val netOfPort = new Array[Int](portCount)
    val rootToNet = mutable.HashMap[Int, Int](highRoot -> 0)
    var nextNet = 2
    // If High and Low are wired together (a short), net 1 stays reserved but empty.
    if (lowRoot != highRoot) rootToNet(lowRoot) = 1
    var pid = 0
    while (pid < portCount) {
      val r = find(pid)
      val net = rootToNet.getOrElseUpdate(r, { val v = nextNet; nextNet += 1; v })
      netOfPort(pid) = net
      pid += 1
    }
    val netCount = nextNet

    val netRepresentative = Array.fill(netCount)(-1)
    pid = 0
    while (pid < portCount) {
      val net = netOfPort(pid)
      if (netRepresentative(net) < 0) netRepresentative(net) = pid
      pid += 1
    }

    // Split primitives into struct-of-arrays, in traversal order per kind.
    val primCount = b.primKind.size
    var nNands = 0
    var nSwitches = 0
    var nClocks = 0
    var i = 0
    while (i < primCount) {
      b.primKind(i) match {
        case NandKind => nNands += 1
        case SwitchKind => nSwitches += 1
        case ClockKind => nClocks += 1
      }
      i += 1
    }
    val nandIn1 = new Array[Int](nNands)
    val nandIn2 = new Array[Int](nNands)
    val nandOut = new Array[Int](nNands)
    val switchIn = new Array[Int](nSwitches)
    val switchEn = new Array[Int](nSwitches)
    val switchOut = new Array[Int](nSwitches)
    val clockHalfPeriod = new Array[Int](nClocks)
    val clockOut = new Array[Int](nClocks)
    var jn = 0
    var js = 0
    var jc = 0
    i = 0
    while (i < primCount) {
      b.primKind(i) match {
        case NandKind =>
          nandIn1(jn) = netOfPort(b.primA(i))
          nandIn2(jn) = netOfPort(b.primB(i))
          nandOut(jn) = netOfPort(b.primC(i))
          jn += 1
        case SwitchKind =>
          switchIn(js) = netOfPort(b.primA(i))
          switchOut(js) = netOfPort(b.primB(i))
          switchEn(js) = netOfPort(b.primC(i))
          js += 1
        case ClockKind =>
          clockHalfPeriod(jc) = b.primAux(i)
          clockOut(jc) = netOfPort(b.primB(i))
          jc += 1
      }
      i += 1
    }

    // CSR driver lists: NAND and Clock outputs, plus Switch outputs (tri-state drivers).
    val driverCounts = new Array[Int](netCount)
    i = 0
    while (i < nNands) { driverCounts(nandOut(i)) += 1; i += 1 }
    i = 0
    while (i < nSwitches) { driverCounts(switchOut(i)) += 1; i += 1 }
    i = 0
    while (i < nClocks) { driverCounts(clockOut(i)) += 1; i += 1 }
    val driverOffsets = new Array[Int](netCount + 1)
    var acc = 0
    var k = 0
    while (k < netCount) { driverOffsets(k) = acc; acc += driverCounts(k); k += 1 }
    driverOffsets(netCount) = acc
    val driverEntries = new Array[Int](acc)
    val dcur = driverOffsets.clone()
    i = 0
    while (i < nNands) {
      val net = nandOut(i)
      driverEntries(dcur(net)) = packRef(NandKind, i)
      dcur(net) += 1
      i += 1
    }
    i = 0
    while (i < nSwitches) {
      val net = switchOut(i)
      driverEntries(dcur(net)) = packRef(SwitchKind, i)
      dcur(net) += 1
      i += 1
    }
    i = 0
    while (i < nClocks) {
      val net = clockOut(i)
      driverEntries(dcur(net)) = packRef(ClockKind, i)
      dcur(net) += 1
      i += 1
    }

    // CSR reader lists: NAND inputs, Switch in/enable.
    val readerCounts = new Array[Int](netCount)
    i = 0
    while (i < nNands) { readerCounts(nandIn1(i)) += 1; readerCounts(nandIn2(i)) += 1; i += 1 }
    i = 0
    while (i < nSwitches) { readerCounts(switchIn(i)) += 1; readerCounts(switchEn(i)) += 1; i += 1 }
    val readerOffsets = new Array[Int](netCount + 1)
    acc = 0
    k = 0
    while (k < netCount) { readerOffsets(k) = acc; acc += readerCounts(k); k += 1 }
    readerOffsets(netCount) = acc
    val readerEntries = new Array[Int](acc)
    val rcur = readerOffsets.clone()
    def addReader(net: Int, ref: Int): Unit = {
      readerEntries(rcur(net)) = ref
      rcur(net) += 1
    }
    i = 0
    while (i < nNands) {
      addReader(nandIn1(i), packRef(NandKind, i))
      addReader(nandIn2(i), packRef(NandKind, i))
      i += 1
    }
    i = 0
    while (i < nSwitches) {
      addReader(switchIn(i), packRef(SwitchKind, i))
      addReader(switchEn(i), packRef(SwitchKind, i))
      i += 1
    }

    // Every hierarchical name stays reachable, mapped to its net id; display names
    // keep first-encountered order.
    val orderedNames = b.names.map { case (portId, name) => (name, netOfPort(portId)) }.toVector
    val names = orderedNames.toMap

    val pins = b.pins.map { case (name, dir, portId) => Pin(name, dir, netOfPort(portId)) }.toVector

    val stats = Stats(
      nands = nNands,
      switches = nSwitches,
      clocks = nClocks,
      nets = netCount,
      ports = portCount,
      maxFanout = if (readerCounts.isEmpty) 0 else readerCounts.max
    )

    new Netlist(
      ports = b.portList.toArray,
      netOfPort = netOfPort,
      netCount = netCount,
      netRepresentative = netRepresentative,
      nandIn1 = nandIn1,
      nandIn2 = nandIn2,
      nandOut = nandOut,
      switchIn = switchIn,
      switchEn = switchEn,
      switchOut = switchOut,
      clockHalfPeriod = clockHalfPeriod,
      clockOut = clockOut,
      driverOffsets = driverOffsets,
      driverEntries = driverEntries,
      readerOffsets = readerOffsets,
      readerEntries = readerEntries,
      names = names,
      orderedNames = orderedNames,
      pins = pins,
      stats = stats,
      portIds = b.portIds
    )
  }
}
