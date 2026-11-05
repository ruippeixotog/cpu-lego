package simulator

import scala.collection.mutable

/** A single characterised cyclic region of a [[Netlist]]: a bistable storage core, an oscillator, or a cyclic region
  * that could not be characterised as either.
  *
  * @param kind
  *   the classification of the region
  * @param memberNets
  *   the nets inside the region (the nets driven by the region's primitives)
  * @param inputNets
  *   the nets outside the region that its primitives read, in ascending order; constant nets are folded before the
  *   analysis and never appear here
  * @param outputNets
  *   the member nets that leave the region — read by primitives outside it, or exposed as top-level pins — in ascending
  *   order
  * @param stateNet
  *   for bistables, the representative member net whose value is the stored bit
  * @param nextState
  *   for bistables, the next-state truth table `q' = f(inputs, q)` over `inputNets` and the state bit
  */
final case class StateElement(
    kind: StateElement.Kind,
    memberNets: Set[Int],
    inputNets: Vector[Int],
    outputNets: Vector[Int],
    stateNet: Option[Int],
    nextState: Option[StateElement.NextState]
)

object StateElement {

  enum Kind {
    case Bistable, Oscillator, Unsupported
  }

  /** The next-state function of a bistable, as a truth table over its external inputs and the current state bit.
    *
    * `values` is indexed by a bitmask: bit `p` is the value of `inputNets(p)`, and bit `inputNets.size` is the current
    * state bit.
    */
  final case class NextState(inputNets: Vector[Int], stateNet: Int, values: Vector[Boolean]) {

    /** The next state bit under the given input values (one entry per net in `inputNets`) and current state bit. */
    def apply(inputs: Map[Int, Boolean], state: Boolean): Boolean = {
      var idx = 0
      var p = 0
      while (p < inputNets.size) {
        if (inputs(inputNets(p))) idx |= (1 << p)
        p += 1
      }
      if (state) idx |= (1 << inputNets.size)
      values(idx)
    }
  }
}

/** The result of the state-element analysis over a [[Netlist]]: every bistable core found (at latch granularity), plus
  * the cyclic regions that remained after cutting at the bistables and were characterised as oscillators or
  * unsupported.
  *
  * @param elements
  *   the characterised regions, ordered by their lowest member net
  * @param passes
  *   per-pass counts of the cut-and-recompute iteration
  */
final case class StateElements(elements: Vector[StateElement], passes: Vector[StateElements.Pass]) {

  /** The bistable storage elements found. */
  def bistables: Vector[StateElement] = elements.filter(_.kind == StateElement.Kind.Bistable)

  /** The residual cyclic regions characterised as oscillators. */
  def oscillators: Vector[StateElement] = elements.filter(_.kind == StateElement.Kind.Oscillator)

  /** The residual cyclic regions that could not be characterised. */
  def unsupported: Vector[StateElement] = elements.filter(_.kind == StateElement.Kind.Unsupported)

  /** A human-readable report: per-pass counts, then every element with hierarchical net names. */
  def report(netlist: Netlist): String = {
    def name(net: Int): String = netlist.nameOf(net).getOrElse(s"<net $net>")
    val sb = new StringBuilder
    sb ++= s"State elements: ${bistables.size} bistable, ${oscillators.size} oscillator, ${unsupported.size} unsupported\n"
    passes.zipWithIndex.foreach { case (pass, i) =>
      sb ++= s"Pass ${i + 1}: ${pass.bistablesFound} bistables found, ${pass.cutsMade} cuts made, ${pass.residualSccs} residual SCCs\n"
    }
    elements.foreach { el =>
      sb ++= s"\n${el.kind}: members [${el.memberNets.toVector.sorted.map(name).mkString(", ")}]"
      sb ++= s"\n  inputs: [${el.inputNets.map(name).mkString(", ")}]"
      sb ++= s"\n  outputs: [${el.outputNets.map(name).mkString(", ")}]"
      for {
        stateNet <- el.stateNet
        next <- el.nextState
      } {
        var sets, resets, holds = 0
        var a = 0
        while (a < (1 << el.inputNets.size)) {
          val fromLow = next.values(a)
          val fromHigh = next.values(a | (1 << el.inputNets.size))
          if (fromLow && fromHigh) sets += 1
          else if (!fromLow && !fromHigh) resets += 1
          else holds += 1
          a += 1
        }
        sb ++= s"\n  state: ${name(stateNet)}; over ${1 << el.inputNets.size} input assignments: $sets set, $resets reset, $holds hold"
      }
      sb ++= "\n"
    }
    sb.result()
  }
}

object StateElements {

  /** Per-pass counts of the analysis iteration. `residualSccs` is the number of cyclic SCCs left after the pass's cuts
    * (for the last pass, the regions reported as oscillators or unsupported).
    */
  final case class Pass(bistablesFound: Int, cutsMade: Int, residualSccs: Int)

  /** Maximum number of internal nets for a region to be characterised exhaustively. */
  val MaxInternalNets = 16

  /** Maximum number of external inputs for a region to be characterised exhaustively. */
  val MaxExternalInputs = 8

  /** Maximum number of nets in a single cycle considered by the core search. Unions of overlapping cycles can grow a
    * core up to [[MaxInternalNets]] nets.
    */
  private val MaxCycleNets = 12

  /** Maximum number of cycles enumerated from a single seed net in the core search. */
  private val MaxCyclesPerSeed = 256

  /** Total DFS expansion budget for the core search inside one SCC. Keeps hub nets (e.g. buses) from making the search
    * unbounded; exhausting it only means fewer cores are searched for, never a wrong characterisation.
    */
  private val CoreSearchBudget = 40000000

  /** Analyse `netlist`, finding where storage lives without knowing anything about the components it was built from.
    *
    * The gate graph (an edge from each net to the nets driven by the primitives reading it) is decomposed into SCCs. An
    * SCC that characterises as a bistable is a state element. Inside any other SCC, minimal storage cores — unions of
    * overlapping short cycles — are characterised the same way, with signals entering the core from the enclosing loop
    * treated as free inputs. The graph is then cut at every bistable found — the nets of a found bistable become
    * pseudo-inputs, severing the loops through it — and the SCCs are recomputed, repeating until nothing changes.
    * Whatever is still cyclic at that point is a real combinational loop, characterised normally: an oscillator if some
    * input assignment has no stable state, unsupported otherwise.
    */
  def apply(netlist: Netlist): StateElements = new Analyser(netlist).run()

  private sealed trait Characterisation
  private case class BistableChar(next: StateElement.NextState, stateNet: Int) extends Characterisation
  private case object OscillatorChar extends Characterisation
  private case object UnsupportedChar extends Characterisation

  private final class Analyser(netlist: Netlist) {

    // Net graph adjacency: succ(n) = nets driven by the primitives reading n, ascending and duplicate-free.
    private val succ: Array[Array[Int]] = Array.tabulate(netlist.netCount) { net =>
      val out = mutable.ArrayBuffer[Int]()
      val rs = netlist.readersOf(net)
      var i = 0
      while (i < rs.length) {
        Netlist.refKind(rs(i)) match {
          case Netlist.NandKind => out += netlist.nandOut(Netlist.refIndex(rs(i)))
          case Netlist.SwitchKind => out += netlist.switchOut(Netlist.refIndex(rs(i)))
          case _ => // clocks read nothing and never appear as readers
        }
        i += 1
      }
      out.distinct.sorted.toArray
    }

    private val cuts = mutable.Set[Int]()
    private val elements = mutable.ArrayBuffer[StateElement]()
    private val recordedMembers = mutable.Set[Set[Int]]()

    /** Nets exposed as pins of the top-level component: a region net on a pin leaves the region even when no primitive
      * reads it.
      */
    private val pinNets: Set[Int] = netlist.pins.map(_.net).toSet

    def run(): StateElements = {
      val passCounts = mutable.ArrayBuffer[(Int, Int)]()
      val passCandidates = mutable.ArrayBuffer[Int]()
      var lastCandidates = Vector.empty[Vector[Int]]
      var searching = true
      while (searching) {
        val candidates = cyclicSccs()
        lastCandidates = candidates
        passCandidates += candidates.size
        var bistables = 0
        var newCuts = 0
        candidates.foreach { scc =>
          val found = findBistables(scc)
          found.foreach { el =>
            if (!recordedMembers.contains(el.memberNets)) {
              recordedMembers += el.memberNets
              elements += el
              bistables += 1
              el.memberNets.foreach { net =>
                if (!cuts.contains(net)) {
                  cuts += net
                  newCuts += 1
                }
              }
            }
          }
        }
        passCounts += ((bistables, newCuts))
        searching = newCuts > 0
      }
      // Whatever is still cyclic is a real combinational loop: characterise it normally.
      lastCandidates.foreach { scc =>
        if (!recordedMembers.contains(scc.toSet)) {
          elements += residualElement(scc)
        }
      }
      val passes = passCounts.indices.map { i =>
        val residual = if (i + 1 < passCandidates.size) passCandidates(i + 1) else lastCandidates.size
        Pass(passCounts(i)._1, passCounts(i)._2, residual)
      }.toVector
      StateElements(elements.toVector.sortBy(el => (el.memberNets.min, el.memberNets.size)), passes)
    }

    /** Every bistable in one SCC: the SCC itself if it characterises as one, otherwise the bistable cores inside it.
      */
    private def findBistables(scc: Vector[Int]): Vector[StateElement] = {
      val region = new Region(scc)
      if (region.withinLimits) {
        characterise(region) match {
          case b: BistableChar => return Vector(element(StateElement.Kind.Bistable, region, Some(b)))
          case _ =>
        }
      }
      findBistableCores(scc.toSet).map { case (core, b) =>
        element(StateElement.Kind.Bistable, new Region(core), Some(b))
      }
    }

    private def residualElement(scc: Vector[Int]): StateElement = {
      val region = new Region(scc)
      if (!region.withinLimits) element(StateElement.Kind.Unsupported, region, None)
      else {
        characterise(region) match {
          case b: BistableChar => element(StateElement.Kind.Bistable, region, Some(b))
          case OscillatorChar => element(StateElement.Kind.Oscillator, region, None)
          case UnsupportedChar => element(StateElement.Kind.Unsupported, region, None)
        }
      }
    }

    private def element(kind: StateElement.Kind, region: Region, bistable: Option[BistableChar]): StateElement =
      StateElement(
        kind,
        region.members.toSet,
        region.inputNets,
        region.outputNets,
        bistable.map(_.stateNet),
        bistable.map(_.next)
      )

    /** The cyclic SCCs of the graph with the current cuts applied, in a deterministic order. An SCC is cyclic when it
      * has more than one net, or its single net feeds itself.
      */
    private def cyclicSccs(): Vector[Vector[Int]] =
      stronglyConnectedComponents()
        .filter { comp =>
          comp.size > 1 || (comp.size == 1 && !cuts.contains(comp.head) && succ(comp.head).contains(comp.head))
        }
        .map(_.sorted)
        .sortBy(c => (c.head, c.size))

    /** Iterative Tarjan over the net graph, with edges out of cut nets removed. */
    private def stronglyConnectedComponents(): Vector[Vector[Int]] = {
      val n = netlist.netCount
      val index = Array.fill(n)(-1)
      val low = new Array[Int](n)
      val onStack = new Array[Boolean](n)
      val stack = mutable.ArrayDeque[Int]()
      val result = mutable.ArrayBuffer[Vector[Int]]()
      var counter = 0
      var root = 0
      while (root < n) {
        if (index(root) < 0) {
          val work = mutable.ArrayDeque[(Int, Int)]()
          index(root) = counter
          low(root) = counter
          counter += 1
          stack += root
          onStack(root) = true
          work += ((root, 0))
          while (work.nonEmpty) {
            val (v, ci) = work.last
            val children = if (cuts.contains(v)) Array.emptyIntArray else succ(v)
            if (ci < children.length) {
              work(work.size - 1) = (v, ci + 1)
              val w = children(ci)
              if (index(w) < 0) {
                index(w) = counter
                low(w) = counter
                counter += 1
                stack += w
                onStack(w) = true
                work += ((w, 0))
              } else if (onStack(w)) {
                low(v) = math.min(low(v), index(w))
              }
            } else {
              work.removeLast()
              if (work.nonEmpty) {
                val (p, _) = work.last
                low(p) = math.min(low(p), low(v))
              }
              if (low(v) == index(v)) {
                val comp = Vector.newBuilder[Int]
                var w = -1
                while (w != v) {
                  w = stack.removeLast()
                  onStack(w) = false
                  comp += w
                }
                result += comp.result()
              }
            }
          }
        }
        root += 1
      }
      result.toVector
    }

    /** A set of nets with the primitives driving them, evaluated as a unit-delay Boolean region. Constant nets are
      * folded into the evaluation, so they never take part in the member/input accounting.
      */
    private final class Region(val members: Vector[Int]) {
      val memberSet: Set[Int] = members.toSet

      private val memberPos: Map[Int, Int] = members.zipWithIndex.toMap

      /** Packed references of every primitive driving a member net (NANDs and Switches; clocks are flagged apart). */
      val primRefs: Vector[Int] =
        members.flatMap(net => netlist.driversOf(net).toVector).distinct.filter { ref =>
          Netlist.refKind(ref) != Netlist.ClockKind
        }

      private val primRefSet: Set[Int] = primRefs.toSet

      val hasClockDriver: Boolean =
        members.exists(net => netlist.driversOf(net).exists(ref => Netlist.refKind(ref) == Netlist.ClockKind))

      val inputNets: Vector[Int] =
        primRefs
          .flatMap { ref =>
            Netlist.refKind(ref) match {
              case Netlist.NandKind =>
                val i = Netlist.refIndex(ref)
                List(netlist.nandIn1(i), netlist.nandIn2(i))
              case Netlist.SwitchKind =>
                val i = Netlist.refIndex(ref)
                List(netlist.switchIn(i), netlist.switchEn(i))
              case _ => Nil
            }
          }
          .filter(net => net > 1 && !memberSet.contains(net))
          .distinct
          .sorted

      private val inputPos: Map[Int, Int] = inputNets.zipWithIndex.toMap

      val outputNets: Vector[Int] =
        members.filter { net =>
          pinNets.contains(net) || netlist.readersOf(net).exists(ref => !primRefSet.contains(ref))
        }

      def withinLimits: Boolean =
        members.size <= MaxInternalNets && inputNets.size <= MaxExternalInputs && !hasClockDriver

      /** The value of member `net` in the candidate state bitmask `state`. */
      def stateBit(state: Int, net: Int): Boolean = ((state >> memberPos(net)) & 1) == 1

      private def valueOf(net: Int, state: Int, assignment: Int): Boolean =
        if (net == 0) false
        else if (net == 1) true
        else {
          val mp = memberPos.get(net)
          if (mp.isDefined) ((state >> mp.get) & 1) == 1
          else ((assignment >> inputPos(net)) & 1) == 1
        }

      /** One unit-delay step of every member net under the given state and input assignment, as a bitmask over member
        * positions; -1 when some member net is left undriven or driven to both values (a floating or contended bus
        * net), in which case the candidate state is not a fixed point.
        */
      def step(state: Int, assignment: Int): Int = {
        var next = 0
        var p = 0
        while (p < members.size) {
          val drivers = netlist.driversOf(members(p))
          var driven = -1 // -1 undriven, 0/1 driven value, 2 contended
          var d = 0
          while (d < drivers.length && driven != 2) {
            val ref = drivers(d)
            def drive(v: Boolean): Unit = {
              val vi = if (v) 1 else 0
              driven = if (driven < 0 || driven == vi) vi else 2
            }
            Netlist.refKind(ref) match {
              case Netlist.NandKind =>
                val i = Netlist.refIndex(ref)
                drive(
                  !(valueOf(netlist.nandIn1(i), state, assignment) &&
                    valueOf(netlist.nandIn2(i), state, assignment))
                )
              case Netlist.SwitchKind =>
                val i = Netlist.refIndex(ref)
                if (valueOf(netlist.switchEn(i), state, assignment)) {
                  drive(valueOf(netlist.switchIn(i), state, assignment))
                }
              case _ => // a clock output is not a Boolean function of the region; flagged by hasClockDriver
            }
            d += 1
          }
          if (driven < 0 || driven == 2) return -1
          if (driven == 1) next |= (1 << p)
          p += 1
        }
        next
      }
    }

    /** Exhaustively characterise a region: enumerate the stable states (fixed points of the unit-delay update) of every
      * external input assignment.
      */
    private def characterise(region: Region): Characterisation = {
      if (!region.withinLimits) return UnsupportedChar
      val nAssignments = 1 << region.inputNets.size
      val nStates = 1 << region.members.size
      val fixedPoints = new Array[List[Int]](nAssignments)
      var anyMany = false
      var a = 0
      while (a < nAssignments) {
        var fps = List.empty[Int]
        var count = 0
        var s = 0
        while (s < nStates && count <= 2) {
          if (region.step(s, a) == s) {
            count += 1
            fps = s :: fps
          }
          s += 1
        }
        if (count == 0) return OscillatorChar // some input assignment has no stable state
        if (count > 2) anyMany = true
        fixedPoints(a) = fps.reverse
        a += 1
      }
      if (anyMany) return UnsupportedChar // more than one bit of state
      if (!fixedPoints.exists(_.size == 2)) return UnsupportedChar // no assignment stores anything
      // The region is a bistable when some member net carries the state bit: under every two-state assignment the
      // two stable states differ on it, so it can serve as the representative net. Output nets are preferred; a
      // region at the design boundary may have none (its outputs are read by nothing), in which case any member
      // net carrying the bit does.
      val repCandidates = region.outputNets ++ region.members.filterNot(region.outputNets.contains)
      val rep = repCandidates.find { out =>
        fixedPoints.forall { fps =>
          fps.size != 2 || region.stateBit(fps.head, out) != region.stateBit(fps(1), out)
        }
      }
      rep match {
        case None => UnsupportedChar
        case Some(stateNet) =>
          val values = new Array[Boolean](nAssignments * 2)
          val stateBitPos = 1 << region.inputNets.size
          var a = 0
          while (a < nAssignments) {
            val fps = fixedPoints(a)
            if (fps.size == 1) {
              val bit = region.stateBit(fps.head, stateNet)
              values(a) = bit
              values(a | stateBitPos) = bit
            } else {
              values(a) = false // hold: the next bit is the current bit
              values(a | stateBitPos) = true
            }
            a += 1
          }
          BistableChar(StateElement.NextState(region.inputNets, stateNet, values.toVector), stateNet)
      }
    }

    /** Search one SCC for bistable cores: unions of overlapping short cycles, grown until they characterise as
      * bistables. Cores already accepted are excluded from the search space, so the cores found are disjoint.
      */
    private def findBistableCores(scc: Set[Int]): Vector[(Vector[Int], BistableChar)] = {
      val accepted = mutable.Set[Int]()
      val cores = mutable.ArrayBuffer[(Vector[Int], BistableChar)]()
      var budget = CoreSearchBudget
      val seeds = scc.toVector.sorted
      var si = 0
      while (si < seeds.size && budget > 0) {
        val seed = seeds(si)
        if (!accepted.contains(seed)) {
          val (found, spent) = searchFromSeed(seed, scc -- accepted, budget)
          budget -= spent
          found.foreach { case (core, b) =>
            cores += ((core, b))
            accepted ++= core
          }
        }
        si += 1
      }
      cores.toVector
    }

    /** Try each short cycle through `seed` (smallest first) as a core, growing it with overlapping cycles until it
      * characterises as a bistable. Returns the core found, if any, and the DFS budget spent.
      */
    private def searchFromSeed(
        seed: Int,
        allowed: Set[Int],
        budget: Int
    ): (Option[(Vector[Int], BistableChar)], Int) = {
      var remaining = budget
      val (cycles, spent) = cyclesThrough(seed, allowed, remaining)
      remaining -= spent
      val it = cycles.iterator
      while (it.hasNext && remaining > 0) {
        var candidate = it.next()
        var result = characterise(new Region(candidate))
        var growing = result match {
          case _: BistableChar => false
          case _ => true
        }
        while (growing && remaining > 0) {
          val (options, spent) = growthCycles(candidate, allowed, remaining)
          remaining -= spent
          options.find(c => (candidate.toSet ++ c).size <= MaxInternalNets) match {
            case None => growing = false
            case Some(extra) =>
              candidate = (candidate.toSet ++ extra).toVector.sorted
              result = characterise(new Region(candidate))
              growing = result match {
                case _: BistableChar => false
                case _ => true
              }
          }
        }
        result match {
          case b: BistableChar => return (Some((candidate, b)), budget - remaining)
          case _ =>
        }
      }
      (None, budget - remaining)
    }

    /** Short cycles that overlap `candidate` and add at least one net to it, smallest first. */
    private def growthCycles(
        candidate: Vector[Int],
        allowed: Set[Int],
        budget: Int
    ): (Vector[Vector[Int]], Int) = {
      var remaining = budget
      val found = mutable.LinkedHashMap[Set[Int], Vector[Int]]()
      val candidateSet = candidate.toSet
      candidate.foreach { net =>
        if (remaining > 0) {
          val (cycles, spent) = cyclesThrough(net, allowed, remaining)
          remaining -= spent
          cycles.foreach { cycle =>
            val set = cycle.toSet
            if (set.intersect(candidateSet).nonEmpty && !set.subsetOf(candidateSet)) {
              found(set) = cycle
            }
          }
        }
      }
      (found.values.toVector.sortBy(c => (c.size, c.mkString(" "))), budget - remaining)
    }

    /** Elementary cycles through `seed` within `allowed`, of at most [[MaxCycleNets]] nets, smallest first. The search
      * depth is bounded by that constant, so an explicit-stack DFS never grows with the graph; `budget` bounds the
      * total expansions.
      */
    private def cyclesThrough(seed: Int, allowed: Set[Int], budget: Int): (Vector[Vector[Int]], Int) = {
      val found = mutable.LinkedHashMap[Set[Int], Vector[Int]]()
      var spent = 0
      if (allowed.contains(seed)) {
        if (succ(seed).contains(seed)) found(Set(seed)) = Vector(seed)
        var len = 2
        while (len <= MaxCycleNets && spent < budget && found.size < MaxCyclesPerSeed) {
          spent += enumerateCycles(seed, allowed, len, budget - spent, found)
          len += 1
        }
      }
      (found.values.toVector.sortBy(c => (c.size, c.mkString(" "))), spent)
    }

    /** Enumerate the elementary cycles of exactly `len` nets through `seed`, adding their net sets to `found`. Returns
      * the number of DFS expansions used.
      */
    private def enumerateCycles(
        seed: Int,
        allowed: Set[Int],
        len: Int,
        budget: Int,
        found: mutable.Map[Set[Int], Vector[Int]]
    ): Int = {
      var spent = 0
      val path = mutable.ArrayDeque[Int](seed)
      val inPath = mutable.Set[Int](seed)
      // Explicit stack of (net, next successor index) frames; depth never exceeds MaxCycleNets.
      val stack = mutable.ArrayDeque[(Int, Int)]((seed, 0))
      while (stack.nonEmpty && spent < budget && found.size < MaxCyclesPerSeed) {
        val (v, ci) = stack.last
        val children = succ(v)
        if (ci < children.length) {
          stack(stack.size - 1) = (v, ci + 1)
          val w = children(ci)
          spent += 1
          if (w == seed && path.size == len) {
            found(path.toSet) = path.toVector.sorted
          } else if (w != seed && path.size < len && allowed.contains(w) && !inPath.contains(w)) {
            path += w
            inPath += w
            stack += ((w, 0))
          }
        } else {
          stack.removeLast()
          path.removeLast()
          inPath -= v
        }
      }
      spent
    }
  }
}
