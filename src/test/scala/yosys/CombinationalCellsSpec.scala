package yosys

import component.BuilderAPI.*
import core.*
import simulator.GateProcessor
import testkit.*
import yosys.Design.Direction as DesignDirection

/** Exhaustive truth-table tests for every yosys internal simple combinational cell mapped by [[ComponentCreator]].
  *
  * Expected values are transcribed from the `assign` statements in `techlibs/common/simcells.v` in the Yosys source
  * tree (https://github.com/YosysHQ/yosys/blob/main/techlibs/common/simcells.v).
  */
class CombinationalCellsSpec extends BaseSpec {

  /** A single cell under test: its yosys type, single-bit input port names, output port name, and the model transcribed
    * from simcells.v.
    */
  case class CellUnderTest(
      cellType: String,
      inputs: List[String],
      output: String,
      model: List[Boolean] => Boolean
  )

  val dataPorts: List[String] =
    List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P")

  /** Mux model: `selects` are S, T, U, V with S as the least significant bit, matching the simcells truth tables (e.g.
    * S=1,T=0 selects B on `$_MUX4_`).
    */
  def muxModel(selects: List[Boolean])(data: List[Boolean]): Boolean = {
    val index = selects.zipWithIndex.collect { case (true, i) => 1 << i }.sum
    data(index)
  }

  val cells = List(
    CellUnderTest("$_BUF_", List("A"), "Y", ins => ins(0)),
    CellUnderTest("$_NAND_", List("A", "B"), "Y", ins => !(ins(0) && ins(1))),
    CellUnderTest("$_NOR_", List("A", "B"), "Y", ins => !(ins(0) || ins(1))),
    CellUnderTest("$_XNOR_", List("A", "B"), "Y", ins => ins(0) == ins(1)),
    CellUnderTest("$_AOI3_", List("A", "B", "C"), "Y", ins => !((ins(0) && ins(1)) || ins(2))),
    CellUnderTest("$_OAI3_", List("A", "B", "C"), "Y", ins => !((ins(0) || ins(1)) && ins(2))),
    CellUnderTest(
      "$_AOI4_",
      List("A", "B", "C", "D"),
      "Y",
      ins => !((ins(0) && ins(1)) || (ins(2) && ins(3)))
    ),
    CellUnderTest(
      "$_OAI4_",
      List("A", "B", "C", "D"),
      "Y",
      ins => !((ins(0) || ins(1)) && (ins(2) || ins(3)))
    ),
    CellUnderTest("$_NMUX_", List("A", "B", "S"), "Y", ins => if (ins(2)) !ins(1) else !ins(0)),
    CellUnderTest("$_MUX4_", dataPorts.take(4) ++ List("S", "T"), "Y", ins => muxModel(ins.drop(4))(ins.take(4))),
    CellUnderTest(
      "$_MUX8_",
      dataPorts.take(8) ++ List("S", "T", "U"),
      "Y",
      ins => muxModel(ins.drop(8))(ins.take(8))
    ),
    CellUnderTest(
      "$_MUX16_",
      dataPorts.take(16) ++ List("S", "T", "U", "V"),
      "Y",
      ins => muxModel(ins.drop(16))(ins.take(16))
    )
  )

  /** A one-cell design: module "dut" with the given single-bit ports and a single cell "cell0" of the given type.
    */
  def designFor(
      cellType: String,
      inputs: List[String],
      output: String,
      attributes: Map[String, String] = Map.empty
  ): Design = {
    val nets = (inputs :+ output).zipWithIndex.toMap
    def bit(name: String): Vector[Int] = Vector(nets(name))
    val module = Design.Module(
      Map.empty,
      inputs.map(n => n -> Design.Port(DesignDirection.Input, bit(n))).toMap +
        (output -> Design.Port(DesignDirection.Output, bit(output))),
      Map(
        "cell0" -> Design.Cell(
          0,
          cellType,
          Map.empty,
          attributes,
          inputs.map(_ -> DesignDirection.Input).toMap + (output -> DesignDirection.Output),
          inputs.map(n => n -> bit(n)).toMap + (output -> bit(output))
        )
      ),
      Map.empty
    )
    Design("test", Map("dut" -> module))
  }

  /** Builds the cell once with free input `Port`s, returning the ports, the output port and the settled simulator.
    * Every input combination is then driven through the same `GateProcessor` instead of rebuilding the circuit.
    */
  def buildCell(cell: CellUnderTest): (Map[String, Port], Port, GateProcessor) = {
    val design = designFor(cell.cellType, cell.inputs, cell.output)
    val ((ins, y), sim) = buildAndRun {
      val ports: Map[String, Port] = cell.inputs.map(n => n -> new Port()).toMap
      val out = ComponentCreator(design).create("dut", ports)(cell.output).asInstanceOf[Port]
      (ports, out)
    }
    (ins, y, sim)
  }

  /** Input combinations to check: fully exhaustive, except `$_MUX16_` (2^20 combinations), which checks every select
    * value with the selected input toggled against all-low/all-high data.
    */
  def cases(cell: CellUnderTest): List[Map[String, Boolean]] = {
    def allCombos(names: List[String]): List[Map[String, Boolean]] =
      (0 until (1 << names.length)).map { i =>
        names.zipWithIndex.map { case (n, j) => n -> ((i >> j & 1) == 1) }.toMap
      }.toList

    cell.cellType match {
      case "$_MUX16_" =>
        val (data, selects) = cell.inputs.splitAt(cell.inputs.length - 4)
        for {
          sel <- (0 until (1 << selects.length)).toList
          selVals = selects.zipWithIndex.map { case (n, j) => n -> ((sel >> j & 1) == 1) }.toMap
          selected = data(sel)
          selectedValue <- List(false, true)
          pattern <- List(false, true)
          dataVals = data.map(n => n -> (if (n == selected) selectedValue else pattern)).toMap
        } yield selVals ++ dataVals
      case _ => allCombos(cell.inputs)
    }
  }

  cells.foreach { cell =>
    s"${cell.cellType}" should {
      "match the simcells truth table" in {
        val (ins, y, initialSim) = buildCell(cell)
        var sim = initialSim
        cases(cell).foreach { values =>
          sim = cell.inputs.foldLeft(sim) { (s, n) => s.set(ins(n), values(n)) }.run()
          val expected = cell.model(cell.inputs.map(values))
          (sim.get(y) aka s"inputs $values") must beSome(expected)
        }
      }
    }
  }

  "an unsupported cell" should {
    "fail with the cell type, cell name and src attribute" in {
      val design = designFor("$_UNSUPPORTED_", List("A"), "Y", Map("src" -> "test.v:42"))
      buildAndRun {
        ComponentCreator(design).create("dut", Map("A" -> Low))
      } must throwAn[IllegalArgumentException].like { case e =>
        e.getMessage must (contain("$_UNSUPPORTED_") and contain("cell0") and contain("test.v:42"))
      }
    }
  }
}
