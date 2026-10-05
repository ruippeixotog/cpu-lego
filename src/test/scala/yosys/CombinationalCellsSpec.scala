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

  /** Imports the cell with the given constant inputs and runs the simulation, returning the settled output.
    */
  def simulate(cell: CellUnderTest, values: Map[String, Boolean]): Option[Boolean] = {
    val design = designFor(cell.cellType, cell.inputs, cell.output)
    val (y, sim) = buildAndRun {
      val ins: Map[String, Port | Bus] =
        cell.inputs.map(n => n -> ((if (values(n)) High else Low): Port | Bus)).toMap
      ComponentCreator(design).create("dut", ins)(cell.output).asInstanceOf[Port]
    }
    sim.get(y)
  }

  /** Input combinations to check. Fully exhaustive, except for the wide muxes (`$_MUX8_` has 2^11 and `$_MUX16_` 2^20
    * combinations — infeasible to run one simulation each). A mux output depends only on the select lines and the
    * selected data input, so for those two we exhaust the selects and toggle the selected input against all-low /
    * all-high data patterns, which still catches any select/data wiring mistake.
    */
  def cases(cell: CellUnderTest): List[Map[String, Boolean]] = {
    def allCombos(names: List[String]): List[Map[String, Boolean]] =
      (0 until (1 << names.length)).map { i =>
        names.zipWithIndex.map { case (n, j) => n -> ((i >> j & 1) == 1) }.toMap
      }.toList

    cell.cellType match {
      case t if t == "$_MUX8_" || t == "$_MUX16_" =>
        val nSel = if (t == "$_MUX8_") 3 else 4
        val (data, selects) = cell.inputs.splitAt(cell.inputs.length - nSel)
        for {
          sel <- (0 until (1 << nSel)).toList
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
        cases(cell).foreach { values =>
          val expected = cell.model(cell.inputs.map(values))
          (simulate(cell, values) aka s"inputs $values") must beSome(expected)
        }
      }
    }
  }

  "an unsupported cell" should {
    "fail with the cell type, cell name and src attribute" in {
      val design = designFor("$_UNSUPPORTED_", List("A"), "Y", Map("src" -> "test.v:42"))
      val outcome =
        try {
          buildAndRun {
            ComponentCreator(design).create("dut", Map("A" -> Low))
          }
          None
        } catch {
          case e: IllegalArgumentException => Some(e.getMessage)
        }
      outcome must beSome
      outcome.get must contain("$_UNSUPPORTED_")
      outcome.get must contain("cell0")
      outcome.get must contain("test.v:42")
    }
  }
}
