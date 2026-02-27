import sbt._

import java.io.File

class TupleCompositionTestGenerator(sourceManaged: File, to: Int, testConcats: Boolean, testPrepends: Boolean)
    extends SourceGenerator(
      sourceManaged / "app" / "tulz" / "tuplez" / "TupleCompositionTests.scala"
    ) {

  def doGenerate(): Unit = {
    println("""package app.tulz.tuplez""")
    println()
    println("""import org.junit.Test""")
    println("""import org.junit.Assert._""")
    println()
    enter("""class TupleCompositionTests {""")("}") {
      println()

      println("""private val unit: Unit = (): Unit""")
      println()

      enter("""@Test def `Unit+Unit`(): Unit = {""")("}") {
        println("""assertEquals("composed should match", unit, TupleComposition.compose(unit, unit))""")
        println("""assertEquals("decomposed should match", (unit, unit), Decompose[Unit, Unit].decompose(unit))""")
      }

      println()

      enter("""@Test def `scalar+scalar`(): Unit = {""")("}") {
        println("""assertEquals("composed should match", ("1", "2"), TupleComposition.compose("1", "2"))""")
        println("""assertEquals("decomposed should match", ("1", "2"), Decompose[String, String].decompose(Tuple2("1", "2")))""")
      }

      println()

      enter("""@Test def `scalar+unit`(): Unit = {""")("}") {
        println("""assertEquals("composed should match", "1", TupleComposition.compose("1", unit))""")
        println("""assertEquals("decomposed should match", ("1", unit), Decompose[String, Unit].decompose("1"))""")
      }

      println()

      enter("""@Test def `unit+scalar`(): Unit = {""")("}") {
        println("""assertEquals("composed should match", "2", TupleComposition.compose(unit, "2"))""")
        println("""assertEquals("decomposed should match", (unit, "2"), Decompose[Unit, String].decompose("2"))""")
      }

      println()

      def tupleElements(size: Int, offset: Int): String = {
        if (size == 1) {
          s"${offset + 1}"
        } else {
          s"${(offset + 1 to offset + size).mkString(", ")}"
        }
      }

      def tupleValue(size: Int, offset: Int): String = {
        if (size == 1) {
          s"Tuple1(${tupleElements(size, offset)})"
        } else {
          s"(${tupleElements(size, offset)})"
        }
      }

      def tupleType(size: Int): String = {
        if (size == 1) {
          s"Tuple1[Int]"
        } else {
          s"(${(1 to size).map(_ => "Int").mkString(", ")})"
        }
      }

      for (size1 <- 1 until to) {
        enter(s"""@Test def `${size1}-tuple+Unit`(): Unit = {""")("}") {
          println(s"""val tuple: ${tupleType(size1)} = ${tupleValue(size1, 100)}""")
          println(s"""assertEquals("composed should match", tuple, TupleComposition.compose(tuple, (): Unit))""")
          println(s"""assertEquals("decomposed should match", (tuple, unit), Decompose[${tupleType(size1)}, Unit].decompose(tuple))""")
        }
        println()
      }

      for (size1 <- 1 until to) {
        enter(s"""@Test def `${size1}-Unit+tuple`(): Unit = {""")("}") {
          println(s"""val tuple: ${tupleType(size1)} = ${tupleValue(size1, 100)}""")
          println(s"""assertEquals("composed should match", tuple, TupleComposition.compose((): Unit, tuple))""")
          println(s"""assertEquals("decomposed should match", (unit, tuple), Decompose[Unit, ${tupleType(size1)}].decompose(tuple))""")
        }
        println()
      }

      for (size1 <- 1 until to) {
        enter(s"""@Test def `${size1}-tuple+scalar`(): Unit = {""")("}") {
          println(s"""val _ = Compose[${tupleType(size1)}, Int]""")
          println(s"""val _ = Decompose[${tupleType(size1)}, Int]""")
          println(s"""val _ = Composition[${tupleType(size1)}, Int]""")
          println(s"""val tuple: ${tupleType(size1)} = ${tupleValue(size1, 100)}""")
          println(s"""val expected: ${tupleType(size1+1)} = (${tupleElements(size1, 100)}, 201)""")
          println(s"""assertEquals("composed should match", expected, TupleComposition.compose(tuple, 201))""")
          println(s"""assertEquals("decomposed should match", (tuple, 201), Decompose[${tupleType(size1)}, Int].decompose(expected))""")
        }
        println()
      }

      if (testPrepends) {
        for (size1 <- 1 until to) {
          enter(s"""@Test def `scalar+${size1}-tuple`(): Unit = {""")("}") {
            println(s"""val tuple = ${tupleValue(size1, 100)}""")
            println(s"""val expected = (201, ${tupleElements(size1, 100)})""")
            println(s"""assertEquals("composed should match", expected, TupleComposition.compose(201, tuple))""")
            println(s"""assertEquals("decomposed should match", (201, tuple), Decompose[Int, ${tupleType(size1)}].decompose(expected))""")
          }
          println()
        }
      }

      if (testConcats) {
        for (size1 <- 1 until to) {
          for (size2 <- 1 to to - size1) {
            enter(s"""@Test def `${size1}-tuple+${size2}-tuple`(): Unit = {""".stripMargin)("}") {
              println(s"""val tuple1 = ${tupleValue(size1, 100)}""")
              println(s"""val tuple2 = ${tupleValue(size2, 200)}""")
              println(s"""val expected = (${tupleElements(size1, 100)}, ${tupleElements(size2, 200)})""")
              println(s"""assertEquals("composed should match", expected, TupleComposition.compose(tuple1, tuple2))""")
              println(s"""assertEquals("decomposed should match", (tuple1, tuple2), Decompose[${tupleType(size1)}, ${tupleType(size2)}].decompose(expected))""")
            }
            println()
          }
        }
      }

    }
  }

}
