import sbt._

import java.io.File

class TupleCompositionGenerator(sourceManaged: File, to: Int, splitPriorityAt: Int, generateConcats: Boolean, generatePrepends: Boolean)
    extends SourceGenerator(
      sourceManaged / "app" / "tulz" / "tuplez" / "TupleComposition.scala"
    ) {

  private def newComposition(
    name: String,
    typeParams: String,
    L: String,
    R: String,
    O: String,
    compose: String,
  ): Unit = {
    enter(s"""implicit def `$name`${if (typeParams.nonEmpty) s"[$typeParams]" else ""}: Composition.Aux[$L, $R, $O] = new Composition[$L, $R] {""")("}") {
      println()
      println(s"override type Composed = ${O}")
      println()
      enter(s"def compose(l: $L, r: $R): $O =")("") {
        println(compose)
      }
    }
  }

  def doGenerate(): Unit = {
    println("""package app.tulz.tuplez""")
    println()
    enter("""object TupleComposition {""")("}") {
      println()
      println("""def compose[L, R](l: L, r: R)(implicit composition: Composition[L, R]): composition.Composed = composition.compose(l, r)""")
      println()
    }
    println()
    enter("""abstract class Composition[-L, -R] {""")("}") {
      println("""type Composed""")
      println("""def compose(a: L, b: R): Composed""")
    }
    println()
    enter("""trait Composition_Pri0 {""")("}") {
      newComposition(
        name = "***",
        typeParams = "A, B",
        L = "A",
        R = "B",
        O = "Tuple2[A, B]",
        compose = "Tuple2(l, r)",
      )
    }
    println()

    enter("""trait Composition_Pri5 extends Composition_Pri0{""")("}") {
      newComposition(
        name = "T1+R",
        typeParams = "L, R",
        L = "Tuple1[L]",
        R = "R",
        O = "Tuple2[L, R]",
        compose = "Tuple2(l._1, r)",
      )

      newComposition(
        name = "L+T1",
        typeParams = "L, R",
        L = "L",
        R = "Tuple1[R]",
        O = "Tuple2[L, R]",
        compose = "Tuple2(l, r._1)",
      )
    }

    println()

    generatePri7()

    println()

    generatePri10()

    println()

    enter("""object Composition extends Composition_Pri10 {""")("}") {
      println("""type Aux[A, B, O] = Composition[A, B] { type Composed = O }""")
      println()

      newComposition(
        name = "unit+unit",
        typeParams = "",
        L = "Unit",
        R = "Unit",
        O = "Unit",
        compose = "()",
      )

      println()
    }
  }

  def generateSizeAndScalar(minArity: Int, maxArity: Int, priority: Int, extendsPriority: Int): Unit = {
    def forSizeAndScalar(size: Int): Unit = {
      val left = tupleType(size - 1)
      newComposition(
        name = s"T${size - 1}+scalar",
        typeParams = s"${left}, R",
        L = s"(${left})",
        R = s"R",
        O = s"(${left}, R)",
        compose = s"(${tupleAccess(size - 1, "l")}, r)",
      )
    }

    def forScalarAndSize(size: Int): Unit = {
      val right = tupleType(size - 1)
      newComposition(
        name = s"scalar+T${size - 1}",
        typeParams = s"L, ${right}",
        L = s"L",
        R = s"(${right})",
        O = s"(L, ${right})",
        compose = s"(l, ${tupleAccess(size - 1, "r")})",
      )
    }

    enter(s"""trait Composition_Pri${priority} extends Composition_Pri${extendsPriority} {""")("}") {
      println()
      for (size <- 3 to to) {
        if (size >= minArity && size <= maxArity) {
          forSizeAndScalar(size)
          if (generatePrepends) {
            forScalarAndSize(size)
          }
        }
      }
    }
  }

  def generatePri7(): Unit = {
    if (splitPriorityAt < to) {
      generateSizeAndScalar(minArity = splitPriorityAt + 1, maxArity = Int.MaxValue, priority = 6, extendsPriority = 5)
      println()
      generateSizeAndScalar(minArity = 1, maxArity = splitPriorityAt, priority = 7, extendsPriority = 6)
    } else {
      generateSizeAndScalar(minArity = 1, maxArity = Int.MaxValue, priority = 7, extendsPriority = 5)
    }
  }

  def generateHighPriority(minArity: Int, maxArity: Int, priority: Int, extendsPriority: Int): Unit = {

    def forSizeAnd1(size: Int): Unit = {
      val left = tupleType(size)
      newComposition(
        name = s"T${size}+T1",
        typeParams = s"${left}, R",
        L = s"(${left})",
        R = s"Tuple1[R]",
        O = s"(${left}, R)",
        compose = s"(${tupleAccess(size, "l")}, r._1)",
      )
    }

    def for1AndSize(size: Int): Unit = {
      val right = tupleType(size)
      newComposition(
        name = s"T1+T${size}",
        typeParams = s"L, ${right}",
        L = s"Tuple1[L]",
        R = s"(${right})",
        O = s"(L, ${right})",
        compose = s"(l._1, ${tupleAccess(size, "r")})",
      )
    }

    def forSizes(size1: Int, size2: Int): Unit = {
      val left  = tupleType(size1, "L")
      val right = tupleType(size2, "R")
      newComposition(
        name = s"T${size1}+T${size2}",
        typeParams = s"${left}, ${right}",
        L = s"(${left})",
        R = s"(${right})",
        O = s"(${left}, ${right})",
        compose = s"(${tupleAccess(size1, "l")}, ${tupleAccess(size2, "r")})",
      )
    }

    enter(s"""trait Composition_Pri${priority} extends Composition_Pri${extendsPriority} {""")("}") {
      println()
      if (minArity <= 2) {
        newComposition(
          name = s"T1+T1",
          typeParams = s"L, R",
          L = s"Tuple1[L]",
          R = s"Tuple1[R]",
          O = s"Tuple2[L, R]",
          compose = s"(l._1, r._1)",
        )
        println()
      }

      for (size <- 2 until to) {
        if (size + 1 >= minArity && size + 1 <= maxArity) {
          forSizeAnd1(size)
          for1AndSize(size)
        }
      }

      if (generateConcats) {
        for (size1 <- 2 to to - 2) {
          for (size2 <- 2 to to - size1) {
            if (size1 + size2 >= minArity && size1 + size2 <= maxArity) {
              forSizes(size1, size2)
            }
          }
        }
      }

      if (minArity == 1) {
        newComposition(
          name = s"unit+A",
          typeParams = s"A",
          L = s"Unit",
          R = s"A",
          O = s"A",
          compose = s"r",
        )

        newComposition(
          name = s"A+unit",
          typeParams = s"A",
          L = s"A",
          R = s"Unit",
          O = s"A",
          compose = s"l",
        )

        println()
      }
    }
  }

  def generatePri10(): Unit = {
    if (splitPriorityAt < to) {
      generateHighPriority(minArity = splitPriorityAt + 1, maxArity = Int.MaxValue, priority = 9, extendsPriority = 7)
      println()
      generateHighPriority(minArity = 1, maxArity = splitPriorityAt, priority = 10, extendsPriority = 9)
    } else {
      generateHighPriority(minArity = 1, maxArity = Int.MaxValue, priority = 10, extendsPriority = 7)
    }
  }

}
