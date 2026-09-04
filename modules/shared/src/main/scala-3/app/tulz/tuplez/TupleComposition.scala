package app.tulz.tuplez

import scala.util.NotGiven

object TupleComposition {

  def compose[L, R](l: L, r: R)(using composition: Compose[L, R]): composition.Composed = composition.compose(l, r)
  def decompose[L, R, C](c: C)(using composition: Decompose.Aux[L, R, C]): (L, R)       = composition.decompose(c)

}

trait Compose[-L, -R] {
  type Composed
  def compose(a: L, b: R): Composed
}


trait Decompose[L, R] {
  type Composed
  def decompose(c: Composed): (L, R)
}

abstract class Composition[L, R] extends Compose[L, R], Decompose[L, R] {
  type Composed
}

object Composition {

  type Aux[A, B, O] = Composition[A, B] { type Composed = O }

  given implied[A, B, O](using c: Compose.Aux[A, B, O], d: Decompose.Aux[A, B, O]): Composition.Aux[A, B, O] =
    new Composition[A, B] {
      override type Composed = O

      def compose(l: A, r: B): O = c.compose(l, r)
      def decompose(c: O): (A, B) = d.decompose(c)
    }

}

// ---- compose givens

trait Compose_Pri0 {
  given `***`[A, B]: Compose.Aux[A, B, Tuple2[A, B]] = new Compose[A, B] {

    override type Composed = Tuple2[A, B]

    def compose(l: A, r: B): Tuple2[A, B] =
      Tuple2(l, r)

  }
}

trait Compose_Pri10 extends Compose_Pri0 {

  given `T+Scalar`[T1 <: Tuple, T2]: Compose.Aux[T1, T2, Tuple.Append[T1, T2]] = new Compose[T1, T2] {

    override type Composed = Tuple.Append[T1, T2]

    def compose(l: T1, r: T2): Tuple.Append[T1, T2] =
      l :* r

  }

  given `Scalar+T`[T1, T2 <: Tuple]: Compose.Aux[T1, T2, T1 *: T2] = new Compose[T1, T2] {

    override type Composed = T1 *: T2

    def compose(l: T1, r: T2): T1 *: T2 =
      l *: r

  }

}

object Compose extends Compose_Pri10 {

  type Aux[A, B, O] = Compose[A, B] { type Composed = O }

  given `unit+unit`: Compose.Aux[Unit, Unit, Unit] = new Compose[Unit, Unit] {

    override type Composed = Unit

    def compose(l: Unit, r: Unit): Unit =
      ()

  }

  given `unit+A`[A]: Compose.Aux[Unit, A, A] = new Compose[Unit, A] {

    override type Composed = A

    def compose(l: Unit, r: A): A =
      r

  }

  given `A+unit`[A]: Compose.Aux[A, Unit, A] = new Compose[A, Unit] {

    override type Composed = A

    def compose(l: A, r: Unit): A =
      l

  }

  // Structural per-left-arity flatten: `*:` cons types (not match types) reduce in the
  // pattern-match reachability checker, and being flat givens they keep implicit search
  // O(1) (no nested/recursive implicits). Covers every left arity representable in a <= 22-tuple.
  given `T1+T`[L1, R <: Tuple]: Compose.Aux[Tuple1[L1], R, L1 *: R] = new Compose[Tuple1[L1], R] {
    override type Composed = L1 *: R
    def compose(l: Tuple1[L1], r: R): L1 *: R = l._1 *: r
  }

  given `T2+T`[L1, L2, R <: Tuple]: Compose.Aux[(L1, L2), R, L1 *: L2 *: R] = new Compose[(L1, L2), R] {
    override type Composed = L1 *: L2 *: R
    def compose(l: (L1, L2), r: R): L1 *: L2 *: R = l._1 *: l._2 *: r
  }

  given `T3+T`[L1, L2, L3, R <: Tuple]: Compose.Aux[(L1, L2, L3), R, L1 *: L2 *: L3 *: R] = new Compose[(L1, L2, L3), R] {
    override type Composed = L1 *: L2 *: L3 *: R
    def compose(l: (L1, L2, L3), r: R): L1 *: L2 *: L3 *: R = l._1 *: l._2 *: l._3 *: r
  }

  given `T4+T`[L1, L2, L3, L4, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4), R, L1 *: L2 *: L3 *: L4 *: R] = new Compose[(L1, L2, L3, L4), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: R
    def compose(l: (L1, L2, L3, L4), r: R): L1 *: L2 *: L3 *: L4 *: R = l._1 *: l._2 *: l._3 *: l._4 *: r
  }

  given `T5+T`[L1, L2, L3, L4, L5, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5), R, L1 *: L2 *: L3 *: L4 *: L5 *: R] = new Compose[(L1, L2, L3, L4, L5), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: R
    def compose(l: (L1, L2, L3, L4, L5), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: r
  }

  given `T6+T`[L1, L2, L3, L4, L5, L6, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: R] = new Compose[(L1, L2, L3, L4, L5, L6), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: r
  }

  given `T7+T`[L1, L2, L3, L4, L5, L6, L7, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: r
  }

  given `T8+T`[L1, L2, L3, L4, L5, L6, L7, L8, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: r
  }

  given `T9+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: r
  }

  given `T10+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: r
  }

  given `T11+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: r
  }

  given `T12+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: r
  }

  given `T13+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: r
  }

  given `T14+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: l._14 *: r
  }

  given `T15+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: l._14 *: l._15 *: r
  }

  given `T16+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: l._14 *: l._15 *: l._16 *: r
  }

  given `T17+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: l._14 *: l._15 *: l._16 *: l._17 *: r
  }

  given `T18+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: l._14 *: l._15 *: l._16 *: l._17 *: l._18 *: r
  }

  given `T19+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: l._14 *: l._15 *: l._16 *: l._17 *: l._18 *: l._19 *: r
  }

  given `T20+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: l._14 *: l._15 *: l._16 *: l._17 *: l._18 *: l._19 *: l._20 *: r
  }

  given `T21+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21, R <: Tuple]: Compose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: L21 *: R] = new Compose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: L21 *: R
    def compose(l: (L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21), r: R): L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: L21 *: R = l._1 *: l._2 *: l._3 *: l._4 *: l._5 *: l._6 *: l._7 *: l._8 *: l._9 *: l._10 *: l._11 *: l._12 *: l._13 *: l._14 *: l._15 *: l._16 *: l._17 *: l._18 *: l._19 *: l._20 *: l._21 *: r
  }

}

// ---- decompose givens

trait Decompose_Pri0 {
  given `***`[A, B]: Decompose.Aux[A, B, Tuple2[A, B]] = new Decompose[A, B] {

    override type Composed = Tuple2[A, B]

    def decompose(c: Tuple2[A, B]): (A, B) =
      c

  }
}

trait Decompose_Pri10 extends Decompose_Pri0 {


  given `T+Scalar`[T1 <: Tuple, T2](using NotGiven[T2 <:< Tuple]): Decompose.Aux[T1, T2, Tuple.Append[T1, T2]] = new Decompose[T1, T2] {

    override type Composed = Tuple.Append[T1, T2]

    def decompose(c: Tuple.Append[T1, T2]): (T1, T2) = {
      val left = c.init.asInstanceOf[T1]
      val right = c.last.asInstanceOf[T2] // c.drop(size1).productElement(0).asInstanceOf[T2] // Get the single element
      (left, right)
    }

  }

  given `Scalar+T`[T1, T2 <: Tuple](using NotGiven[T1 <:< Tuple]): Decompose.Aux[T1, T2, T1 *: T2] = new Decompose[T1, T2] {

    override type Composed = T1 *: T2

    def decompose(c: T1 *: T2): (T1, T2) = {
      val left = c.head
      val right = c.tail
      (left, right)
    }

  }

  // Structural counterparts of Compose's Tk+T, so Composition.implied unifies Composed syntactically.
  given `T1+T`[L1, R <: Tuple]: Decompose.Aux[Tuple1[L1], R, L1 *: R] = new Decompose[Tuple1[L1], R] {
    override type Composed = L1 *: R
    def decompose(c: L1 *: R): (Tuple1[L1], R) = {
      val (left, right) = c.splitAt(1)
      (left.asInstanceOf[Tuple1[L1]], right.asInstanceOf[R])
    }
  }

  given `T2+T`[L1, L2, R <: Tuple]: Decompose.Aux[(L1, L2), R, L1 *: L2 *: R] = new Decompose[(L1, L2), R] {
    override type Composed = L1 *: L2 *: R
    def decompose(c: L1 *: L2 *: R): ((L1, L2), R) = {
      val (left, right) = c.splitAt(2)
      (left.asInstanceOf[(L1, L2)], right.asInstanceOf[R])
    }
  }

  given `T3+T`[L1, L2, L3, R <: Tuple]: Decompose.Aux[(L1, L2, L3), R, L1 *: L2 *: L3 *: R] = new Decompose[(L1, L2, L3), R] {
    override type Composed = L1 *: L2 *: L3 *: R
    def decompose(c: L1 *: L2 *: L3 *: R): ((L1, L2, L3), R) = {
      val (left, right) = c.splitAt(3)
      (left.asInstanceOf[(L1, L2, L3)], right.asInstanceOf[R])
    }
  }

  given `T4+T`[L1, L2, L3, L4, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4), R, L1 *: L2 *: L3 *: L4 *: R] = new Decompose[(L1, L2, L3, L4), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: R): ((L1, L2, L3, L4), R) = {
      val (left, right) = c.splitAt(4)
      (left.asInstanceOf[(L1, L2, L3, L4)], right.asInstanceOf[R])
    }
  }

  given `T5+T`[L1, L2, L3, L4, L5, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5), R, L1 *: L2 *: L3 *: L4 *: L5 *: R] = new Decompose[(L1, L2, L3, L4, L5), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: R): ((L1, L2, L3, L4, L5), R) = {
      val (left, right) = c.splitAt(5)
      (left.asInstanceOf[(L1, L2, L3, L4, L5)], right.asInstanceOf[R])
    }
  }

  given `T6+T`[L1, L2, L3, L4, L5, L6, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: R): ((L1, L2, L3, L4, L5, L6), R) = {
      val (left, right) = c.splitAt(6)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6)], right.asInstanceOf[R])
    }
  }

  given `T7+T`[L1, L2, L3, L4, L5, L6, L7, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: R): ((L1, L2, L3, L4, L5, L6, L7), R) = {
      val (left, right) = c.splitAt(7)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7)], right.asInstanceOf[R])
    }
  }

  given `T8+T`[L1, L2, L3, L4, L5, L6, L7, L8, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8), R) = {
      val (left, right) = c.splitAt(8)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8)], right.asInstanceOf[R])
    }
  }

  given `T9+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9), R) = {
      val (left, right) = c.splitAt(9)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9)], right.asInstanceOf[R])
    }
  }

  given `T10+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10), R) = {
      val (left, right) = c.splitAt(10)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10)], right.asInstanceOf[R])
    }
  }

  given `T11+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11), R) = {
      val (left, right) = c.splitAt(11)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11)], right.asInstanceOf[R])
    }
  }

  given `T12+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12), R) = {
      val (left, right) = c.splitAt(12)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12)], right.asInstanceOf[R])
    }
  }

  given `T13+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13), R) = {
      val (left, right) = c.splitAt(13)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13)], right.asInstanceOf[R])
    }
  }

  given `T14+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14), R) = {
      val (left, right) = c.splitAt(14)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14)], right.asInstanceOf[R])
    }
  }

  given `T15+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15), R) = {
      val (left, right) = c.splitAt(15)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15)], right.asInstanceOf[R])
    }
  }

  given `T16+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16), R) = {
      val (left, right) = c.splitAt(16)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16)], right.asInstanceOf[R])
    }
  }

  given `T17+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17), R) = {
      val (left, right) = c.splitAt(17)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17)], right.asInstanceOf[R])
    }
  }

  given `T18+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18), R) = {
      val (left, right) = c.splitAt(18)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18)], right.asInstanceOf[R])
    }
  }

  given `T19+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19), R) = {
      val (left, right) = c.splitAt(19)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19)], right.asInstanceOf[R])
    }
  }

  given `T20+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20), R) = {
      val (left, right) = c.splitAt(20)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20)], right.asInstanceOf[R])
    }
  }

  given `T21+T`[L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21, R <: Tuple]: Decompose.Aux[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21), R, L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: L21 *: R] = new Decompose[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21), R] {
    override type Composed = L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: L21 *: R
    def decompose(c: L1 *: L2 *: L3 *: L4 *: L5 *: L6 *: L7 *: L8 *: L9 *: L10 *: L11 *: L12 *: L13 *: L14 *: L15 *: L16 *: L17 *: L18 *: L19 *: L20 *: L21 *: R): ((L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21), R) = {
      val (left, right) = c.splitAt(21)
      (left.asInstanceOf[(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L20, L21)], right.asInstanceOf[R])
    }
  }

}

object Decompose extends Decompose_Pri10 {

  type Aux[A, B, O] = Decompose[A, B] { type Composed = O }

  given `unit+unit`: Decompose.Aux[Unit, Unit, Unit] = new Decompose[Unit, Unit] {

    override type Composed = Unit

    def decompose(c: Unit): (Unit, Unit) =
      ((), ())

  }

  given `unit+A`[A]: Decompose.Aux[Unit, A, A] = new Decompose[Unit, A] {

    override type Composed = A

    def decompose(c: A): (Unit, A) =
      ((), c)

  }

  given `A+unit`[A]: Decompose.Aux[A, Unit, A] = new Decompose[A, Unit] {

    override type Composed = A

    def decompose(c: A): (A, Unit) =
      (c, ())

  }

}
