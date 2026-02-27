package app.tulz.tuplez

object TupleComposition {

  def compose[L, R](l: L, r: R)(using composition: Composition[L, R]): composition.Composed = composition.compose(l, r)

}

abstract class Composition[-L, -R] {
  type Composed
  def compose(a: L, b: R): Composed
}

trait Composition_Pri0 {
  given `***`[A, B]: Composition.Aux[A, B, Tuple2[A, B]] = new Composition[A, B] {

    override type Composed = Tuple2[A, B]

    def compose(l: A, r: B): Tuple2[A, B] =
      Tuple2(l, r)

  }
}

trait Composition_Pri10 extends Composition_Pri0 {

  given `T+Scalar`[T1 <: Tuple, T2]: Composition.Aux[T1, T2, Tuple.Append[T1, T2]] = new Composition[T1, T2] {

    override type Composed = Tuple.Append[T1, T2]

    def compose(l: T1, r: T2): Tuple.Append[T1, T2] =
      l :* r

  }

  given `Scalar+T`[T1, T2 <: Tuple]: Composition.Aux[T1, T2, Tuple.Concat[Tuple1[T1], T2]] = new Composition[T1, T2] {

    override type Composed = Tuple.Concat[Tuple1[T1], T2]

    def compose(l: T1, r: T2): Tuple.Concat[Tuple1[T1], T2] =
      l *: r

  }

}

object Composition extends Composition_Pri10 {
  type Aux[A, B, O] = Composition[A, B] { type Composed = O }

  given `unit+unit`: Composition.Aux[Unit, Unit, Unit] = new Composition[Unit, Unit] {

    override type Composed = Unit

    def compose(l: Unit, r: Unit): Unit =
      ()

  }

  given `unit+A`[A]: Composition.Aux[Unit, A, A] = new Composition[Unit, A] {

    override type Composed = A

    def compose(l: Unit, r: A): A =
      r

  }

  given `A+unit`[A]: Composition.Aux[A, Unit, A] = new Composition[A, Unit] {

    override type Composed = A

    def compose(l: A, r: Unit): A =
      l

  }

  given `T+T`[T1 <: Tuple, T2 <: Tuple]: Composition.Aux[T1, T2, Tuple.Concat[T1, T2]] = new Composition[T1, T2] {

    override type Composed = Tuple.Concat[T1, T2]

    def compose(l: T1, r: T2): Tuple.Concat[T1, T2] =
      l ++ r

  }

}
