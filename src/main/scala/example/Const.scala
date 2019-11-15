package example

import fp._

/**
 * The constant functor.
 */
sealed case class Const[T, B](value: T)

object Const {
  implicit def instance[T: Monoid]: Applicative[Const[T, ?]] = new Applicative[Const[T, ?]] {
    private val T: Monoid[T] = implicitly[Monoid[T]]

    override def fmap[A, B](ff: Const[T, A], f: A => B): Const[T, B] =
      ff match { case Const(a) => Const(a) }

    // To implement [[Applicative.pure]] we need a way to magic up an initial [[T]] value.
    override def pure[A](a: A): Const[T, A] = Const(T.unit)

    // The "effect" of the [[Const]] functor is the constant value being carried around.
    // When we combine computations with [[ap]] we need to combine the two effects as well.
    override def ap[A, B](ff: Const[T, A => B], fa: Const[T, A]): Const[T, B] = 
      Const(T.plus(ff.value, fa.value))
  }
 
  private def const[A, B] : A => B => A = (a:A) => (b:B) => a
}