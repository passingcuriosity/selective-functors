package example

import fp._

/** The "over-estimate" selective functor
 *
 * Like [[Const]]: Over[T,A] contains a T describing an effect. The instances below will RUN
 * every effect we can. This over-estimates all possible effects that can happen in a program.
 */
final case class Over[T, A](value: T)

object Over {
  def fromConst[T]: NT[Const[T, ?], Over[T, ?]] = new NT[Const[T, ?], Over[T, ?]] {
    def apply[A](k: Const[T, A]): Over[T, A] =
      Over(k.value)
  }

  implicit def instance[T: Monoid]: Selective[Over[T, ?]] =  new Selective[Over[T, ?]] {
    val T = implicitly[Monoid[T]]

    override def fmap[A,B](fa: Over[T, A], f: A => B): Over[T, B] =
      fa match { case Over(x) => Over(x) }
    
    override def pure[A](a: A): Over[T, A] =
      Over(T.unit)

    override def ap[A,B](ff: Over[T, A => B], fa: Over[T, A]): Over[T, B] =
      (ff, fa) match {
        case (Over(x), Over(y)) => Over(T.plus(x, y))
      }

    override def select[A,B](fe: Over[T, Either[A,B]], ff: Over[T, A => B]) =
      (fe, ff) match {
        case (Over(x), Over(y)) => Over(T.plus(x, y))
      }
  }
}