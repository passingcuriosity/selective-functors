package example

import fp._

/** The "under-estimate" functor
 *
 * Like [[Const]]: Under[T,A] contains a description of an effect. The instances below will SKIP
 * every effect we can. This under estimates the effects that can happen in a program.
 */
final case class Under[T, A](value: T)

object Under {
  def fromConst[T]: NT[Const[T, ?], Under[T, ?]] = new NT[Const[T, ?], Under[T, ?]] {
    def apply[A](k: Const[T, A]): Under[T, A] =
      Under(k.value)
  }

  implicit def instance[T: Monoid]: Selective[Under[T, ?]] =  new Selective[Under[T, ?]] {
    val T = Monoid[T]

    override def fmap[A,B](fa: Under[T, A], f: A => B): Under[T, B] =
      fa match { case Under(x) => Under(x) }
    
    override def pure[A](a: A): Under[T, A] =
      Under(T.unit)

    override def ap[A,B](ff: Under[T, A => B], fa: Under[T, A]): Under[T, B] =
      (ff, fa) match {
        case (Under(x), Under(y)) => Under(T.plus(x, y))
      }

    override def select[A,B](fe: Under[T, Either[A,B]], ff: Under[T, A => B]) =
      (fe, ff) match {
        case (Under(x), _) => Under(x)
      }
  }
}
