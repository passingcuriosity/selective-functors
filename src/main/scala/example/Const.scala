package example

import fp._

sealed case class Const[T, B](value: T)

object Const {

  implicit def instance[T](implicit T: Monoid[T]): Applicative[Const[T, ?]] = new Applicative[Const[T, ?]] {
    override def fmap[A,B](
      ff: Const[T, A],
      f: A => B
    ): Const[T, B] =
      ff match {
        case Const(a) => Const(a)
      }

    override def pure[A](a: A): Const[T, A] = Const(T.unit)

    override def ap[A,B](ff: Const[T, A => B], fa: Const[T, A]): Const[T, B] = 
      Const(T.plus(ff.value, fa.value))
  }
 
  private def const[A,B] : A => B => A = (a:A) => (b:B) => a

}