package fp

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  
  def ap[A,B](
    ff: F[A => B],
    fa: F[A]
  ): F[B]
}

object Applicative {
  def apply[G[_] : Applicative]: Applicative[G] = implicitly[Applicative[G]]
}
