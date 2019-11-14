package fp

trait Monad[F[_]] extends Selective[F] {
  def flatMap[A,B](
    fa: F[A],
    f: A => F[B]
  ): F[B]
}

object Monad {
  def apply[G[_] : Monad]: Monad[G] = implicitly[Monad[G]]
}
