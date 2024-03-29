package fp

trait Functor[F[_]] {
  def fmap[A,B](
    fa: F[A],
    f: A => B
  ): F[B]
}

object Functor {
  def apply[G[_] : Functor]: Functor[G] = implicitly[Functor[G]]
}
