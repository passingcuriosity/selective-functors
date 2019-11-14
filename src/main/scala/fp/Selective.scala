package fp

trait Selective[F[_]] extends Applicative[F] {
  def select[A,B](
    fe: F[Either[A,B]],
    ff: F[A => B]
  ): F[B]
}

object Selective {
  def apply[G[_] : Selective]: Selective[G] = implicitly[Selective[G]]

  def branch[F[_]: Selective, A, B, C](
    fe: F[Either[A,B]],
    fl: F[A => C],
    fr: F[B => C]
  ): F[C] = {
    val F = implicitly[Selective[F]]
    val ffe: F[Either[A, Either[B,C]]] = F.fmap(fe, (_: Either[A,B]).map(left[B,C]))
    val ffl: F[A => Either[B,C]] = F.fmap(fl, (r: A => C) => (a:A) => Right(r(a)))
    F.select(F.select(ffe, ffl), fr)
  }

  def ifS[F[_]: Selective, A](
    cond: F[Boolean],
    yes: F[A],
    no: F[A]
  ): F[A] = {
    val F = implicitly[Selective[F]]
    val fc: F[Either[Unit,Unit]] = F.fmap(cond, bool((), ()))
    val fl: F[Unit => A] = F.fmap(yes, const[A, Unit])
    val fr: F[Unit => A] = F.fmap(no, const[A, Unit])
    branch(fc, fl, fr)
  }

  private def const[A,B]: A => B => A = (a: A) => (_: B) => a

  private def bool[A,B](a: A, b: B)(cond: Boolean): Either[A, B] =
    if (cond) { Left(a) } else { Right(b) }

  private def left[A,B](a: A): Either[A,B] = Left(a)

  private def right[A,B](b: B): Either[A,B] = Right(b)

}
