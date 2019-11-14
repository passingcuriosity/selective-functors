package fp

package object syntax {
  implicit class AppSyntax[F[_]: Applicative, A](fa: F[A]) {
    val F: Applicative[F] = implicitly[Applicative[F]]
    
    def |*>|[B](fb: F[B]): F[B] = F.pure((_:A) => (b:B) => b) |*| fa |*| fb
    def |<*|[B](fb: F[B]): F[A] = F.pure((a:A) => (_:B) => a) |*| fa |*| fb
  }

  implicit class FApSyntax[F[_]: Applicative, A, B](ff: F[A => B]) {
    val F: Applicative[F] = implicitly[Applicative[F]]

    def |*|(fa: F[A]): F[B] = F.ap(ff, fa)
  }
}