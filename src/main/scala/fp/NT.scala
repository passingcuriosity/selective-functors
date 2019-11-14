package fp


// This is called ~>[F, G] in Scalaz and FunctionK[F,G] in Cats
trait NT[F[_], G[_]] {
  def apply[A](a: F[A]): G[A]
}
