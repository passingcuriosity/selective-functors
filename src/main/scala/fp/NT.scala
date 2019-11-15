package fp

/**
 * A natural transformation maps between functors, respecting the internal
 * structure.
 *
 * This type is called ~>[F, G] in Scalaz and FunctionK[F,G] in Cats.
 */
trait NT[F[_], G[_]] {
  def apply[A](a: F[A]): G[A]
}
