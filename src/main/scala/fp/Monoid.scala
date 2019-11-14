package fp

trait Monoid[M] extends Semigroup[M] with Pointed[M]

object Monoid {
  def apply[M: Monoid]: Monoid[M] = implicitly[Monoid[M]]
}
