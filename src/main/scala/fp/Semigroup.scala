package fp

trait Semigroup[M] {
  def plus(l: M, r: M): M
}

object Semigroup {
  def apply[M : Semigroup]: Semigroup[M] = implicitly[Semigroup[M]]
}
