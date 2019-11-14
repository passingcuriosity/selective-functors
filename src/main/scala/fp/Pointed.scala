package fp

trait Pointed[M] {
  def unit: M
}

object Pointed {
  def apply[M: Pointed]: Pointed[M] = implicitly[Pointed[M]]
}
