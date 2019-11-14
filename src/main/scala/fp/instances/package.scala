package fp

package object instances {
  implicit def listInstance[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def unit: List[A] = List.empty[A]
    override def plus(l: List[A], r: List[A]): List[A] = l ++ r
  }

  private def just[A](a: A): Maybe[A] = Just(a)
  private def none[A]: Maybe[A] = None

  implicit val maybeInstance: Applicative[Maybe] = new Applicative[Maybe] {
    def fmap[A,B](fa: Maybe[A], f: A => B): Maybe[B] =
      fa match {
        case Just(a) => Just(f(a))
        case None => None
      }

    def pure[A](a: A): Maybe[A] = Just(a)

    def ap[A,B](ff: Maybe[A => B], fa: Maybe[A]): Maybe[B] =
      (ff, fa) match {
        case (Just(f), Just(a)) => Just(f(a))
        case _ => None
      }
  }

  implicit val instance2: Selective[Maybe] = new Selective[Maybe] {
    def fmap[A,B](fa: Maybe[A], f: A => B): Maybe[B] =
      fa match {
        case Just(a) => Just(f(a))
        case None => None
      }

    def pure[A](a: A): Maybe[A] = Just(a)

    def ap[A,B](ff: Maybe[A => B], fa: Maybe[A]): Maybe[B] =
      (ff, fa) match {
        case (Just(f), Just(a)) => Just(f(a))
        case _ => None
      }

    def select[A,B](me: Maybe[Either[A,B]], mf: Maybe[A => B]): Maybe[B] =
      me match {
        case Just(Left(a)) =>
          mf match {
            case Just(f) => Just(f(a))
            case None => None
          }
        case Just(Right(b)) =>
          Just(b)
        case None =>
          None
      }

  }

}