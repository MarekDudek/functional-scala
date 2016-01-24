package interretis.functionalscala.introduction

sealed trait FEither[+E, +A] {

  def map[B](f: A => B): FEither[E, B] =
    this match {
      case FRight(a) => FRight(f(a))
      case FLeft(e) => FLeft(e)
    }

  def flatMap[EE >: E, B](f: A => FEither[EE, B]): FEither[EE, B] =
    this match {
      case FRight(a) => f(a)
      case FLeft(e) => FLeft(e)
    }

  def orElse[EE >: E, B >: A](b: => FEither[EE, B]): FEither[EE, B] =
    this match {
      case FLeft(_) => b
      case _ => this
    }

  def map2[EE >: E, B, C](eb: FEither[EE, B])(f: (A, B) => C): FEither[EE, C] =
    for {
      a <- this
      b <- eb
    } yield f(a, b)
}

case class FLeft[+E](value: E) extends FEither[E, Nothing]

case class FRight[+A](value: A) extends FEither[Nothing, A]

object FEither {

  def sequence[E, A](l: List[FEither[E, A]]): FEither[E, List[A]] =
    l match {
      case Nil => FRight(Nil)
      case e :: es => e flatMap {
        a => sequence(es) map (as => a :: as)
      }
    }

  def traverse[E, A, B](l: List[A])(f: A => FEither[E, B]): FEither[E, List[B]] =
    l match {
      case Nil => FRight(Nil)
      case a :: as => f(a) flatMap {
        b => traverse(as)(f) map (bb => b :: bb)
      }
    }


  def attempt[A](a: => A): FEither[String, A] =
    try FRight(a) catch {
      case e: Exception => FLeft(e.getMessage)
    }
}
