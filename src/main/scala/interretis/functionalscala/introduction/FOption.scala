package interretis.functionalscala.introduction

sealed trait FOption[+A] {

  // transparent for function application, doesn't fail when no value present
  def map[B](f: A => B): FOption[B] =
    this match {
      case FNone => FNone
      case FSome(a) => FSome(f(a))
    }

  // also doesn't fail on no value but function can also fail
  def flatMap[B](f: A => FOption[B]): FOption[B] =
    map(f) getOrElse FNone

  def getOrElse[B >: A](default: => B): B =
    this match {
      case FNone => default
      case FSome(a) => a
    }

  def orElse[B >: A](ob: => FOption[B]): FOption[B] =
    map(FSome(_)) getOrElse ob

  def filter(f: A => Boolean): FOption[A] =
    this match {
      case FSome(a) if f(a) => this
      case _ => FNone
    }
}

case class FSome[+A](get: A) extends FOption[A]

case object FNone extends FOption[Nothing]

object FOption {

  def lift[A, B](f: A => B): FOption[A] => FOption[B] = _ map f

  def map2[A, B, C](oa: FOption[A], ob: FOption[B])(f: (A, B) => C): FOption[C] =
    oa flatMap {
      a => ob map {
        b => f(a, b)
      }
    }

  def sequence[A](a: List[FOption[A]]): FOption[List[A]] =
    a match {
      case Nil => FSome(Nil)
      case h :: t => h flatMap {
        hh =>
          sequence(t) map (tt => hh :: tt)
      }
    }

  def sequence2[A](a: List[FOption[A]]): FOption[List[A]] =
    a.foldRight[FOption[List[A]]](FSome(Nil))((x, y) => map2(x, y)(_ :: _))


  def attempt[A](a: => A): FOption[A] =
    try FSome(a) catch {
      case e: Exception => FNone
    }
}
