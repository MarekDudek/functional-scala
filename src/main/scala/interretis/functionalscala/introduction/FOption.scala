package interretis.functionalscala.introduction

sealed trait FOption[+A] {

  def map[B](f: A => B): FOption[B] =
    this match {
      case FNone => FNone
      case FSome(a) => FSome(f(a))
    }

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
