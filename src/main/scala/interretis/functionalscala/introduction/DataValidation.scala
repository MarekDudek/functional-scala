package interretis.functionalscala.introduction

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Person {

  def mkName(name: String): FEither[String, Name] =
    if (name == "" || name == null) FLeft("Name is empty.")
    else FRight(new Name(name))

  def mkAge(age: Int): FEither[String, Age] =
    if (age < 0) FLeft("Age is out of range.")
    else FRight(new Age(age))

  def mkPerson(name: String, age: Int): FEither[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}
