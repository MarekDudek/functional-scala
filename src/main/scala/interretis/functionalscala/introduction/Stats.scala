package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FOption.{attempt, lift, map2}

import scala.language.postfixOps
import scala.math.pow

object Stats {

  def arithmeticMean(xs: Seq[Double]): FOption[Double] =
    if (xs.isEmpty) FNone
    else {
      val mean = xs.sum / xs.length
      FSome(mean)
    }

  def variance(xs: Seq[Double]): FOption[Double] =
    arithmeticMean(xs) flatMap {
      mean =>
        val deviations = xs map (_ - mean)
        val squares = deviations map (pow(_, 2))
        arithmeticMean(squares)
    }

  val abs: FOption[Double] => FOption[Double] = lift(math.abs)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    numberOfSpeedingTickets / age

  def insuranceRateQuote(age: String, numberOfSpeedingTickets: String): FOption[Double] = {
    val optAge = attempt(age toInt)
    val optTickets = attempt(numberOfSpeedingTickets toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }
}
