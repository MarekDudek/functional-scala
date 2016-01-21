package interretis.functionalscala.introduction

import scala.math.pow

object Stats {

  def arithmeticMean(xs: Seq[Double]): FOption[Double] =
    if (xs.isEmpty) FNone
    else {
      val mean = xs.sum / xs.length
      FSome(mean)
    }

  def variance(xs: Seq[Double]): FOption[Double] =
    if (xs.isEmpty) FNone
    else {
      val mean = arithmeticMean(xs).getOrElse(0.0)
      val deviations = xs map (_ - mean)
      val squares = deviations map (pow(_, 2))
      val variance = squares.sum / xs.length
      FSome(variance)
    }

  def variance2(xs: Seq[Double]): FOption[Double] =
    arithmeticMean(xs) flatMap {
      mean =>
        val deviations = xs map (_ - mean)
        val squares = deviations map (pow(_, 2))
        arithmeticMean(squares)
    }
}
