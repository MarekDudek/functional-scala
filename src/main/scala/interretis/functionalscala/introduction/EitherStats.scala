package interretis.functionalscala.introduction

object EitherStats {

  def arithmeticMean(xs: Seq[Double]): FEither[String, Double] =
    if (xs.isEmpty)
      FLeft("mean on empty list")
    else {
      val mean = xs.sum / xs.length
      FRight(mean)
    }

  def safeDiv(x: Int, y: Int): FEither[Exception, Int] =
    try FRight(x / y)
    catch {
      case e: Exception => FLeft(e)
    }
}
