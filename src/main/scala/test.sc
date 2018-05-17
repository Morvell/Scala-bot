import scala.util.Random

case class Distribution(nBins: Int, data: List[Double]) {
  require(data.length > nBins)

  val Epsilon = 0.000001
  val (max,min) = (data.max,data.min)
  val binWidth = (max - min) / nBins + Epsilon
  val bounds = (1 to nBins).map { x => min + binWidth * x }.toList

  def histo(bounds: List[Double], data: List[Double]): List[List[Double]] =
    bounds match {
      case h :: Nil => List(data)
      case h :: t   => val (l,r) = data.partition( _ < h) ; l :: histo(t,r)
    }

  val histogram = histo(bounds, data)
}


val data = Array.tabulate(100){ _ => scala.util.Random.nextDouble * 10 }
val h = Distribution(5, data.toList).histogram

val tabulated = h.map {_.size}
println(tabulated)




