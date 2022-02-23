object bucketize{

  def trim(value: Double, precision: Int): Double = {
    val x = math.pow(10, precision)
    (math floor value * x) / x
  }

  def find_range(element: Double): (Double, Double) = {
    val temp = element * 100
    val start: Double = ((temp / 5).floor) * 0.05;
    val end: Double = start + 0.049;
    return (trim(start, 3), trim(end, 3))
  }

  def main(args: Array[String]): Unit = {
    val array: Array[Double] = Array(12.05, 12.03, 10.33, 11.45, 13.50)
    for (element <- array) {
      println(s"Element => $element | Bucket Range => ${find_range(element)}")
    }
  }
}