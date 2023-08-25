object Q2 {
  def main(args: Array[String]): Unit = {
    val x = new Rational(3, 4)
    val y = new Rational(5, 8)
    val z = new Rational(2, 7)

    //x- y -z
    val result = x.sub(y).sub(z)
    println(result.output())
  }

  class Rational(n: Int, d: Int) {
    require(d > 0, "Denominator must be greater than 0")

    private val gcdValue = gcd(n.abs, d.abs)
    val numer: Int = n / gcdValue
    val denom: Int = d / gcdValue

    def neg = new Rational(-this.numer, this.denom)

    def sub(obj: Rational): Rational = {
      new Rational(
        numer * obj.denom - obj.numer * denom,
        denom * obj.denom
      )
    }

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    def output(): String = numer + "/" + denom
  }
}