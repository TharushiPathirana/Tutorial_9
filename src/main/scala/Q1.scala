object Q1 {
  def main(args: Array[String]): Unit = {
    val obj = new Rational(1, 2)
    val myObj = obj.neg
    println(myObj.output())
  }

  class Rational(n: Int, d: Int) {
    require(d > 0, "Denominator must be greater than 0")

    var numer: Int = n
    var denom: Int = d

    def neg = new Rational(-this.numer, this.denom)

    def output(): String = numer + "/" + denom
  }
}

