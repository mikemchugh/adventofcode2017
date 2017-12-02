package advent

object Day01 {
  def convert(input: String): List[Int] = input.split("").map(_.toInt).toList

  def captcha(input: String): Int = {
    val numbers = convert(input)

    def captcha(numbers: List[Int], first: Int): List[Int] = numbers match {
      case Nil => Nil
      case x :: Nil => if (x == first) List(x) else Nil
      case x :: ys =>
        if (x == ys.head) x :: captcha(ys, first)
        else captcha(ys, first)
    }

    captcha(numbers, numbers.head).sum
  }

  def captchaHalfAround(input: String) : Int = {
    val numbers = convert(input)

    def captchaHalfAround(numbers: (List[Int], List[Int])): List[Int] = numbers match {
      case (Nil, Nil) => Nil
      case (x :: Nil, y :: Nil) => if (x == y) List(x,y) else Nil
      case (x :: xs, y :: ys) =>
        if (x == y) x :: y :: captchaHalfAround(xs, ys)
        else captchaHalfAround(xs, ys)
    }

    captchaHalfAround(numbers.splitAt(numbers.length / 2)).sum
  }
}
