package advent

import org.scalatest._

class CaptchaSpec extends FlatSpec with Matchers {
  "captcha" should "add a digit that match the next digit in the list" in {
    Day01.captcha("1122") shouldEqual 3
  }

  "captcha" should "sum all the digits when they are identical" in {
    Day01.captcha("1111") shouldEqual 4
  }

  "captcha" should "return 0 when no digit matches the next digit in the list" in {
    Day01.captcha("1234") shouldEqual 0
  }

  "captcha" should "return the first digit when it matches the last digit as the list is circular" in {
    Day01.captcha("91212129") shouldEqual 9
  }

  "captchaHalfAround" should "add a digit if the identical digit is half way around the list" in {
    Day01.captchaHalfAround("1212") shouldEqual 6
  }

  "captchaHalfAround" should "not add a digit if the identical digit is not half way around the list" in {
    Day01.captchaHalfAround("1122") shouldEqual 0
  }

  "captchaHalfAround" should "double the digit when it is the only match half way around the list" in {
    Day01.captchaHalfAround("123425") shouldEqual 4
  }
}
