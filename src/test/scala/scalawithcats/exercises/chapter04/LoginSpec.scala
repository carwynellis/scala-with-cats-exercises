package scalawithcats.exercises.chapter04

import org.scalatest.{FlatSpec, Matchers}

class LoginSpec extends FlatSpec with Matchers {

  behavior of "findUsername"

  it should "return a username for a valid user id" in {
    Login.findUsername(1).run(Login.db) shouldBe Some("dade")
  }

  it should "return None for an invalid user id" in {
    Login.findUsername(12345).run(Login.db) shouldBe None
  }

  behavior of "checkPassword"

  it should "return true for a valid username and password" in {
    Login.checkPassword("dade", "zerocool").run(Login.db) shouldBe true
  }

  it should "return false for a valid username with bad password" in {
    Login.checkPassword("dade", "foo").run(Login.db) shouldBe false
  }

  it should "return false for an invalid username" in {
    Login.checkPassword("foo", "bar").run(Login.db) shouldBe false
  }

  behavior of "checkLogin"

  it should "return true for a valid user id and password" in {
    Login.checkLogin(1, "zerocool").run(Login.db) shouldBe true
  }

  it should "return false for a valid user id with bad password" in {
    Login.checkLogin(1, "foo").run(Login.db) shouldBe false
  }

  it should "return false for an invalid user id" in {
    Login.checkLogin(12345, "bar").run(Login.db) shouldBe false
  }

}
