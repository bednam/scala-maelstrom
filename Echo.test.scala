//> using test.dep org.scalameta::munit::0.7.29
//> using files Echo.scala Message.scala

import Echo._
import Message._
import cats.syntax.all._

class EchoSpec extends munit.FunSuite {
  test("create initial message response") {
    val init = Message("c1", "n1", InitOk("init", 1.some, None, None, None))
    val response = toResponse(init, 2)
    val expected =
      Message("n1", "c1", InitOk("init_ok", 2.some, Some(1), None, None))

    assert(response == expected)
  }

  test("create echo message response") {
    val echo = Message("c1", "n1", EchoOk("echo", 1.some, None, "echo 123"))
    val response = toResponse(echo, 2)
    val expected =
      Message("n1", "c1", EchoOk("echo_ok", 2.some, 1.some, "echo 123"))

    assert(response == expected)
  }
}
