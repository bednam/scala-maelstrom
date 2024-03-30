//> using test.dep org.scalameta::munit::0.7.29
//> using test.dep io.circe::circe-parser::0.14.6
//> using file Echo.scala

import io.circe.parser.parse
import io.circe.syntax.*
import io.circe.*
import Echo.*

class EchoSpec extends munit.FunSuite {
    def decode[A: Decoder](s: String) = 
        parse(s).flatMap(_.as[A]).toOption.get

    def noSpaces(s: String) = parse(s).toOption.get.noSpaces
    

    test("decode initial message") {
        val init = """|{
                      | "src": "c1",
                      | "dest": "n1",
                      | "body": {
                      |     "type": "init",
                      |     "msg_id": 1
                      | }
                      |}
                      |""".stripMargin
        val expected = InitMessage("c1", "n1", InitBody("init", 1, None, None, None))

        assert(decode[InitMessage](init) == expected)
    }

    test("encode initial response message") {
        val response = InitMessage("n1", "c1", InitBody("init_ok", 2, None, None, Some(1)))
        val expected = """|{
                          | "src": "n1",
                          | "dest": "c1",
                          | "body": {
                          |     "type": "init_ok",
                          |     "msg_id": 2,
                          |     "in_reply_to": 1    
                          | }
                          |}
                          |""".stripMargin
                            
        assert(response.asJson.noSpaces == noSpaces(expected))
    }

    test("decode echo message") {
        val echo = """|{
                      | "src": "c1",
                      | "dest": "n1",
                      | "body": {
                      |     "type": "echo",
                      |     "msg_id": 1,
                      |     "echo": "echo 123"
                      | }
                      |}
                      |""".stripMargin

        val expected = EchoMessage("c1", "n1", EchoBody("echo", 1, None, "echo 123"))

        assert(decode[EchoMessage](echo) == expected)
    }
    
    test("encode echo response message") {
        val echo = EchoMessage("n1", "c1", EchoBody("echo_ok", 2, Some(1), "echo 123"))
        val encoded = """|{
                         |  "src": "n1",
                         |  "dest": "c1",
                         |  "body": {
                         |      "type": "echo_ok",
                         |      "msg_id": 2,
                         |      "in_reply_to": 1,
                         |      "echo": "echo 123"
                         |  }
                         |}
                         |""".stripMargin

        assert(echo.asJson.noSpaces == noSpaces(encoded))
    }
}