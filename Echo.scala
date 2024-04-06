//> using toolkit typelevel:0.1.23
//> using dep io.circe::circe-parser::0.14.6
//> using dep io.circe::circe-generic::0.14.6

import cats.effect.*
import cats.syntax.all.*
import fs2.io.*
import cats.effect.std.Console
import io.circe.parser.*
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import scala.io.StdIn

object Echo extends IOApp.Simple {
  case class InitMessage(src: String, dest: String, body: InitBody)
  case class InitBody(
      `type`: String,
      msg_id: Int,
      node_id: Option[String],
      node_ids: Option[Vector[String]],
      in_reply_to: Option[Int]
  )
  case class EchoMessage(src: String, dest: String, body: EchoBody)
  case class EchoBody(
      `type`: String,
      msg_id: Int,
      in_reply_to: Option[Int],
      echo: String
  )

  implicit val initMessageDecoder: Decoder[InitMessage] = deriveDecoder
  implicit val initMessageEncoder: Encoder[InitMessage] = deriveEncoder
  implicit val initBodyEncoder: Encoder[InitBody] =
    deriveEncoder[InitBody].mapJsonObject(_.filter {
      case ("node_id", v)  => !v.isNull
      case ("node_ids", v) => !v.isNull
      case (_, _)          => true
    })

  implicit val echoMessageDecoder: Decoder[EchoMessage] = deriveDecoder
  implicit val echoMessageEncoder: Encoder[EchoMessage] = deriveEncoder

  def toInitResponse(m: InitMessage, responseId: Int) = InitMessage(
    src = m.dest,
    dest = m.src,
    body = InitBody(
      msg_id = responseId,
      `type` = "init_ok",
      node_id = None,
      node_ids = None,
      in_reply_to = m.body.msg_id.some
    )
  )

  def toEchoResponse(m: EchoMessage, responseId: Int) = EchoMessage(
    src = m.dest,
    dest = m.src,
    body = EchoBody(
      `type` = "echo_ok",
      msg_id = responseId,
      in_reply_to = m.body.msg_id.some,
      echo = m.body.echo
    )
  )

  def toMessage(json: Json) =
    json.hcursor.downField("body").downField("type").as[String] match {
      case Right("init") => json.as[InitMessage]
      case Right("echo") => json.as[EchoMessage]
      case Right(t) =>
        Left(new Throwable(s"unsupported message type $t"))
      case Left(_) =>
        Left(new Throwable("couldn't determine message type"))
    }

  def toMessage2(json: Json) = 
    json.as[InitMessage]

  def run =
    Ref.of[IO, Int](0).flatMap { localId =>
      fs2.Stream
        .repeatEval(
          Console[IO].readLine
        )
        .evalTap(line => Console[IO].errorln(s"Received: $line"))
        .evalMap(in => IO.fromEither(parse(in)))
        .evalMap(json =>
          IO.fromEither(
            toMessage(json)
          )
        )
        .evalMap(message =>
          localId
            .getAndUpdate(_ + 1)
            .map(id =>
              message match {
                case m: InitMessage => toInitResponse(m, id).asJson.noSpaces
                case m: EchoMessage => toEchoResponse(m, id).asJson.noSpaces
              }
            )
        )
        .evalTap(message => Console[IO].errorln(s"Sending: $message"))
        .evalTap(Console[IO].println)
        .compile
        .drain
    }
}

// {"src": "c1", "dest": "n1", "body": {"msg_id": 1, "type": "init", "node_id": "n1", "node_ids": ["n1"]}}
// {"src": "c1", "dest": "n1", "body": {"type": "echo", "msg_id": 1, "echo": "Echo 123"}}
// {src: "n1", dest: "c1", body: {msg_id: 123 in_reply_to: 1, type: "init_ok"}}
// ../maelstrom/maelstrom test -w echo --bin echo --nodes n1 --time-limit 10 --log-stderr
