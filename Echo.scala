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
  case class EchoBody(`type`: String, msg_id: Int, echo: String)

  implicit def initMessageDecoder: Decoder[InitMessage] = deriveDecoder
  implicit def initMessageEncoder: Encoder[InitMessage] = deriveEncoder

  implicit def echoMessageDecoder: Decoder[EchoMessage] = deriveDecoder
  implicit def echoMessageEncoder: Encoder[EchoMessage] = deriveEncoder

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
    body = EchoBody(`type` = "echo_ok", msg_id = responseId, echo = m.body.echo)
  )
  def run =
    Ref.of[IO, Int](0).flatMap { localId =>
      stdinUtf8[IO](1024)
        .evalMap(in => IO.fromEither(parse(in)))
        .evalMap(json =>
          IO.fromEither(
            json.hcursor.downField("body").downField("type").as[String] match {
              case Right("init") => json.as[InitMessage]
              case Right("echo") => json.as[EchoMessage]
              case Right(t) =>
                Left(new Throwable(s"unsupported message type $t"))
              case Left(_) =>
                Left(new Throwable("couldn't determine message type"))
            }
          )
        )
        .evalMap(message =>
          localId.getAndUpdate(_ + 1).map(id => message match {
            case m: InitMessage => toInitResponse(m, id).asJson.noSpaces
            case m: EchoMessage => toEchoResponse(m, id).asJson.noSpaces
          })
        )
        .evalTap(IO.println)
        .compile
        .drain
    }
}
// {"src": "c1", "dest": "n1", "body": {"msg_id": 1, "type": "init", "node_id": "n1", "node_ids": ["n1"]}}
// {src: "n1", dest: "c1", body: {msg_id: 123 in_reply_to: 1, type: "init_ok"}}
// ../maelstrom/maelstrom test -w echo --bin echo --nodes n1 --time-limit 10 --log-stderr
