//> using scala 2.13
//> using toolkit typelevel:0.1.25
//> using dep io.circe::circe-parser::0.14.6
//> using dep io.circe::circe-generic::0.14.6
//> using file Message.scala

import cats.effect._
import cats.syntax.all._
import fs2.io._
import cats.effect.std.Console
import io.circe.parser._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import scala.io.StdIn
import Message._

object Echo extends IOApp.Simple {
  def toMessage(json: Json) =
    json.as[Message]

  def toResponse(m: Message, responseId: Int) =
    Message(
      src = m.dest,
      dest = m.src,
      body = m match {
        case Message(_, _, body: init_ok) =>
          init_ok(
            msg_id = responseId.some,
            `type` = "init_ok",
            node_id = None,
            node_ids = None,
            in_reply_to = body.msg_id
          )
        case Message(_, _, body: echo_ok) =>
          echo_ok(
            `type` = "echo_ok",
            msg_id = responseId.some,
            in_reply_to = body.msg_id,
            echo = body.echo
          )
      }
    )

  def run =
    UniqueID.of[IO].flatMap { id =>
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
          id.next.map(toResponse(message, _))
        )
        .map(_.asJson.deepDropNullValues.noSpaces)
        .evalTap(message => Console[IO].errorln(s"Sending: $message"))
        .evalTap(Console[IO].println)
        .compile
        .drain
    }

    trait UniqueID[F[_]] {
      def next: F[Int]
    }

    object UniqueID {
      def of[F[_]: Sync]: F[UniqueID[F]] =
        for {
          id <- Ref.of[F, Int](0)
        } yield new UniqueID[F] {
          def next: F[Int] = id.getAndUpdate(_ + 1)
        }
    }
}

// {"src": "c1", "dest": "n1", "body": {"msg_id": 1, "type": "init", "node_id": "n1", "node_ids": ["n1"]}}
// {"src": "c1", "dest": "n1", "body": {"type": "echo", "msg_id": 1, "echo": "Echo 123"}}
// {src: "n1", dest: "c1", body: {msg_id: 123 in_reply_to: 1, type: "init_ok"}}
// ../maelstrom/maelstrom test -w echo --bin echo --nodes n1 --time-limit 10 --log-stderr
