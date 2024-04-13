//> using scala 2.13
//> using toolkit typelevel:0.1.25
//> using dep io.circe::circe-parser::0.14.6
//> using file Message.scala

import cats.effect._
import cats.syntax.all._
import cats.effect.std.Console
import io.circe.parser.parse
import io.circe.syntax._
import Message._
import scala.util.control.NoStackTrace

object UniqueIDs extends IOApp.Simple {
  def run =
    Ref.of[IO, Int](0).flatMap { id =>
      fs2.Stream
        .repeatEval(Console[IO].readLine)
        .evalTap(line => Console[IO].errorln(s"Received: $line"))
        .evalMap(in => IO.fromEither(parse(in)))
        .evalMap(json => IO.fromEither(json.as[Message]))
        .evalMap(message =>
          id.getAndUpdate(_ + 1).flatMap(toResponse(message, _))
        )
        .map(_.asJson.deepDropNullValues.noSpaces)
        .evalTap(message => Console[IO].errorln(s"Sending: $message"))
        .evalTap(Console[IO].println)
        .printlns
        .compile
        .drain

    }

  def toResponse(m: Message, responseId: Int) = {
    case object MessageNotSupported extends NoStackTrace

    val body: IO[Body] = m match {
      case Message(_, _, body: init_ok) =>
        init_ok(
          msg_id = responseId.some,
          `type` = "init_ok",
          node_id = None,
          node_ids = None,
          in_reply_to = body.msg_id
        ).pure[IO]
      case Message(_, _, body: generate_ok) =>
        IO.randomUUID.map { uid =>
          generate_ok(
            `type` = "generate_ok",
            msg_id = responseId.some,
            in_reply_to = body.msg_id,
            id = uid.toString.some
          )
        }
      case _ => IO.raiseError(MessageNotSupported)
    }

    body.map(body =>
      Message(
        src = m.dest,
        dest = m.src,
        body = body
      )
    )
  }
}

// {"src": "c1", "dest": "n1", "body": {"type": "generate"}}
// ../maelstrom/maelstrom test -w unique-ids --bin unique-ids --time-limit 30 --rate 1000 --node-count 3 --availability total --nemesis partition
