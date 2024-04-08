//> using scala 2.13
//> using dep io.circe::circe-parser::0.14.6
//> using dep io.circe::circe-generic::0.14.6
//> using dep io.circe::circe-generic-extras::0.14.3
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._

object Message {
  implicit val config: Configuration =
    Configuration.default.withSnakeCaseMemberNames.withDiscriminator("type")

  // 1. when sealed trait, when sealed abstract class, anything else?
  /** @param src
    *   A string identifying the node this message came from
    * @param dest
    *   A string identifying the node this message is to
    * @param body
    *   An object: the payload of the message
    */
  case class Message(src: String, dest: String, body: Body)
  // implicit val messageEncoder: Encoder[Message] = Encoder.instance { m =>
  //   Json.obj(
  //     "src" := m.src,
  //     "dest" := m.dest,
  //     "body" := m.body.asJson
  //   )
  // }
  implicit val messageEncoder: Encoder[Message] = deriveEncoder
  implicit val messageDecoder: Decoder[Message] = deriveDecoder

  /** @param type
    *   (mandatory) A string identifying the type of message this is
    * @param msg_id
    *   (optional) A unique integer identifier
    * @param in_reply_to
    *   (optional) For req/response, the msg_id of the request
    */
  sealed trait Body {
    def `type`: String
    def msg_id: Option[Int]
    def in_reply_to: Option[Int]
  }

  implicit val bodyDecoder: Decoder[Body] = (cursor: HCursor) =>
    cursor.downField("type").as[String] match {
      case Right("init") => cursor.as[InitOk]
      case Right("echo") => cursor.as[EchoOk]
      case Right(t) =>
        Left(DecodingFailure(s"Unsupported message type: $t", cursor.history))
      case Left(e) => Left[DecodingFailure, Body](e)
    }

  implicit val bodyEncoder: Encoder[Body] =
    deriveEncoder[Body].mapJsonObject(json =>
      json("type").flatMap(_.hcursor.downField("type").as[String].toOption) match {
        case Some("InitOk") => json.add("type", "init_ok".asJson)
        case Some("EchoOk") => json.add("type", "echo_ok".asJson)
        case _ => json
      }
    )

  case class InitOk(
      `type`: String,
      msg_id: Option[Int],
      in_reply_to: Option[Int],
      node_id: Option[String],
      node_ids: Option[Vector[String]]
  ) extends Body

  implicit val initOkDecoder: Decoder[InitOk] = deriveDecoder

  case class EchoOk(
      `type`: String,
      msg_id: Option[Int],
      in_reply_to: Option[Int],
      echo: String
  ) extends Body

  implicit val echoOkDecoder: Decoder[EchoOk] = deriveDecoder
}
