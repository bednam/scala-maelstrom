//> using dep io.circe::circe-parser::0.14.6
//> using dep io.circe::circe-generic::0.14.6
import io.circe.*
import io.circe.generic.semiauto.*
import Init.*
import Echo.*

// 1. when sealed trait, when sealed abstract class, anything else?
/** @param src
  *   A string identifying the node this message came from
  * @param dest
  *   A string identifying the node this message is to
  * @param body
  *   An object: the payload of the message
  */
sealed trait Message[MessageBody <: Body] {
  def src: String // 2. defs or vals
  def dest: String
  def body: MessageBody
}

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
    case Right("init") => cursor.as[InitBody]
    case Right("echo") => cursor.as[EchoBody]
    case Right(t) => Left(DecodingFailure(s"Unsupported message type: $t", cursor.history))
    case Left(e) => Left[DecodingFailure, Body](e)
  }

object Init {
  case class InitBody(
      `type`: String,
      msg_id: Option[Int],
      in_reply_to: Option[Int],
      node_id: Option[String],
      node_ids: Option[Vector[String]]
  ) extends Body
  case class InitMessage(src: String, dest: String, body: InitBody)
      extends Message[InitBody]

  implicit val initBodyDecoder: Decoder[InitBody] = deriveDecoder
  implicit val initMessageDecoder: Decoder[InitMessage] = deriveDecoder
  implicit val initMessageEncoder: Encoder[InitMessage] = deriveEncoder      
}

object Echo {
  case class EchoBody(
      `type`: String,
      msg_id: Option[Int],
      in_reply_to: Option[Int],
      echo: String
  ) extends Body
  case class EchoMessage(src: String, dest: String, body: EchoBody)
      extends Message[EchoBody]

  implicit val echoBodyDecoder: Decoder[EchoBody] = deriveDecoder    
  implicit val echoMessageDecoder: Decoder[EchoMessage] = deriveDecoder
  implicit val echoMessageEncoder: Encoder[EchoMessage] = deriveEncoder      
}
