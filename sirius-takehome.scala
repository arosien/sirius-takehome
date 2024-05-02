//> using scala 2
//> using toolkit typelevel:0.1.25
//> using dep com.beachape::enumeratum-circe:1.7.3
//> using dep io.circe::circe-generic-extras::0.14.3

import cats.data.NonEmptyList
import cats.syntax.all._
import enumeratum._
import enumeratum.EnumEntry.Lowercase
import io.circe._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import io.circe.syntax._
import Content.Playable
import Content.Container

/** Algebraic data type of content: either a playable or a container. */
sealed trait Content {
  def title: String
}

object Content {
  case class Playable(id: Id[Playable], title: String, `type`: Playable.Type)
      extends Content

  object Playable {

    /** Types of playables. */
    sealed trait Type extends EnumEntry with Lowercase

    object Type extends Enum[Type] with CirceEnum[Type] {
      val values = findValues

      case object Channel extends Type
      case object Episode extends Type
    }
  }

  case class Container(id: Id[Container], title: String, `type`: Container.Type)
      extends Content

  object Container {

    /** Types of containers. */
    sealed trait Type extends EnumEntry with Lowercase

    object Type extends Enum[Type] with CirceEnum[Type] {
      val values = findValues

      case object Show extends Type
      case object Category extends Type
    }
  }

  /** Typesafe identifier. */
  case class Id[A](value: String) extends AnyVal

  /** Maps a container to its playable "children". */
  case class Index(value: Map[Id[Container], NonEmptyList[Id[Playable]]])

  object Index {
    def fromPairs(values: List[(Id[Container], Id[Playable])]): Index =
      Index(values.foldMap { case (cid, pid) =>
        Map(cid -> NonEmptyList.of(pid))
      })
  }
}

/** "RESTful API" to fetch playables from an input query. */
trait API {
  def query(input: API.Input): API.Output
}

object API {

  /** A input request to the API, expected to be of the form "Alex, play <query>
    * on SiriusXM".
    */
  case class Input(input: String) {
    private val pattern = "Alexa, play (.+) on SiriusXM".r

    def query: Option[String] =
      input match {
        case pattern(query) => Some(query)
        case _              => None
      }
  }

  /** Output only contains playable content. */
  case class Output(playable: List[Content.Playable])

  object Output {
    val empty: Output = Output(List.empty)
  }

  /** Searches for all content where the title contains the input query:
    *
    *   - playables included as-is, containers expanded to contained playables;
    *   - deduplicate playables;
    *   - sort by id ascending.
    */
  def from(contents: List[Content], index: Content.Index): API =
    new API {
      val playableById: Map[Content.Id[Playable], Playable] =
        contents.collect { case p: Playable =>
          p.id -> p
        }.toMap

      def query(input: Input): Output =
        // TODO: if no input query, change method return type and return an error
        input.query
          .map { query =>
            val candidates =
              contents
                .filter(_.title.contains(query))
                .flatMap {
                  case p: Playable         => List(p)
                  case Container(id, _, _) =>
                    // expand container to contained playables
                    index.value
                      .get(id)
                      .map(_.map(playableById.apply).toList)
                      .getOrElse(List.empty)
                }
                .distinct
                .sortBy(
                  _.id.value.toInt // n.b. assumption that ids are int-as-string.
                )

            Output(candidates)
          }
          .getOrElse(
            Output.empty
          )
    }
}

/** All implicits needed to encode/decode JSON. */
object JsonSupport {

  implicit val customConfig: Configuration = Configuration.default

  implicit def codecId[A]: Codec[Content.Id[A]] =
    deriveUnwrappedCodec[Content.Id[A]]

  implicit val codecPlayable: Codec[Content.Playable] =
    deriveConfiguredCodec[Content.Playable]

  // TODO: somehow the "group" property is (correctly!) being used as the type discriminator, how??
  implicit val codecContainer: Codec[Content.Container] =
    deriveConfiguredCodec[Content.Container]

  // had to look how to do ADTs...
  implicit val decoderContent: Decoder[Content] =
    List[Decoder[Content]](
      Decoder[Playable].widen,
      Decoder[Container].widen
    ).reduceLeft(_ or _)
  implicit val encoderContent: Encoder[Content] =
    Encoder.instance {
      case playable: Content.Playable   => playable.asJson
      case container: Content.Container => container.asJson
    }

  implicit val codecInput: Codec[API.Input] = deriveConfiguredCodec[API.Input]

  implicit val codecOutput: Codec[API.Output] =
    deriveConfiguredCodec[API.Output]

  /** Representation of the container id -> playable id "edge" entry. */
  private case class Edge(container_id: String, playable_id: String)

  private implicit val decoderEdge: Decoder[Edge] =
    deriveConfiguredDecoder[Edge]

  /** An index is decoded from a list of edges representation. */
  implicit val decoderIndex: Decoder[Content.Index] =
    Decoder[List[Edge]].map(edges =>
      Content.Index.fromPairs(edges.map { case Edge(cid, pid) =>
        (Content.Id[Content.Container](cid), Content.Id[Content.Playable](pid))
      })
    )
}
