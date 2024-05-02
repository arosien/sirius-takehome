//> using resourceDir path .
//> using test.dep io.circe::circe-parser::0.14.7
//> using test.dep org.scalameta::munit::0.7.29

import cats.syntax.all._

class MyTests extends munit.FunSuite {

  import JsonSupport._

  val api = {
    val api = for {
      contents <- io.circe.parser
        .decode[List[Content]](
          scala.io.Source.fromResource("content.json").mkString
        )
      index <- io.circe.parser
        .decode[Content.Index](
          scala.io.Source.fromResource("playable.json").mkString
        )
    } yield API.from(contents, index)

    api.fold(throw _, identity) // go boom if test data not available + parsable
  }

  /** Golden tests: inputs paired with expected outputs. */
  val golden: List[(API.Input, List[Content.Id[Content.Playable]])] =
    List(
      API.Input("Alexa, play Howard Stern on SiriusXM") -> List("1", "2", "3")
        .map(Content.Id[Content.Playable]),
      API.Input("Alexa, play Howard Stern 24/7 on SiriusXM") -> List(
        Content.Id[Content.Playable]("1")
      ),
      API.Input("Alexa, play SiriusXM NFL Radio on SiriusXM") -> List(
        Content.Id[Content.Playable]("4")
      ),
      API.Input("Alexa, play Sports on SiriusXM") -> List(
        Content.Id[Content.Playable]("4")
      ),
      API.Input("Alexa, play Elton John on SiriusXM") -> List(
        Content.Id[Content.Playable]("3")
      )
    )

  // Generate a test for each golden entry: returned ids match expected ids.
  golden.foreach { case (input, expected) =>
    val query = input.query.getOrElse(sys.error(s"no query in $input!"))

    test(s"Query: $query") {
      assertEquals(
        api
          .query(input)
          .playable
          .map(_.id),
        expected
      )
    }
  }
}
