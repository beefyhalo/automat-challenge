import cats.effect._
import cats.implicits._
import cats.temp.par._
import fs2.Stream
import io.circe.Decoder
import io.circe.generic.auto._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.blaze._
import org.scalatest.{Matchers, WordSpec}
import scala.concurrent.ExecutionContext.Implicits.global

class Queries[F[_]: Concurrent: Par](client: Client[F]) {

  def top: F[List[StoryId]] =
    client.expect("https://hacker-news.firebaseio.com/v0/topstories.json")(jsonOf[F, List[StoryId]])

  def item[A: Decoder](id: Int): F[Option[A]] =
    client
      .expect(s"https://hacker-news.firebaseio.com/v0/item/$id.json")(jsonOf[F, A])
      .map(Option(_))
      .recover { case _ => None }

  def commentsForStory(story: Story): F[List[Comment]] =
    story.kids.toList.parFlatTraverse(cid => item[Comment](cid.value).map(_.toList))

  def results(storyCount: Int, commentCount: Int): F[List[Result]] =
    storiesWithComments(storyCount).compile.toList
      .map { storiesWithComments =>
        val totalCounts = groupSize(storiesWithComments >>= (_.comments))(_.by)
        storiesWithComments.map {
          case StoryWithComments(story, comments) =>
            val storyCounts = groupSize(comments)(_.by)
            val commenters  = storyCounts.map { case (name, storyCount) => Commenter(name, storyCount, totalCounts(name)) }.toList.sortBy(_.storyCount)
            Result(story, commenters.take(commentCount))
        }
      }

  def storiesWithComments(storyCount: Int): Stream[F, StoryWithComments] =
    Stream
      .eval(top)
      .flatMap(Stream.emits)
      .parEvalMapUnordered(16)(sid => item[Story](sid.value))
      .flatMap(story => Stream.emits(story.toSeq))
      .take(storyCount.toLong)
      .evalMap(story => commentsForStory(story).map(StoryWithComments(story, _)))

  private def groupSize[A, B](as: List[A])(groupBy: A => B) = as.groupBy(groupBy).map { case (k, v) => (k, v.size) }
}

class AutomatChallengeSpec extends WordSpec with Matchers {
  implicit val cs: ContextShift[IO] = IO.contextShift(global)

  "get comments" in {
    val n       = 30
    val results = BlazeClientBuilder[IO](global).stream.flatMap(new Queries[IO](_).storiesWithComments(n)).compile.toVector.unsafeRunSync
    println(results)
    results != Nil
  }

  "get results" in {
    val storyCount   = 30
    val commentCount = 10
    val results      = BlazeClientBuilder[IO](global).resource.use(new Queries[IO](_).results(storyCount, commentCount)).unsafeRunSync

    results.foreach(result =>
      println(s"""
        Story ${result.story.title} | ${result.commenters
        .map(commenter => s"${commenter.name} (${commenter.storyCount} for story - ${commenter.totalCount} total)")
        .mkString("|")}
    """))

    results.size shouldEqual storyCount
    results.foreach(_.commenters.size should be <= commentCount)
  }
}

final case class CommentId(val value: Int) extends AnyVal
final case class StoryId(val value: Int)   extends AnyVal

final case class Comment(by: String)
final case class Commenter(name: String, storyCount: Int, totalCount: Int)
final case class Result(story: Story, commenters: List[Commenter])
final case class Story(title: String, kids: List[CommentId])
final case class StoryWithComments(story: Story, comments: List[Comment])

object CommentId {
  implicit val CommentIdDecoder: Decoder[CommentId] = Decoder[Int].map(CommentId(_))
}
object StoryId {
  implicit val StoryIdDecoder: Decoder[StoryId] = Decoder[Int].map(StoryId(_))
}
