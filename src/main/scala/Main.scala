import play.api.libs.json.*
import java.io.PrintWriter
import scala.io.Source

val perPage = 100

extension (github: GitHubApi)
  def getRepos(minStars: Int, maxStars: Int): LazyList[Repo] =
    def aux(s: Int): LazyList[Repo] =
      if s < minStars then LazyList()
      else
        val t = s / 2
        getReposBetween(t, s) #::: aux(t)
    aux(maxStars)

  def getReposBetween(minStars: Int, maxStars: Int): LazyList[Repo] =
    def aux(p: Int): LazyList[Repo] =
      val repos = getReposAt(minStars, maxStars, p)
      if repos.length < perPage then repos
      else repos #::: aux(p + 1)
    aux(1)

  def getReposAt(minStars: Int, maxStars: Int, page: Int): LazyList[Repo] =
    println(s"finding repos: $minStars..$maxStars [$page]")
    github
      .get(
        "search/repositories",
        "q" -> s"stars:$minStars..$maxStars language:c",
        "order" -> "stars",
        "page" -> page.toString,
        "per_page" -> perPage.toString
      )
      .as[Repos]
      .items
      .to(LazyList)

  def getLanguages(repo: Repo): Langs =
    println(s"finding languages: ${repo}")
    github.get(s"repos/${repo.name}/languages").as[Langs]

  def getOccurrences(repo: Repo, tok: String): Int =
    println(s"finding $tok: ${repo}")
    github
      .get(
        "search/code",
        "q" -> s"$tok repo:${repo.name} language:c",
        "per_page" -> perPage.toString
      )
      .as[Occurrences]
      .count

@main def main() =
  val token = Source.fromFile("token").mkString.trim
  val github = GitHubApi(token)
  val pw = PrintWriter("result")
  github
    .getRepos(10000, 100000)
    .map(r =>
      val l = github.getLanguages(r)
      val c = github.getOccurrences(r, "pthread_mutex_lock")
      RepoData(r, l, c)
    )
    .filter(d => d.langs.cPortion >= 0.5 && d.count > 0)
    .foreach(d =>
      pw.println(d)
      pw.flush()
    )
  pw.close()

case class RepoData(repo: Repo, langs: Langs, count: Int):
  override def toString =
    s"${repo.name}\t${repo.stars}\t${langs.c}\t${langs.total}\t$count"

case class Repos(items: List[Repo])

object Repos:
  given Reads[Repos] = Json.reads[Repos]

case class Repo(full_name: String, stargazers_count: Int):
  val name = full_name
  val stars = stargazers_count

  override def toString = s"$name ($stars)"

object Repo:
  given Reads[Repo] = Json.reads[Repo]

case class Langs(langs: Map[String, Int]):
  val c = langs.getOrElse("C", 0)
  val total = langs.values.sum
  val cPortion = c.toFloat / (total + 1)

  override def toString = f"$c/$total=$cPortion%.3f"

object Langs:
  given Reads[Langs] with
    def reads(json: JsValue): JsResult[Langs] = JsSuccess(
      Langs(
        json
          .as[JsObject]
          .fields
          .map((k, v) => k -> v.as[Int])
          .toMap
      )
    )

case class Occurrences(total_count: Int):
  val count = total_count

object Occurrences:
  given Reads[Occurrences] = Json.reads[Occurrences]
