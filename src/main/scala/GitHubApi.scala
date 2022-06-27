import org.apache.commons.io.IOUtils
import play.api.libs.json.*
import java.net.{URL, URLEncoder, HttpURLConnection}

enum GitHubResult:
  case Success(result: JsValue)
  case Failure(reset: Long)
  case SecondaryLimit

import GitHubResult.*

class GitHubApi(token: String):
  val headers =
    Map("Authorization" -> s"token $token", "User-Agent" -> "request")

  def get(name: String, params: (String, String)*): JsValue =
    val query = params
      .map((k, v) => s"$k=${URLEncoder.encode(v, "UTF-8")}")
      .mkString("&")
    val url = s"https://api.github.com/$name?$query"

    def aux(): JsValue = getFrom(url) match
      case Success(v) => v
      case Failure(r) =>
        val wait = r - System.currentTimeMillis
        println(s"Limit exceeded. Wait for $wait ms.")
        Thread.sleep(wait)
        aux()
      case SecondaryLimit =>
        val wait = 3
        println(s"Secondary limit exceeded. Wait for $wait min.")
        Thread.sleep(1000 * 60 * wait)
        aux()
    aux()

  def getFrom(url: String): GitHubResult =
    val con = URL(url).openConnection.asInstanceOf[HttpURLConnection]
    headers.map(con.setRequestProperty(_, _))
    con.setUseCaches(false)
    con.setInstanceFollowRedirects(false)
    con.setDoInput(true)
    con.setDoOutput(false)
    con.setRequestMethod("GET")

    val res =
      if con.getResponseCode == HttpURLConnection.HTTP_OK then
        val in = con.getInputStream
        val content = IOUtils.toString(in, "UTF-8")
        in.close()
        Success(Json.parse(content))
      else
        val err = con.getErrorStream
        val content = IOUtils.toString(err, "UTF-8")
        err.close()
        if content.contains("secondary") then SecondaryLimit
        else
          val reset =
            con.getHeaderFields.get("X-RateLimit-Reset").get(0).toLong * 1000
          Failure(reset)
    con.disconnect()
    res
