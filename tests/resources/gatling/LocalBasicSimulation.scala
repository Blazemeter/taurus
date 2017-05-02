package mytest

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

val mode = "production"

class BasicSimulation extends Simulation {

  val httpConf = http
    .baseURL("http://127.0.0.1/")
    .acceptHeader("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    .doNotTrackHeader("1")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .acceptEncodingHeader("gzip, deflate")
    .userAgentHeader("Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0")

  val scn =     scenario("BasicSimulation") .exec(
          http("request_1").post("some_action")
0          .formUpload("key", "tests/resources/test1.csv")
1          .bodyPart(RawFileBodyPart("file", "tests/resources/unicode_file").contentType("application/binary").fileName("unicode_file")).asMultipartForm
2          .body(RawFileBody("tests/json/blazemeter-api-user.json"))
3          .body(RawFileBodyPart("tests/json/get-post.json"))
4          .body(RawFileBodyPart("file_name", "tests/json/passfail.json"))
5          .body(ELFileBodyPart("tests/json/mock_start_err.json"))
6          .body(ELFileBodyPart("file_name", "tests/json/grinder.json"))

7!          csv("tests/json/gatling_" + mode + ".json")
8          csv("tests/json/gatling.json")
9          tsv("tests/json/get-post.json")
10          ssv("tests/json/merge2.json")
11          separatedValues("tests/json/reporting.json", "#")
12          jsonFile("tests/json/mock_normal.json")

      )

  setUp(
    scn.inject(atOnceUsers(10))
  ).protocols(httpConf)
}