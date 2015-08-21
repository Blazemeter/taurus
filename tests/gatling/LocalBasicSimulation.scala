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
0          .formUpload("key", "tests/data/test1.csv")
1          .bodyPart(RawFileBodyPart("file", "tests/data/unicode_file").contentType("application/binary").fileName("unicode_file")).asMultipartForm
2          .body(RawFileBody("tests/json/dummy.json"))
3          .body(ELFileBody("tests/json/includes.json"))
4          .body(RawFileBodyPart("tests/json/get-post.json"))
5          .body(RawFileBodyPart("file_name", "tests/json/passfail.json"))
6          .body(ELFileBodyPart("tests/json/mock_start_err.json"))
7          .body(ELFileBodyPart("file_name", "tests/json/throughput.json"))

8!          csv("tests/json/gatling_" + mode + ".json")
9          csv("tests/json/gatling.json")
10          tsv("tests/json/get-post.json")
11          ssv("tests/json/merge2.json")
12          separatedValues("tests/json/reporting.json", "#")
13          jsonFile("tests/json/mock_normal.json")

      )

  setUp(
    scn.inject(atOnceUsers(10))
  ).protocols(httpConf)
}