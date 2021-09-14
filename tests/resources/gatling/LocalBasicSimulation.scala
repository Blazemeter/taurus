package mytest

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class BasicSimulation extends Simulation {

  val httpConf = http
    .baseUrl("http://127.0.0.1/")
    .acceptHeader("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    .doNotTrackHeader("1")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .acceptEncodingHeader("gzip, deflate")
    .userAgentHeader("Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0")

  val scn =     scenario("BasicSimulation") .exec(
     http("request_1").post("some_action")
        .formUpload("key", "tests/resources/test1.csv")
        .bodyPart(RawFileBodyPart("file", "tests/resources/unicode_file").contentType("application/binary").fileName("unicode_file")).asMultipartForm
        .body(RawFileBody("tests/resources/json/blazemeter-api-user.json"))
        .body(RawFileBodyPart("tests/resources/json/get-post.json"))
        .body(RawFileBodyPart("file_name", "tests/resources/json/passfail.json"))
        .body(ELFileBodyPart("tests/resources/json/mock_start_err.json"))

        csv("tests/resources/json/gatling_" + "production" + ".json")
        csv("tests/resources/json/gatling.json")
        tsv("tests/resources/json/get-post.json")
        ssv("tests/resources/json/merge2.json")
        separatedValues("tests/resources/json/reporting.json", "#")
        jsonFile("tests/resources/json/mock_normal.json")

      )

  setUp(
    scn.inject(atOnceUsers(10))
  ).protocols(httpConf)
}