package mytest

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class BasicSimulation extends Simulation {

  val httpConf = http
    .baseURL("http://192.168.25.8/1.html")
    .acceptHeader("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    .doNotTrackHeader("1")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .acceptEncodingHeader("gzip, deflate")
    .userAgentHeader("Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0")

  val scn =     scenario("BasicSimulation") .exec(
          http("request_1").post("some_action")
          .formUpload("key", "tests/data/test1.csv")
          .bodyPart(RawFileBodyPart("file", "tests/data/unicode_file").contentType("application/binary").fileName("unicode_file")).asMultipartForm
          .body(RawFileBody("tests/json/dummy.json"))
          .body(ELFileBody("tests/json/includes.json"))
          .body(RawFileBodyPart("tests/json/local.json"))
          .body(RawFileBodyPart("file_name", "tests/json/passfail.json"))
          .body(ELFileBodyPart("tests/json/mock_start_err.json"))
          .body(ELFileBodyPart("file_name", "tests/json/throughput.json"))

          csv("tests/json/gatling.json")
          tsv("tests/json/get-post.json")
          ssv("tests/json/merge2.json")
          separatedValues("tests/json/reporting.json", "#")
          jsonFile("tests/postproc_err/json")

      )

  setUp(
    scn.inject(atOnceUsers(10))
  ).protocols(httpConf)
}