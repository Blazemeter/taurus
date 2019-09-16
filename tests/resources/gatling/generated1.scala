// generated automatically by Taurus

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class SIMNAME extends Simulation {
  val concurrency = Integer.getInteger("concurrency", 1).toInt
  val rampUpTime = Integer.getInteger("ramp-up", 0).toInt
  val holdForTime = Integer.getInteger("hold-for", 0).toInt
  val throughput = Integer.getInteger("throughput")
  val iterationLimit = Integer.getInteger("iterations")

  val durationLimit = rampUpTime + holdForTime

  var httpConf = http.baseURL("http://blazedemo.com")
    .header("H1", "V1")

  var testScenario = scenario("Taurus Scenario")

  var execution = exec(
    http("/reserve.php").post("/reserve.php")
      .header("H2", "V2")
      .body(StringBody("""{"com": {"pli": {"cat": ["ed", "dict"]}}}"""))
      .check(
        substring("""bootstrap.min""").notExists
      )
      .disableFollowRedirect
  ).pause(1).exec(
    http("/").get("/")
  ).pause(2).exec(
    http("/reserve.php").post("/reserve.php")
      .body(StringBody("""Body Content 日本語"""))
      .disableFollowRedirect
  ).pause(1).exec(
    http("/something.php").post("/something.php")
      .formParam("param_name1", "param_value1")
      .formParam("param_name2", "param_value2")
      .disableFollowRedirect
  ).pause(1).exec(
    http("/something_else.php").post("/something_else.php")
      .header("Content-Type", "application/json")
      .body(StringBody("""{"param_name3": "param_value4"}"""))
      .disableFollowRedirect
  ).pause(1)

  if (iterationLimit == null)
    testScenario = testScenario.forever{execution}
  else
    testScenario = testScenario.repeat(iterationLimit.toInt){execution}

  val virtualUsers =
    if (rampUpTime > 0)
      rampUsers(concurrency) over (rampUpTime seconds)
    else
      atOnceUsers(concurrency)

  var testSetup = setUp(testScenario.inject(virtualUsers).protocols(httpConf))

  if (throughput != null)
    testSetup = testSetup.throttle(
      reachRps(throughput) in (rampUpTime),
      holdFor(Int.MaxValue)
    )

  if (durationLimit > 0)
    testSetup.maxDuration(durationLimit)
}
