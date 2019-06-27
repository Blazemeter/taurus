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

  var httpConf = http.baseUrl("")

  var testScenario = scenario("Taurus Scenario")

  val test1Feed = separatedValues("test1.csv", ',')
  val test2Feed = csv("test2.csv").circular
  val test2Feed_1 = csv("test2.csv").circular

  var execution = feed(test1Feed).feed(test2Feed).feed(test2Feed_1).exec(
    http("http://blazedemo.com/?tag=${col1}").get("http://blazedemo.com/?tag=${col1}")
  )

  if (iterationLimit == null)
    testScenario = testScenario.forever{execution}
  else
    testScenario = testScenario.repeat(iterationLimit.toInt){execution}

  val virtualUsers =
    if (rampUpTime > 0)
      rampUsers(concurrency) during (rampUpTime seconds)
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
