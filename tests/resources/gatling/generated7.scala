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

  var httpConf = http.baseUrl("http://example.com")

  var testScenario = scenario("Taurus Scenario")

  var execution = exec(
    _.set("foo", "bar")
  ).exec(
    http("/${foo}").get("http://example.com/${foo}")
  ).exec(
    _.set("foo", "bar1").set("foo2", "bar2")
  ).exec(
    http("/#{foo}/#{foo2}").get("http://example.com/#{foo}/#{foo2}")
  ).exec(
    _.set("hanging", "loose\"'")
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
