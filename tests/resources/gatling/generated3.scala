// generated automatically by Taurus

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class TaurusSimulation_139661999666512 extends Simulation {
  val concurrency = Integer.getInteger("concurrency", 1).toInt
  val rampUpTime = Integer.getInteger("ramp-up", 0).toInt
  val holdForTime = Integer.getInteger("hold-for", 0).toInt
  val throughput = Integer.getInteger("throughput")
  val iterationLimit = Integer.getInteger("iterations")

  val durationLimit = rampUpTime + holdForTime

  var httpConf = http.baseURL("")

  var _scn = scenario("Taurus Scenario")

  var _exec = exec(
    http("http://site.com/reserve.php").get("http://site.com/reserve.php")
      .check(
        status.is(200)
      )
  )

  if (iterationLimit == null)
    _scn = _scn.forever{_exec}
  else
    _scn = _scn.repeat(iterationLimit.toInt){_exec}

  val _users =
    if (rampUpTime > 0)
      rampUsers(concurrency) over (rampUpTime seconds)
    else
      atOnceUsers(concurrency)

  var _setUp = setUp(_scn.inject(_users).protocols(httpConf))

  if (throughput != null)
    _setUp = _setUp.throttle(
      reachRps(throughput) in (rampUpTime),
      holdFor(Int.MaxValue)
    )

  if (durationLimit > 0)
    _setUp.maxDuration(durationLimit)
}
