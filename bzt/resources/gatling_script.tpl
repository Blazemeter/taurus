// generated automatically by Taurus

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class %(class_name)s extends Simulation {
    val _t_concurrency = Integer.getInteger("concurrency", 1).toInt
    val _t_ramp_up = Integer.getInteger("ramp-up", 0).toInt
    val _t_hold_for = Integer.getInteger("hold-for", 0).toInt
    val _t_throughput = Integer.getInteger("throughput")
    val _t_iterations = Integer.getInteger("iterations")

    val _duration = _t_ramp_up + _t_hold_for

    var httpConf = %(httpConf)s
    var _scn = scenario("Taurus Scenario")

    var _exec = %(_exec)s

    if (_t_iterations == null)
        _scn = _scn.forever{_exec}
     else
        _scn = _scn.repeat(_t_iterations.toInt){_exec}

    val _users =
        if (_t_ramp_up > 0)
            rampUsers(_t_concurrency) over (_t_ramp_up seconds)
        else
            atOnceUsers(_t_concurrency)

    var _setUp = setUp(_scn.inject(_users).protocols(httpConf))

    if (_t_throughput != null)
        _setUp = _setUp.throttle(
          reachRps(_t_throughput) in (_t_ramp_up),
          holdFor(Int.MaxValue))

    if (_duration > 0)
        _setUp.maxDuration(_duration)
}
