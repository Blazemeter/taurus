// generated automatically by Taurus

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class TaurusSimulation extends Simulation {

    val _t_concurrency = Integer.getInteger("concurrency", 1).toInt
    val _t_ramp_up = Integer.getInteger("ramp-up", 0).toInt
    val _t_hold_for = Integer.getInteger("hold-for", 0).toInt
    val _t_iterations = Integer.getInteger("iterations")

    val httpConf = http.baseURL("%(addr)s")
        .header("%(key)s", "%(val)s")

    var _scn = scenario("Taurus Scenario")

    var _exec =
        exec(
            http("%(req_label)s").%(method)s("%(url)s")
                .header("%(key)s", "%(val)s")
                .body(%(method)s(""""%(body)s"""))
        ).pause(%(think_time)s)

        if (_t_iterations == null)
            _scn = _scn.forever{_exec}
        else
            _scn = _scn.repeat(_t_iterations.toInt){_exec}

    var _scn_pop = _scn.inject(rampUsers(_t_concurrency) over (_t_ramp_up))

    setUp(_scn_pop.protocols(httpConf)).maxDuration(_t_hold_for + _t_ramp_up)
}