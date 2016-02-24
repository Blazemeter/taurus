// generated automatically by Taurus

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class %(class_name)s extends Simulation {
	val _t_concurrency = Integer.getInteger("concurrency", 1).toInt
	val _t_ramp_up = Integer.getInteger("ramp-up", 0).toInt
	val _t_hold_for = Integer.getInteger("hold-for", 0).toInt
	val _t_iterations = Integer.getInteger("iterations")

	var httpConf = %(httpConf)s
	var _scn = scenario("Taurus Scenario")

	var _exec = %(_exec)s

	if (_t_iterations == null) {
		_scn = _scn.forever{_exec}
		setUp(_scn.inject(rampUsers(_t_concurrency) over (_t_ramp_up))
			.protocols(httpConf)).maxDuration(_t_hold_for + _t_ramp_up)
	} else {
		_scn = _scn.repeat(_t_iterations.toInt){_exec}
		setUp(_scn.inject(rampUsers(_t_concurrency) over (_t_ramp_up))
			.protocols(httpConf))
	}
}
