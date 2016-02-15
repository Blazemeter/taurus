// generated automatically by Taurus

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class TaurusSimulation extends Simulation {

	val _t_concurrency = Integer.getInteger("concurrency", 1).toInt
	val _t_ramp_up = Integer.getInteger("ramp-up", 0).toInt
	val _t_hold_for = Integer.getInteger("hold-for", 0).toInt
	val _t_iterations = Integer.getInteger("iterations")
	val _t_think_time = Integer.getInteger("think-time", 0).toInt

	val httpConf = http.baseURL("http://blazedemo.com")
		.header("SH1", "SV1")
		.header("SH2", "SV2")

	var _scn = scenario("Taurus Scenario")

	var _exec = exec(
			http("/reserve.php").post("/reserve.php")
				.header("RH1", "RV1")
				.header("RH2", "RV2")
		).pause(_t_think_time).exec(
			http("/").get("/")
		).pause(_t_think_time)

	if (_t_iterations == null)
		_scn = _scn.forever{_exec}
	else
		_scn = _scn.repeat(_t_iterations.toInt){_exec}

	var _scn_pop = _scn.inject(rampUsers(_t_concurrency) over (_t_ramp_up))
	setUp(_scn_pop.protocols(httpConf)).maxDuration(_t_hold_for + _t_ramp_up)

}
