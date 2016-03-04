// generated automatically by Taurus

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class TaurusSimulation_140280980933712 extends Simulation {
	val _t_concurrency = Integer.getInteger("concurrency", 1).toInt
	val _t_ramp_up = Integer.getInteger("ramp-up", 0).toInt
	val _t_hold_for = Integer.getInteger("hold-for", 0).toInt
	val _t_iterations = Integer.getInteger("iterations")

	val _duration = _t_ramp_up + _t_hold_for

	var httpConf = http.baseURL("http://blazedemo.com")

	var _scn = scenario("Taurus Scenario")

	var _exec = exec(
			http("/reserve.php").get("/reserve.php")
				.check(
					regex("""boot(.*)strap.min""").exists,
					status.in(200 to 304),
					status.not(300),
					status.not(301),
					status.not(302),
					status.not(302))
		).pause(0)

	if (_t_iterations == null)
		_scn = _scn.forever{_exec}
	 else
		_scn = _scn.repeat(_t_iterations.toInt){_exec}

	var _setUp = setUp(_scn.inject(rampUsers(_t_concurrency) over (_t_ramp_up))
			.protocols(httpConf))

	if (_duration > 0)
		_setUp.maxDuration(_duration)
}
