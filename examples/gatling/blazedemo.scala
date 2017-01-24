package examples.gatling
 
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._
 
class blazedemo extends Simulation {
  // parse load profile from Taurus
  val iterations = Integer.getInteger("iterations", 100).toInt
  val concurrency = Integer.getInteger("concurrency", 10).toInt
  val rampUp = Integer.getInteger("ramp-up", 1).toInt
  val holdFor = Integer.getInteger("hold-for", 30).toInt
  val httpConf = http.baseURL("http://blazedemo.com/")
 
  // 'forever' means each thread will execute scenario until
  // duration limit is reached
  val loopScenario = scenario("Loop Scenario").forever() {
    exec(http("index").get("/"))
  }
 
  // if you want to set an iteration limit (instead of using duration limit),
  // you can use the following scenario
  val iterationScenario = scenario("Iteration Scenario").repeat(iterations) {
    exec(http("index").get("/"))
  }
 
  val execution = loopScenario
    .inject(rampUsers(concurrency) over rampUp)
    .protocols(httpConf)
 
  setUp(execution).maxDuration(rampUp + holdFor)
}
