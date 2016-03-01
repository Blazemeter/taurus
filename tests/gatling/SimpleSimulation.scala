import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class SimpleSimulation extends Simulation { 

  val conf = http.baseUrl("http:/blazedemo.com/")

  val scn = scenario("Simple scenario")
            .exec(http("req1").get("/")).pause(2)

  setUp(scn.inject(atOnceUsers(5))).protocols(conf)
}
