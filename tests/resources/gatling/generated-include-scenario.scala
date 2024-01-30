// generated automatically by Taurus

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class GeneratedIncludeScenario extends Simulation {
  val concurrency = Integer.getInteger("concurrency", 1).toInt
  val rampUpTime = Integer.getInteger("ramp-up", 0).toInt
  val holdForTime = Integer.getInteger("hold-for", 0).toInt
  val throughput = Integer.getInteger("throughput")
  val iterationLimit = Integer.getInteger("iterations")

  val durationLimit = rampUpTime + holdForTime

  var httpConf = http.baseUrl("http://blazemeter.com")

  var testScenario = scenario("Taurus Scenario")

  var execution = exec(
    http("IndexPage_login").get("http://blazemeter.com/login")
      .header("X-Info", "foo=fooheader")
      .check(
        regex("_ajaxKey=\"(.+?)\"")
        .ofType[(String)].withDefault("NOT FOUND")
      .saveAs("l_ajaxKey")
      )
      .check(
        jmesPath("$.jsonpath[0]")
        .ofType[(String)].withDefault("NOT_FOUND")
      .saveAs("varname")
      )
      .check(
        xpath("/order/client/address").withDefault("NOT_FOUND")
      .saveAs("varname2")
      )
      .check(
        css("input[name=Gatling]").withDefault("NOT_FOUND")
      .saveAs("varname1")
      )
  ).exec(
    http("login").post("http://blazemeter.com/login?_s.token=#{l_ajaxKey}")
      .header("X-Info", "foo=fooheader")
      .check(
        status.is(200)
      )
  ).exec(
    http("Post_login").post("http://blazemeter.com/login?_s.crb=#{l_ajaxKey}")
      .header("X-Info", "foo=fooheader")
      .formParam("password", "********")
      .formParam("passwordHints", "Password")
      .formParam("username", "user")
      .check(
        regex("JSESSIONID=(.+);")
        .ofType[(String)].withDefault("NOT FOUND")
      .saveAs("httpSessionId")
      )
      .check(
        regex("_ajaxKey=\"(.+?)\"")
        .ofType[(String)].withDefault("NOT FOUND")
      .saveAs("sCrb")
      )
      .check(
        regex("_ajaxKey=\"(.+?)\"")
        .ofType[(String)].withDefault("NOT FOUND")
      .saveAs("xAjaxToken")
      )
      .check(
        substring("""#{xAjaxToken}""").exists
      )
  ).exec(
    http("/demo").get("http://blazemeter.com/demo")
  ).exec(
    http("Logout").get("http://blazemeter.com/logout")
  ).exec(
    http("IndexPage_login").get("http://blazemeter.com/login")
      .header("X-Info", "foo=fooheader")
      .check(
        regex("_ajaxKey=\"(.+?)\"")
        .ofType[(String)].withDefault("NOT FOUND")
      .saveAs("l_ajaxKey")
      )
      .check(
        jmesPath("$.jsonpath[0]")
        .ofType[(String)].withDefault("NOT_FOUND")
      .saveAs("varname")
      )
      .check(
        xpath("/order/client/address").withDefault("NOT_FOUND")
      .saveAs("varname2")
      )
      .check(
        css("input[name=Gatling]").withDefault("NOT_FOUND")
      .saveAs("varname1")
      )
  ).exec(
    http("login").post("http://www.blazemeter.com/login?_s.token=#{l_ajaxKey}")
      .header("X-Info", "foo=fooheader")
      .check(
        status.is(200)
      )
  ).exec(
    http("Post_login").post("http://www.blazemeter.com/login?_s.crb=#{l_ajaxKey}")
      .header("X-Info", "foo=fooheader")
      .formParam("password", "********")
      .formParam("passwordHints", "Password")
      .formParam("username", "user")
      .check(
        regex("JSESSIONID=(.+);")
        .ofType[(String)].withDefault("NOT FOUND")
      .saveAs("httpSessionId")
      )
      .check(
        regex("_ajaxKey=\"(.+?)\"")
        .ofType[(String)].withDefault("NOT FOUND")
      .saveAs("sCrb")
      )
      .check(
        regex("_ajaxKey=\"(.+?)\"")
        .ofType[(String)].withDefault("NOT FOUND")
      .saveAs("xAjaxToken")
      )
      .check(
        substring("""#{xAjaxToken}""").exists
      )
  ).exec(
    http("Logout").get("http://www.blazemeter.com/logout")
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
