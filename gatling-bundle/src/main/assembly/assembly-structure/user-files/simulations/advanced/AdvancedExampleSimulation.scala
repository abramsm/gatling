package advanced
import com.excilys.ebi.gatling.core.Predef._
import com.excilys.ebi.gatling.http.Predef._
import com.excilys.ebi.gatling.app.GatlingSimulation

class AdvancedExampleSimulation extends GatlingSimulation {

	def apply = {

		val urlBase = "http://excilys-bank-web.cloudfoundry.com"

		val httpConf = httpConfig.baseURL(urlBase)

		List(
			SomeScenario.scn.configure users 10 ramp 10 protocolConfig httpConf,
			SomeOtherScenario.otherScn.configure users 5 ramp 20 delay 30 protocolConfig httpConf)
	}
}
