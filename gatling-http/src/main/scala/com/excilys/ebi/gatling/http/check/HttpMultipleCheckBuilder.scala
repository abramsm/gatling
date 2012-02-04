package com.excilys.ebi.gatling.http.check
import com.excilys.ebi.gatling.http.request.HttpPhase
import com.excilys.ebi.gatling.core.session.Session
import com.excilys.ebi.gatling.http.request.HttpPhase.HttpPhase
import com.excilys.ebi.gatling.core.check.MultipleOccurence
import com.ning.http.client.Response

abstract class HttpMultipleCheckBuilder[X](what: Session => String, when: HttpPhase) extends HttpCheckBuilder[X](what, when) with MultipleOccurence[HttpCheck[X], HttpCheck[List[X]], Response, X]