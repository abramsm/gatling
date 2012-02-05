/**
 * Copyright 2011-2012 eBusiness Information, Groupe Excilys (www.excilys.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.excilys.ebi.gatling.http.check.body
import com.excilys.ebi.gatling.core.check.CheckContext.{ setAndReturnCheckContextAttribute, getCheckContextAttribute }
import com.excilys.ebi.gatling.core.check.extractor.ExtractorFactory
import com.excilys.ebi.gatling.core.check.extractor.{ XPathExtractor, MultiXPathExtractor }
import com.excilys.ebi.gatling.core.check.CheckOneBuilder
import com.excilys.ebi.gatling.core.check.CheckMultipleBuilder
import com.excilys.ebi.gatling.core.session.Session
import com.excilys.ebi.gatling.core.util.StringHelper.interpolate
import com.excilys.ebi.gatling.http.check.{ HttpMultipleCheckBuilder, HttpCheck }
import com.excilys.ebi.gatling.http.request.HttpPhase.CompletePageReceived
import com.ning.http.client.Response

import HttpBodyXPathCheckBuilder.HTTP_RESPONSE_BODY_DOCUMENT_CHECK_CONTEXT_KEY

object HttpBodyXPathCheckBuilder {

	val HTTP_RESPONSE_BODY_DOCUMENT_CHECK_CONTEXT_KEY = "httpResponseBodyDocument"

	def xpath(what: Session => String) = new HttpBodyXPathCheckBuilder(what)

	def xpath(expression: String): HttpBodyXPathCheckBuilder = xpath(interpolate(expression))
}

/**
 * This class builds a response body check based on XPath expressions
 *
 * @param what the function returning the expression representing what is to be checked
 * @param strategy the strategy used to check
 * @param expected the expected value against which the extracted value will be checked
 * @param saveAs the optional session key in which the extracted value will be stored
 */
class HttpBodyXPathCheckBuilder(what: Session => String) extends HttpMultipleCheckBuilder[String](what, CompletePageReceived) {

	def getResponseBody(response: Response) = getCheckContextAttribute(HTTP_RESPONSE_BODY_DOCUMENT_CHECK_CONTEXT_KEY).getOrElse {
		setAndReturnCheckContextAttribute(HTTP_RESPONSE_BODY_DOCUMENT_CHECK_CONTEXT_KEY, XPathExtractor.parser.parse(response.getResponseBodyAsStream))
	}

	def find: CheckOneBuilder[HttpCheck[String], Response, String] = find(0)

	def find(occurence: Int) = new CheckOneBuilder(checkBuildFunction[String], new ExtractorFactory[Response, String] {
		def getExtractor(response: Response) = new XPathExtractor(getResponseBody(response), occurence)
	})

	def findAll = new CheckMultipleBuilder(checkBuildFunction[List[String]], new ExtractorFactory[Response, List[String]] {
		def getExtractor(response: Response) = new MultiXPathExtractor(getResponseBody(response))
	})
}
