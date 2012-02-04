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
package com.excilys.ebi.gatling.core.check

import com.excilys.ebi.gatling.core.check.extractor.Extractor
import scala.collection.mutable.HashMap
import com.excilys.ebi.gatling.core.session.Session
import com.excilys.ebi.gatling.core.check.extractor.ExtractorFactory

object ResolvedCheck {

	private def buildExtractors[T](where: T, checks: List[Check[T, _]]) = {
		val extractors = new HashMap[ExtractorFactory[T, _], Extractor[_]]
		checks.foreach { check =>
			val extractorFactory = check.how
			if (extractors.get(extractorFactory).isEmpty)
				extractors += extractorFactory -> extractorFactory.getExtractor(where)
		}
		extractors
	}

	private def resolveChecks[T](s: Session, where: T, checks: List[Check[T, _]]) = {

		val extractors = buildExtractors(where, checks)
		checks.map { check =>
			val extractor = extractors.get(check.how).getOrElse(throw new IllegalArgumentException("Extractor should have been built"))
			check.resolve(s, extractor)
		}
	}

	private def applyChecks(s: Session, resolvedChecks: List[ResolvedCheck[_]]): (Session, CheckResult[_]) = {

		var newSession = s
		var lastCheckResult: CheckResult[_] = null

		for (resolvedCheck <- resolvedChecks) {
			lastCheckResult = resolvedCheck.check(s)
			if (!lastCheckResult.ok) {
				return (newSession, lastCheckResult)

			} else if (resolvedCheck.saveAs.isDefined) {
				newSession = newSession.setAttribute(resolvedCheck.saveAs.get, lastCheckResult.extractedValue)
			}
		}

		(newSession, lastCheckResult)
	}

	def resolveAndApplyChecks[T](s: Session, where: T, checks: List[Check[T, _]]) = {
		val resolvedChecks = resolveChecks(s, where, checks)
		applyChecks(s, resolvedChecks)
	}
}

class ResolvedCheck[X](val extractionExpression: String, val extractor: Extractor[X], val strategy: CheckStrategy[X], val saveAs: Option[String]) {

	/**
	 * This method performs the check via the strategy used by this Check
	 *
	 * @param value the value extracted from the T
	 * @return a CheckResult that indicates whether the check succeeded or not
	 */
	def check(s: Session) = strategy(extractor.extract(extractionExpression), s)
}