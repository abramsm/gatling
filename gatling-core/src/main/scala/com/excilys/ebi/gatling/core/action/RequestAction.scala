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
package com.excilys.ebi.gatling.core.action

import com.excilys.ebi.gatling.core.action.request.Request
import com.excilys.ebi.gatling.core.session.Session
import com.excilys.ebi.gatling.core.feeder.Feeder
import com.excilys.ebi.gatling.core.check.CheckBuilder
import akka.actor.ActorRef
import com.excilys.ebi.gatling.core.config.ProtocolConfigurationRegistry
import com.excilys.ebi.gatling.core.config.ProtocolConfiguration
import com.excilys.ebi.gatling.core.check.Check
import com.excilys.ebi.gatling.core.check.CheckBuilder

/**
 * Abstract class for all request actions. For example HTTPRequestAction, and later LDAPRequestAction, etc.
 *
 * @param next action that will be executed after the request
 * @param request request that will be sent
 * @param givenProcessors a list of processors that will apply on the response
 * @param protocolConfiguration the optional protocolConfiguration for this type of request
 */
abstract class RequestAction[C <: Check[R, _], R, P <:ProtocolConfiguration](next: ActorRef, request: Request, givenProcessors: Option[List[C]], protocolConfiguration: Option[P]) extends Action {
	def execute(session: Session)
}