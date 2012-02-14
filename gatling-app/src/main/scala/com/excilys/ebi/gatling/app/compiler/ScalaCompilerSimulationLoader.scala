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
package com.excilys.ebi.gatling.app.compiler
import java.io.{ StringWriter, PrintWriter }
import java.util.regex.Pattern

import scala.io.Source
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.io.Path.string2path
import scala.tools.nsc.io.{ VirtualDirectory, PlainFile, Path, Directory, AbstractFile }
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{ Settings, Global }

import com.excilys.ebi.gatling.app.{ SimulationLoader, GatlingSimulation }
import com.excilys.ebi.gatling.core.config.GatlingFiles
import com.excilys.ebi.gatling.core.util.IOHelper.use
import com.excilys.ebi.gatling.core.util.PathHelper.path2jfile
import com.excilys.ebi.gatling.core.util.ReflectionHelper.getNewInstanceByClassName
import com.excilys.ebi.gatling.core.util.StringHelper.EMPTY
import com.excilys.ebi.gatling.core.Conventions

/**
 * This class is used to interpret scala simulations
 */
class ScalaCompilerSimulationLoader extends SimulationLoader {

	val byteCodeDir = new VirtualDirectory("memory", None)
	val classLoader = new AbstractFileClassLoader(byteCodeDir, getClass.getClassLoader)
	val packagePattern = Pattern.compile("package (.+)")
	val simpleClassNamePattern = Pattern.compile("class (.+) extends GatlingSimulation")
	/**
	 * This method launches the interpretation of the simulation and runs it
	 *
	 * @param fileName the name of the file containing the simulation description
	 * @param startDate the date at which the launch was asked
	 */
	def apply(fileName: String) = {
		val path = GatlingFiles.simulationsFolder / fileName
		compile(path)
		getNewInstanceByClassName[GatlingSimulation](getSimulationClassName(path), classLoader)
	}

	/**
	 * Compiles all the files needed for the simulation
	 *
	 * @param sourceDirectory the file containing the simulation description
	 */
	private def compile(sourceDirectory: Path) {

		// Attempt compilation
		val files = collectSourceFiles(sourceDirectory)

		// Prepare an object for collecting error messages from the compiler
		val messageCollector = new StringWriter

		use(new PrintWriter(messageCollector)) { pw =>
			// Initialize the compiler
			val settings = generateSettings
			val reporter = new ConsoleReporter(settings, Console.in, pw)
			val compiler = new Global(settings, reporter)

			(new compiler.Run).compileFiles(files)

			// Bail out if compilation failed
			if (reporter.hasErrors) {
				reporter.printSummary
				throw new RuntimeException("Compilation failed:\n" + messageCollector.toString)
			}
		}
	}

	protected def getSimulationClassName(sourceDirectory: Path) = {
		val content = Source.fromFile(sourceDirectory.getCanonicalFile).getLines.mkString("\n")

		val packageMatcher = packagePattern.matcher(content)
		val packageName = if (packageMatcher.find) packageMatcher.group(1) + "." else EMPTY

		val simpleClassNameMatcher = simpleClassNamePattern.matcher(content)
		val simpleClassName = if (simpleClassNameMatcher.find) simpleClassNameMatcher.group(1) else throw new IllegalArgumentException("Simulation file malformed : couldn't retrieve class name")

		packageName + simpleClassName
	}

	protected def collectSourceFiles(sourceDirectory: Path): List[AbstractFile] = {
		if (sourceDirectory.isFile) {
			val rootFile = PlainFile.fromPath(sourceDirectory)

			Conventions.getSourceDirectoryNameFromRootFileName(sourceDirectory.getAbsolutePath).map { sourceDirectoryName =>
				val dir = Directory(sourceDirectoryName)
				if (dir.exists)
					rootFile :: dir.walk.map(PlainFile.fromPath(_)).toList
				else
					List(rootFile)
			}.getOrElse(Nil)
		} else
			sourceDirectory.walk.map(PlainFile.fromPath(_)).toList
	}

	/**
	 * Generates the settings of the scala compiler
	 */
	private def generateSettings: Settings = {
		val settings = new Settings
		settings.usejavacp.value = true
		settings.outputDirs.setSingleOutput(byteCodeDir)
		settings.deprecation.value = true
		settings.unchecked.value = true
		settings
	}
}