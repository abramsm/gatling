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
package com.excilys.ebi.gatling.app
import java.lang.System.currentTimeMillis

import scala.collection.immutable.TreeSet
import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable.{ MultiMap, HashMap }
import scala.tools.nsc.io.Directory

import org.joda.time.DateTime

import com.excilys.ebi.gatling.app.compiler.{ TxtCompilerSimulationLoader, ScalaCompilerSimulationLoader, ClasspathSimulationLoader }
import com.excilys.ebi.gatling.charts.config.ChartsFiles.activeSessionsFile
import com.excilys.ebi.gatling.charts.report.ReportsGenerator
import com.excilys.ebi.gatling.core.config.GatlingFiles
import com.excilys.ebi.gatling.core.config.GatlingConfiguration
import com.excilys.ebi.gatling.core.log.Logging
import com.excilys.ebi.gatling.core.runner.Runner
import com.excilys.ebi.gatling.core.util.DateHelper.printFileNameDate
import com.excilys.ebi.gatling.core.util.FileHelper.{ TXT_EXTENSION, SCALA_EXTENSION }
import com.excilys.ebi.gatling.core.Conventions

import scopt.OptionParser

/**
 * Object containing entry point of application
 */
object Gatling extends Logging {

	/**
	 * Entry point of Application
	 *
	 * @param args Arguments of the main method
	 */
	def main(args: Array[String]) {

		val options: Options = Options()

		val cliOptsParser = new OptionParser("gatling") {
			opt("nr", "no-reports", "Runs simulation but does not generate reports", { options.noReports = true })
			opt("ro", "reports-only", "<folderName>", "Generates the reports for the simulation in <folderName>", { v: String => options.reportsOnlyFolder = Some(v) })
			opt("cf", "config-file", "<fileName>", "Uses <fileName> as the configuration file", { v: String => options.configFileName = Some(v) })
			opt("df", "data-folder", "<folderName>", "Uses <folderName> as the folder where feeders are stored", { v: String => options.dataFolder = Some(v) })
			opt("rf", "results-folder", "<folderName>", "Uses <folderName> as the folder where results are stored", { v: String => options.resultsFolder = Some(v) })
			opt("bf", "request-bodies-folder", "<folderName>", "Uses <folderName> as the folder where request bodies are stored", { v: String => options.requestBodiesFolder = Some(v) })
			opt("sf", "simulations-folder", "<folderName>", "Uses <folderName> to discover simulations that could be run", { v: String => options.simulationFolder = Some(v) })
			opt("sp", "simulations-package", "<packageName>", "Uses <packageName> to start the simulations", { v: String => options.simulationPackage = Some(v) })
			opt("s", "simulations", "<simulationNames>", "Runs the <simulationNames> sequentially", { v: String => options.simulations = Some(v.split(",").toList) })
		}

		if (cliOptsParser.parse(args)) {
			start(options)
		}

		// if arguments are bad, usage message is displayed
	}

	def start(options: Options) = new Gatling(options: Options).launch
}

class Gatling(options: Options) extends Logging {

	// Initializes configuration
	GatlingConfiguration.setUp(options.configFileName, options.dataFolder, options.requestBodiesFolder, options.resultsFolder, options.simulationFolder)

	def launch {
		options.reportsOnlyFolder match {
			case Some(reportsOnlyFolder) => generateStats(reportsOnlyFolder)
			case None =>
				options.simulations match {
					case Some(simulations) =>
					case None => {
						val selectedFile = options.simulationPackage match {
							case Some(simulationPackage) => selectSimulationFromSourceFolder(simulationPackage)
							case None => selectSimulationFileFromFileSystem
						}
						run(loadSimulations(selectedFile): _*)
					}
				}
		}
	}

	private def selectSimulationFileFromFileSystem: String = {

		// Getting files in scenarios folder
		val files = Directory(GatlingFiles.simulationsFolder).files.map(_.name).filter(name => name.endsWith(TXT_EXTENSION) || name.endsWith(SCALA_EXTENSION)).filterNot(_.startsWith(".")).toSeq

		// Sorting file names by radical and storing groups for display purpose
		val sortedFiles = new HashMap[String, MSet[String]] with MultiMap[String, String]
		var sortedGroups = new TreeSet[String]

		for (fileName <- files) {
			Conventions.getSourceDirectoryNameFromRootFileName(fileName).map { sourceDirectoryName =>
				sortedFiles.addBinding(sourceDirectoryName, fileName)
				sortedGroups += sourceDirectoryName
			}
		}

		// We get the folder name of the run simulation
		files.size match {
			case 0 =>
				// If there is no simulation file
				logger.error("There are no simulation scripts. Please verify that your scripts are in user-files/simulations and that they do not start with a .")
				sys.exit
			case 1 =>
				// If there is only one simulation file
				logger.info("There is only one simulation, executing it.")
				files(0)
			case _ =>
				// If there are several simulation files
				println("Which simulation do you want to execute ?")

				var i = 0
				var filesList: List[String] = Nil

				for (group <- sortedGroups) {
					println("\n - " + group)
					sortedFiles.get(group).map {
						for (fileName <- _) {
							Conventions.getSimulationSpecificName(fileName).map { simulationSpecificName =>
								println("     [" + i + "] " + simulationSpecificName)
								filesList = fileName :: filesList
								i += 1
							}
						}
					}
				}

				println("\nSimulation #: ")

				val selection = Console.readInt
				filesList.reverse(selection)
		}
	}

	private def selectSimulationFromSourceFolder(simulationPackage: String): String = {

		println("Which simulation do you want to execute ?")

		val files = Directory(GatlingFiles.simulationsFolder).files.map(_.name.takeWhile(_ != '.')).toList

		for (i <- 0 until files.size) {
			println("   [" + i + "] " + files(i))
		}

		val selection = Console.readInt
		simulationPackage + "." + files(selection)
	}

	/**
	 * This method call the statistics module to generate the charts and statistics
	 *
	 * @param folderName The folder from which the simulation.log will be parsed
	 * @return Nothing
	 */
	private def generateStats(folderName: String) {
		println("Generating reports...")
		val start = currentTimeMillis
		if (ReportsGenerator.generateFor(folderName)) {
			println("Reports generated in " + (currentTimeMillis - start) / 1000 + "s.")
			println("Please open the following file : " + activeSessionsFile(folderName))
		} else {
			println("Reports weren't generated")
		}
	}

	/**
	 * This method actually runs the simulation by interpreting the scripts.
	 *
	 * @param fileName The name of the simulation file that will be executed
	 * @return The name of the folder of this simulation (ie: its date)
	 */
	private def loadSimulations(fileNames: String*) = {
		val size = fileNames.size

		for (i <- 0 until size) yield {
			val fileName = fileNames(i)
			println(">> Running simulation (" + (i + 1) + "/" + size + ") - " + fileName)
			println("Simulation " + fileName + " started...")

			val simulationLoader =
				fileName match {
					case fn if (fn.endsWith(SCALA_EXTENSION)) => new ScalaCompilerSimulationLoader
					case fn if (fn.endsWith(TXT_EXTENSION)) => new TxtCompilerSimulationLoader
					case _ => new ClasspathSimulationLoader
				}

			simulationLoader(fileName)
		}
	}

	def run(simulations: GatlingSimulation*) {
		simulations.foreach { simulation =>
			val startDate = DateTime.now
			new Runner(startDate, simulation()).run
			println("Simulation Finished.")

			// Returns the folderName in which the simulation is stored
			if (!options.noReports) {
				generateStats(printFileNameDate(startDate))
			}
		}
	}
}
