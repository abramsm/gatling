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
package com.excilys.ebi.gatling.core.util
import java.io.{ File => JFile }
import java.net.{ URI, JarURLConnection }

import scala.collection.JavaConversions.asIterator
import scala.tools.nsc.io.Path.{ string2path, jfile2path }
import scala.tools.nsc.io.{ Path, Jar, Fileish, File }

import com.excilys.ebi.gatling.core.util.StringHelper.EMPTY

object ScanHelper {

	def packageToStringPath(pkg: String): String = pkg.replace(".", File.separator)

	def getPackageResources(pkg: String, deep: Boolean): Seq[Resource] = {
		asIterator(getClass.getClassLoader.getResources(pkg)).map { packageURL =>
			packageURL.getProtocol match {
				case "file" =>
					val rootDir = File(new JFile(new URI(packageURL.toString).getSchemeSpecificPart)).toDirectory
					val files = if (deep) rootDir.deepFiles else rootDir.files
					files.map(new FileResource(_))

				case "jar" =>
					val connection = packageURL.openConnection.asInstanceOf[JarURLConnection]
					val rootDir: Path = connection.getJarEntry.getName
					val jar = new Jar(File(new JFile(connection.getJarFileURL.toURI)))
					jar.fileishIterator.filter(_.path.extension != EMPTY).filter(fileish => {
						if (deep)
							fileish.path.startsWith(rootDir)
						else
							fileish.parent == rootDir
					}).map(new FileishResource(_))

				case _ => throw new UnsupportedOperationException
			}
		}.toList.flatten
	}

	def deepCopyPackageContent(pkg: String, targetDirectoryPath: Path) {

		def getPathAfterPackage(path: Path, pkg: String): String = path.toString.split(packageToStringPath(pkg))(1)

		getPackageResources(pkg, true).foreach { resource =>
			val target = targetDirectoryPath / getPathAfterPackage(resource.path, pkg)
			resource.copyTo(target)
		}
	}
}

trait Resource {
	def path: Path
	def copyTo(target: Path)
}

case class FileResource(file: File) extends Resource {
	def path = file.path
	def copyTo(target: Path) {
		target.parent.createDirectory()
		file.copyTo(target, true)
	}
}

case class FileishResource(fileish: Fileish) extends Resource {
	def path = fileish.path
	def copyTo(target: Path) {
		target.parent.createDirectory()
		val input = fileish.input()
		val output = target.toFile.outputStream(false)
		IOHelper.copy(input, output)
	}
}