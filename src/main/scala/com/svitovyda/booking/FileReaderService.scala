package com.svitovyda.booking

import scala.io.{BufferedSource, Source}
import scala.util.Try


object FileReaderService {
  def readFile(fileName: String): Validation[Seq[String]] =
    Try {
      Source.fromFile(fileName)
    }.map { bufferedSource: BufferedSource =>
      val lines = (for (line <- bufferedSource.getLines()) yield line).toList
      bufferedSource.close
      Right(lines)
    }.recover { case e =>
      Left(s"Error reading file $fileName: ${e.getMessage}")
    }.get
}
