package com.svitovyda.booking

import scala.io.{BufferedSource, Source}
import scala.util.Try


object FileReaderService {
  def readFile(fileName: String): Either[String, Seq[String]] =
    Try {
      Source.fromFile(fileName)
    }.map { bufferedSource: BufferedSource =>
      val lines = for (line <- bufferedSource.getLines()) yield line
      bufferedSource.close
      Right(lines.toSeq)
    }.recover { case e =>
      Left(s"Error reading file $fileName: ${e.getMessage}")
    }.get
}
