package com.svitovyda.booking

import java.time.LocalDateTime
import java.time.LocalTime

import com.svitovyda.booking.Calendar.TimeRange

import scala.util.Try

object RequestsProcessor {

  val HeaderPattern = "[\\t ]*([0-2][0-9])([0-5][0-9]) ([0-2][0-9])([0-5][0-9])[\\t ]*".r

  def parseHeader(input: String): Try[TimeRange] = Try {
    val (start, end) = input match {
      case HeaderPattern(startH, startM, endH, endM) =>
        (LocalTime.of(startH.toInt, startM.toInt), LocalTime.of(endH.toInt, endM.toInt))
      case _ => throw new RuntimeException(s"Could not parse header: $input")
    }
    TimeRange(start, end)
  }

  def createCalendar(header: String, lines: Seq[String]): Validation[Calendar] =
    parseHeader(header).map { timeRange =>
      Right(Calendar(timeRange))
    }.recover { case e: Exception =>
      Left(s"Could not create calendar: ${e.getMessage}")
    }.get
}
