package com.svitovyda.booking

import java.time.LocalDateTime
import java.time.LocalTime

import com.svitovyda.booking.Calendar.{Meeting, TimeRange}

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

  def parseRequest(line1: String, line2: String): Validation[Meeting] =
    Left[Error, Meeting](s"Could not parse meeting request $line1 $line2")

  def parseRequests(lines: Seq[String]): List[Meeting] =
    lines
      .filter(_.trim.nonEmpty)
      .grouped(2)
      .foldLeft(List[Meeting]()) {
        case (z, List(l1, l2)) =>
          parseRequest(l1, l2) match {
            case Right(m) => m :: z
            case Left(e) => z
          }
        case (z, _) => z
      }

  def createCalendar(header: String, lines: Seq[String]): Validation[Calendar] =
    parseHeader(header).map { timeRange =>
      Right(Calendar(timeRange))
    }.recover { case e: Exception =>
      Left(s"Could not create calendar: ${e.getMessage}")
    }.get
}
