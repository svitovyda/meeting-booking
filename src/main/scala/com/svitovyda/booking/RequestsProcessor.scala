package com.svitovyda.booking

import java.time.LocalDateTime
import java.time.LocalTime
import java.time.format.DateTimeFormatter

import com.svitovyda.booking.Calendar.{Meeting, Period, TimeRange}

import scala.util.Try

object RequestsProcessor {

  // contain minimal validation
  private val hours = "[0-2]\\d"
  private val minOrSec = "[0-5]\\d"
  private val headerTime = s"($hours)($minOrSec)"
  private val trim = "[\\t ]*"
  private val year = "[12]\\d\\d\\d"
  private val month = "[01]\\d"
  private val day = "[0-3]\\d"
  private val duration = "\\d{1,2}.?\\d?"
  private val userId = "[\\d\\w_-]+"

  private val headerPattern = s"$trim$headerTime $headerTime$trim".r
  private val requestDataPattern = s"$trim($year-$month-$day $hours:$minOrSec:$minOrSec) ($userId)$trim".r
  private val requestMeetingPattern = s"$trim($year-$month-$day $hours:$minOrSec) ($duration)$trim".r

  val TimestampFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  val MeetingFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def parseHeader(input: String): Try[TimeRange] = Try {
    val (start, end) = input match {
      case headerPattern(startH, startM, endH, endM) =>
        (LocalTime.of(startH.toInt, startM.toInt), LocalTime.of(endH.toInt, endM.toInt))
      case _ => throw new RuntimeException(s"Could not parse header: $input")
    }
    TimeRange(start, end)
  }

  def parseRequest(line1: String, line2: String): Validation[Meeting] =
    Try {
      val (timestamp, employee) = line1 match {
        case requestDataPattern(t, e) => (t, e)
        case _ => throw new RuntimeException(s"Could not parse request date: $line1")
      }

      val dateRequest = LocalDateTime.parse(timestamp, TimestampFormatter)
      val employeeId = EmployeeId(employee)

      val (time, durationInput) = line2 match {
        case requestMeetingPattern(t, d) => (t, d)
        case _ => throw new RuntimeException(s"Could not parse request date: $line1")
      }

      val dateMeeting = LocalDateTime.parse(time, MeetingFormatter)
      val duration = durationInput.toDouble
      require(duration < 12)

      Right(Meeting(Period(dateMeeting, duration), employeeId, dateRequest))
    }.recover { case e =>
      Left[Error, Meeting](s"Could not parse meeting request $line1 $line2: ${e.getMessage}")
    }.get

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
