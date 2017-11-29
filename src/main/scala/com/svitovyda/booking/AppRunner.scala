package com.svitovyda.booking

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.svitovyda.booking.Calendar.Meeting

import scala.collection.immutable.ListMap

object AppRunner {
  def main(args: Array[String]): Unit = {
    if(args.nonEmpty) // TODO: add validation of filename
      RequestsProcessor.parseStream(FileReaderService.readFile(args.head)) match {
        case Right(calendar) => printMeetingsByDays(calendar.byDay)
        case Left(error) => println(error)
      }
    else println("Could not parse the file name, try again!")
  }

  def printMeetingsByDays(meetings: ListMap[LocalDate, List[Meeting]]): Unit =
    meetings.foreach { case (key, list) =>
      println(key.format(DateTimeFormatter.ofPattern("yyyy-MM-dd")))
      list.foreach { meeting =>
        val from = meeting.time.start.format(DateTimeFormatter.ofPattern("HH:mm"))
        val to = meeting.time.end.format(DateTimeFormatter.ofPattern("HH:mm"))
        println(s"$from $to ${meeting.employeeId.value}")
      }
      println()
    }
}
