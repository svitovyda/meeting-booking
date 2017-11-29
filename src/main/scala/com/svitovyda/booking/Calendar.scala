package com.svitovyda.booking

import java.time.{LocalDate, LocalDateTime, LocalTime}

import com.svitovyda.booking.Calendar._


case class Calendar(workingHours: TimeRange, meetings: List[Meeting] = List()) { // TODO: use TreeSet instead of List
  def + (meeting: Meeting): Calendar =
    copy(meetings = (meeting :: meetings))//.sortBy(_.time.start))
}

object Calendar {

  case class Period(start: LocalDateTime, end: LocalDateTime) {
    require(end.isAfter(start))

  }
  object Period {
    def apply(start: LocalDateTime, duration: Double): Period = Period(
      start, start.plusMinutes(Math.round(duration * 60)))
  }

  case class TimeRange(start: LocalTime, end: LocalTime) {
    require(end.isAfter(start))

    def toPeriod(date: LocalDate): Period = Period(
      LocalDateTime.of(date, start), LocalDateTime.of(date, end))
  }


  case class Meeting(time: Period, userId: EmployeeId)

}
