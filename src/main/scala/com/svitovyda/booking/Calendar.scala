package com.svitovyda.booking

import java.time.{LocalDate, LocalDateTime, LocalTime}

import com.svitovyda.booking.Calendar._

import scala.collection.immutable.ListMap


case class Calendar(workingHours: TimeRange, meetings: List[Meeting] = List()) { // TODO: use TreeSet instead of List
  def validateMeeting(time: Period): Boolean = {
    time.isInRange(workingHours.toPeriod(time.start.toLocalDate)) &&
      meetings.forall(!_.time.intersectsWith(time))
  }

  def + (meeting: Meeting): Calendar =
    if(validateMeeting(meeting.time))
      copy(meetings = (meeting :: meetings).sortBy(_.time.start))
    else this

  def byDay: ListMap[LocalDate, List[Meeting]] = ListMap(
    meetings.groupBy(_.time.start.toLocalDate).toList:_*)
}

object Calendar {

  case class Period(start: LocalDateTime, end: LocalDateTime) {
    require(end.isAfter(start))

    def containsDate(date: LocalDateTime): Boolean =
      date.isAfter(start) && date.isBefore(end)

    def isInRange(range: Period): Boolean =
      start.isAfter(range.start.minusNanos(1)) && end.isBefore(range.end.plusNanos(1))

    def intersectsWith(that: Period): Boolean =
      containsDate(that.start) || containsDate(that.end) ||
        that.start.isEqual(start) || that.end.isEqual(end) ||
        that.containsDate(start) // if `that` fully contains this
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


  case class Meeting(time: Period, employeeId: EmployeeId, requestDate: LocalDateTime)

}
