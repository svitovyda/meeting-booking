package com.svitovyda.booking

import com.svitovyda.booking.Calendar.{Meeting, Period}


case class Calendar(workingHours: Period, meetings: List[Meeting]) {

}

object Calendar {

  case class Period(start: Date, end: Date) {
    require(end.isAfter(start))

  }
  object Period {
    def apply(start: Date, duration: Double): Period = Period(
      start, start.plusMinutes(Math.round(duration * 60)))
  }

  case class Meeting(time: Period, userId: String)

}
