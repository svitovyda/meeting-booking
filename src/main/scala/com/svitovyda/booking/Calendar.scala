package com.svitovyda.booking

import com.svitovyda.booking.Calendar.Period


case class Calendar(workingHours: Period) {

}

object Calendar {

  case class Period(start: Date, end: Date) {
    require(end.isAfter(start))

  }

  object Period {
    def apply(start: Date, duration: Double): Period = Period(
      start, start.plusMinutes(Math.round(duration * 60)))
  }

}
