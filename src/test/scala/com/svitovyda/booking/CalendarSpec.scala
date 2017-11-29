package com.svitovyda.booking

import java.time.{LocalDateTime, LocalTime}

import com.svitovyda.booking.Calendar.{Meeting, Period, TimeRange}
import org.scalatest.{MustMatchers, WordSpecLike}


class CalendarSpec extends WordSpecLike with MustMatchers {
  "Period" must {
    "containsDate()" in {
      val date = LocalDateTime.now
      val period = Period(date.minusMinutes(10), date.plusMinutes(10))
      period.containsDate(date) must be (true)
      period.containsDate(date.minusDays(1)) must be (false)
      period.containsDate(date.plusDays(1)) must be (false)
    }

    "isInRange()" in {
      val date = LocalDateTime.now
      val range = Period(date.minusMinutes(10), date.plusMinutes(10))

      range.isInRange(range) must be (true)
      Period(date.minusMinutes(5), date.plusMinutes(5)).isInRange(range) must be (true)

      Period(date.minusMinutes(10), date.plusMinutes(11)).isInRange(range) must be (false)
      Period(date.minusMinutes(11), date.plusMinutes(10)).isInRange(range) must be (false)
      Period(date.minusMinutes(20), date.minusMinutes(11)).isInRange(range) must be (false)
      Period(date.plusMinutes(11), date.plusMinutes(20)).isInRange(range) must be (false)
    }

    "intersectsWith()" in {
      val date = LocalDateTime.now
      val range = Period(date.minusMinutes(10), date.plusMinutes(10))

      range.intersectsWith(range) must be (true)
      Period(date.minusMinutes(5), date.plusMinutes(5)).intersectsWith(range) must be (true)
      Period(date.minusMinutes(20), date.plusMinutes(10)).intersectsWith(range) must be (true)
      Period(date.minusMinutes(10), date.plusMinutes(20)).intersectsWith(range) must be (true)
      Period(date.minusMinutes(20), date.plusMinutes(20)).intersectsWith(range) must be (true)
      Period(date.minusMinutes(15), date.plusMinutes(5)).intersectsWith(range) must be (true)
      Period(date.minusMinutes(5), date.plusMinutes(15)).intersectsWith(range) must be (true)

      Period(date.minusMinutes(25), date.minusMinutes(15)).intersectsWith(range) must be (false)
      Period(date.plusMinutes(15), date.plusMinutes(25)).intersectsWith(range) must be (false)
      Period(date.minusMinutes(25), date.minusMinutes(10)).intersectsWith(range) must be (false)
      Period(date.plusMinutes(10), date.plusMinutes(25)).intersectsWith(range) must be (false)
    }
  }

  "Calendar" must {
    def createMeeting(period: String) =
      RequestsProcessor.parseRequest("2015-08-17 10:17:06 EMP001", period).right.get

    "add valid period" in {
      val calendar = Calendar(
        workingHours = TimeRange(LocalTime.of(9, 0), LocalTime.of(18, 30)),
        meetings = List(createMeeting("2015-08-21 10:00 1.5"))
      )
      calendar.meetings must have size 1

      val result1 = calendar + createMeeting("2015-08-21 09:00 1")
      result1.meetings must have size 2
      val result2 = result1 + createMeeting("2015-08-21 11:30 1")
      result2.meetings must have size 3
      val result3 = result2 + createMeeting("2015-08-21 14:30 2.5")
      result3.meetings must have size 4
      val result4 = result3 + createMeeting("2015-08-21 17:30 1")
      result4.meetings must have size 5
    }

    "reject invalid period" in {
      val calendar = Calendar(
        workingHours = TimeRange(LocalTime.of(9, 0), LocalTime.of(18, 30)),
        meetings = List(createMeeting("2015-08-21 10:00 1.5"))
      )
      calendar.meetings must have size 1

      (calendar + createMeeting("2015-08-21 08:59 1")).meetings must have size 1
      (calendar + createMeeting("2015-08-21 18:00 1")).meetings must have size 1
      (calendar + createMeeting("2015-08-21 09:00 2")).meetings must have size 1
      (calendar + createMeeting("2015-08-21 11:00 1")).meetings must have size 1
      (calendar + createMeeting("2015-08-21 09:30 3")).meetings must have size 1
      (calendar + createMeeting("2015-08-21 10:10 1")).meetings must have size 1
    }
  }
}
