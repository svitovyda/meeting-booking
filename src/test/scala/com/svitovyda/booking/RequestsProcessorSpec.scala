package com.svitovyda.booking

import java.time.LocalTime

import com.svitovyda.booking.Calendar.{Meeting, Period, TimeRange}
import org.scalatest.{MustMatchers, WordSpecLike}


import scala.util.{Failure, Right, Success}


class RequestsProcessorSpec extends WordSpecLike with MustMatchers {
  "parseHeader()" must {
    "correctly process valid working hours" in {
      RequestsProcessor.parseHeader("1200 1315") must be(Success(TimeRange(
        LocalTime.of(12, 0), LocalTime.of(13, 15))))
      RequestsProcessor.parseHeader(" 0901 2356  ") must be(Success(TimeRange(
        LocalTime.of(9, 1), LocalTime.of(23, 56))))
    }

    "reject invalid working hours" in {
      RequestsProcessor.parseHeader("1600 1315") must be (a[Failure[TimeRange]])
      RequestsProcessor.parseHeader("0901 2456") must be (a[Failure[TimeRange]])
      RequestsProcessor.parseHeader("0966 2356") must be (a[Failure[TimeRange]])
      RequestsProcessor.parseHeader("ab45 2356") must be (a[Failure[TimeRange]])
      RequestsProcessor.parseHeader("09:45 2356") must be (a[Failure[TimeRange]])
    }
  }

  "parseRequest()" must {
    "parse valid lines" in {
      RequestsProcessor.parseRequest(
        "2015-08-17 10:17:06 EMP001", "2015-08-21 09:00 2") must be (a[Right[Error, Meeting]])
    }

    "reject invalid lines" in {
      RequestsProcessor.parseRequest("", "") must be (a[Left[Error, Meeting]])
    }
  }

  "parseRequests()" must {
    "parse valid lines" in {
      val meetings = RequestsProcessor.parseRequests(List(
        "2015-08-17 10:17:06 EMP001",
        "2015-08-21 09:00 2",
        "2015-08-16 12:34:56 EMP002",
        "2015-08-21 09:00 2",
        "2015-08-16 09:28:23 EMP003",
        "2015-08-22 14:00 2",
        "2015-08-17 11:23:45 EMP004",
        "2015-08-22 16:00 1",
        "2015-08-15 17:29:12 EMP005",
        "2015-08-21 16:00 3"
      ))
      meetings must have size 5
    }

    "reject invalid lines but return valid" in {
      val meetings = RequestsProcessor.parseRequests(List(
        "2015-08-17 10:17:06 EMP001",
        "2015-08-21 09:00 2",
        "------ 17:29:12 EMP005",
        "2015-08-21 16:00 3"
      ))
      meetings must have size 1
    }

    "reject non-paired lines" in {
      val meetings = RequestsProcessor.parseRequests(List(
        "2015-08-17 10:17:06 EMP001",
        "2015-08-21 09:00 2",
        "2015-08-16 12:34:56 EMP002",
        "2015-08-16 09:28:23 EMP003",
        "2015-08-22 14:00 2",
        "2015-08-15 17:29:12 EMP005",
        "2015-08-21 16:00 3"
      ))
      meetings must have size 1 // TODO: implement smart validation of pairs to not skip valid
    }

    "ignore empty lines" in {
      val meetings = RequestsProcessor.parseRequests(List(
        "2015-08-17 10:17:06 EMP001",
        "",
        "2015-08-21 09:00 2",
        " ",
        "2015-08-16 12:34:56 EMP002",
        "",
        "",
        "2015-08-21 09:00 2",
        "     ",
        "2015-08-15 17:29:12 EMP005",
        "2015-08-21 16:00 3"
      ))
      meetings must have size 3
    }
  }

  "createCalendar()" must {
    "correctly create a calendar" in {
      RequestsProcessor.createCalendar("0945 1830", List()) must be (Right[Error, Calendar](
        Calendar(TimeRange(LocalTime.of(9, 45), LocalTime.of(18, 30)))))

      RequestsProcessor.createCalendar(
        "0945 1830",
        List(
          "2015-08-17 10:17:06 EMP001",
          "2015-08-21 09:00 2",
          "2015-08-16 12:34:56 EMP002",
          "2015-08-21 09:00 2",
          "2015-08-16 09:28:23 EMP003",
          "2015-08-22 14:00 2",
          "2015-08-17 11:23:45 EMP004",
          "2015-08-22 16:00 1",
          "2015-08-15 17:29:12 EMP005",
          "2015-08-21 16:00 3"
        )
      ) must be (a[Right[Error, Calendar]])
    }

    "reject creating calendar" in {
      RequestsProcessor.createCalendar("2145 1830", List()) must be (a[Left[Error, Calendar]])
    }
  }
}
