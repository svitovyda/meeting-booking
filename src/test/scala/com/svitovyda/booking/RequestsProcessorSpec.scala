package com.svitovyda.booking

import java.time.LocalTime

import com.svitovyda.booking.Calendar.{Period, TimeRange}
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.util.{Failure, Success}


class RequestsProcessorSpec extends WordSpecLike with MustMatchers {
  "parseHeader" must {
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
}
