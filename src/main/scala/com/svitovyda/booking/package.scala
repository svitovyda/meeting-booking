package com.svitovyda

import java.time.{LocalDateTime, ZoneOffset}

package object booking {
  type Error = String
  type Validation[T] = Either[Error, T]

  case class EmployeeId(value: String) extends AnyVal

  implicit val localDateOrdering: Ordering[LocalDateTime] = Ordering.by(_.toEpochSecond(ZoneOffset.UTC))
}
