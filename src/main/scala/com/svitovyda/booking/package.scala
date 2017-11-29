package com.svitovyda

import java.time.LocalDateTime

package object booking {
  type Error = String
  type Validation[T] = Either[Error, T]

  case class EmployeeId(value: String) extends AnyVal
}
