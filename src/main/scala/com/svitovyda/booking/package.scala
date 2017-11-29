package com.svitovyda

package object booking {
  type Error = String
  type Validation[T] = Either[Error, T]
}
