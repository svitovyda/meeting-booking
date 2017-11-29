package com.svitovyda.booking

object AppRunner {
  def main(args: Array[String]): Unit = {
    if(args.nonEmpty) // TODO: add validation of filename
      FileReaderService.readFile(args.head) match {
        case Right(header :: lines) if lines.size > 1  =>
          RequestsProcessor.createCalendar(header, lines) match {
            case Right(calendar) => println("success!")
            case Left(error) => println(error)
          }

        case Left(error) => println(error)
        case _ => println("File doesn't contain enough lines")
      }
    else println("Could not parse the file name, try again!")
  }
}
