package com.svitovyda.booking

object AppRunner {
  def main(args: Array[String]): Unit = {
    if(args.nonEmpty) // TODO: add validation of filename
      FileReaderService.readFile(args.head) match {
        case Right(lines) =>
          //val calendar =
        case Left(error) => println(error)
      }
    else println("Could not parse the file name, try again!")
  }
}
