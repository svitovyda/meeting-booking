package com.svitovyda.booking

object AppRunner {
  def main(args: Array[String]): Unit = {
    if(args.nonEmpty)
      println(args.head)
    else println("Could not parse the file name, try again!")
  }
}
