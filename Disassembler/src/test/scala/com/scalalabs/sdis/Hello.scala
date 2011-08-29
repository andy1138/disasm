package com.scalalabs.sdis

object Hello  {
  def main(args:Array[String]) {
    println("Hello, World")
  }
  
  @deprecated
  def depTest() {
    println("Hello, World")
  }
  
  
  def branchTest() {
    if( System.currentTimeMillis() % 2  > 0 ) println("True") else println("false")
  }
  
  def inner() {
    (1 to 10).foreach { x =>
      x match {
        case 1 => println("One")
        case _ => println("other")
      }
    }
  }
  
  
  def switchTest() {
    1 match {
      case 1 => println("One")
      case 2 => println("two")
      case 3 => println("three")
      case _ => println("Other")
    }
  }
}