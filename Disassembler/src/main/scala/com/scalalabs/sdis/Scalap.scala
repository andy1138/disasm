package com.scalalabs.sdis

import scala.io.Source
import java.io.{File, FileInputStream}


object Scalap {

  val hello = "Hello"
    
    
  def fileToBytes(f:File):Array[Byte] = {
    val len:Int  = f.length.toInt 
    val buff = new Array[Byte](len)
    val in = new FileInputStream(f)
    in.read(buff)
    buff
  }
  
}