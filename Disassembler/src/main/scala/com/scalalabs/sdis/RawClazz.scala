package com.scalalabs.sdis

import java.io.DataInputStream
import java.io.ByteArrayInputStream

class RawClazz(byt:Array[Byte]) {

  val stream = new DataInputStream( new ByteArrayInputStream(byt))
  
  def u1 = stream.readUnsignedByte
  def u2:Short = stream.readUnsignedShort.asInstanceOf[Short]
  def u4 = stream.readInt
  
  def u4Float = stream.readFloat
  def u4Double =stream.readDouble
  def bytes(n:Int) = {
    val buf = new Array[Byte](n)
    stream.readFully(buf)
    buf
  }

}