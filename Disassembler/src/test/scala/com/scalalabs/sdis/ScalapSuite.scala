package com.scalalabs.sdis

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.io.{ Source, Codec }
import com.scalalabs.sdis._
import java.io.{ByteArrayOutputStream, PrintStream}
import java.net.URL
import java.net.URI


class ScalapSuite extends FunSuite with ShouldMatchers {


  test("Full test - Hello.class") {
    val in = getClass.getResource("""Hello$.class""")
//    val in = getClass.getResource("""ScalapSuite.class""")
//    val in = getClass.getResource("""JasminOut.class""")
    val f = new java.io.File(in.toURI)
    println("File: " + f)

    val x = Scalap.fileToBytes(f)
  
    val raw = new RawClazz(x)
    val clazz = ClazzReader.readClazz(raw)

    Preference.displayCodeBytes = true
    Preference.displayConstPool = true
    Preference.displayCodePoint = false 
   
    new JasminOut(clazz, Console.out).disassemble()
  }

  test("can find hello") {
    assert("Hello" == Scalap.hello)
  }
  
  test("readShort") {
    assert(  1 === DisasmCodeBlock.readShort( Array[Byte](00, 00, 0x01), 0), " Test for 1")
    assert(  255 === DisasmCodeBlock.readShort( Array[Byte](00, 00, 0xff.toByte), 0), " Test for 255")
    assert(  256 === DisasmCodeBlock.readShort( Array[Byte](00, 0x01.toByte, 0x00), 0), " Test for 256")
  }

  test("CONSTANT_Utf8s") {
    val cpool = List[ConstantType]()
    val c = ConstantUtf8(2,"abc")
    val result = c.mkString(cpool) 
    assert("abc" === result)
  }

  test("CONSTANT_Class") {
    val cpool = List[ConstantType](ConstantUtf8(5, "Class"))
    val c = ConstantClazz(1)
    val result = c.mkString(cpool)
    assert("Class" === result)
  }
  
  
  test("ConstantFieldref") {
    val cpool = List[ConstantType](ConstantUtf8(5, "Class"), ConstantNameAndType(3,4), ConstantUtf8(6,"Method"), ConstantUtf8(2,"()"))
    val c = ConstantFieldref(1,2)
    val result = c.mkString(cpool)
    assert("Class.Method()" === result)
  }

  test("CONSTANT_Methodref") {
    val cpool = List[ConstantType](ConstantUtf8(5, "Class"), ConstantNameAndType(3,4), ConstantUtf8(6,"Method"), ConstantUtf8(2,"()"))
    val c = ConstantMethodref(1,2)
    val result = c.mkString(cpool)
    assert("Class.Method()" === result)
  }
  
  
  test("CONSTANT_InterfaceMethodref") {
    val cpool = List[ConstantType](ConstantUtf8(5, "Class"), ConstantNameAndType(3,4), ConstantUtf8(6,"Method"), ConstantUtf8(2,"()"))
    val c = ConstantInterfaceMethodref(1,2)
    val result = c.mkString(cpool)
    assert("Class.Method()" === result)
  }
  
  
  test("CONSTANT_String") {
    val cpool = List[ConstantType](ConstantUtf8(7, "AString"))
    val c = ConstantString(1)
    val result = c.mkString(cpool)
    assert("'AString'" === result)
  }
  
  test("CONSTANT_Integer") {
    val cpool = List[ConstantType]()
    val c = ConstantInteger(1)
    val result = c.mkString(cpool)
    assert("1" === result)
  }
  
  
  test("CONSTANT_Float") {
    val cpool = List[ConstantType]()
    val c = ConstantFloat(1)
    val result = c.mkString(cpool)
    assert("1.0" === result)
  }
  
  test("CONSTANT_Long") {
    val cpool = List[ConstantType]()
    val c = ConstantLong(1L )
    val result = c.mkString(cpool)
    assert("1" === result)
  }
  
  test("CONSTANT_Double") {    
    val cpool = List[ConstantType]()
    val c = ConstantDouble(1D)
    val result = c.mkString(cpool)
    assert("1.0" === result)
  }
  
  test("CONSTANT_NameAndType") {
    val cpool = List[ConstantType](ConstantUtf8(5, "Class"), ConstantUtf8(2,"()"))
    val c = ConstantNameAndType(1,2)
    val result = c.mkString(cpool)
    assert("Class()" === result)

  }
  
  test("opcode: Kind.AType") {
      val code = Array[Byte](0xbc.toByte,0x04)
      val asm = """
     newarray T_BOOLEAN"""

    val buff = new ByteArrayOutputStream(1024)
    val out = new PrintStream( buff )

//      val jasmin = new JasminOut( new Clazz(), out)
      val codeBlock = new DisasmCodeBlock(code, List[ConstantType]())
    Preference.displayCodePoint = false 
    val res = codeBlock.mkCode(code, Nil)
    assert( asm === res)
        
    }
  
  test("opcode: Kind.Branch") {
    // a7,0,5,12,6c,b6,
    val code = Array[Byte](0xa7.toByte,0x00,0x5,0x00,0x00,0x00)
    val asm = """
     goto Loop0
     nop 
     nop 
Loop0:
     nop """
    
    val buff = new ByteArrayOutputStream(1024)
    val out = new PrintStream( buff )
    
    
      val codeBlock = new DisasmCodeBlock(code, List[ConstantType]())
//    val jasmin = new JasminOut( new Clazz(), out)
    Preference.displayCodePoint = false 
    val res = codeBlock.mkCode(code, Nil)
    assert( asm === res)
  }

    
  test("opcode: Kind.BranchW") {
    val code = Array[Byte](0xc8.toByte,0x00,0x00,0x00,0x05,0x00)
    val asm = """
     goto_w Loop0
Loop0:
     nop """
    
    val buff = new ByteArrayOutputStream(1024)
    val out = new PrintStream( buff )
    
      val codeBlock = new DisasmCodeBlock(code, List[ConstantType]())
//    val jasmin = new JasminOut( new Clazz(), out)
    Preference.displayCodePoint = false 
    val res = codeBlock.mkCode(code, Nil)
    assert( asm === res)
    }
  
    test("opcode: Kind.Byte") {
      val code = Array[Byte](0x10.toByte,0x05)
      val asm = """
     bipush 0x05"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, List[ConstantType]())
      val jasmin = new JasminOut( new Clazz(), out)
//      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
      assert( asm === res)
    }
  
    test("opcode: Kind.CPRef int") {
      val clazz = new Clazz()
//      val i = ConstantInteger(0x05)
      val cpool = ConstantInteger(0x05) ::  Nil
      
      val code = Array[Byte](0x12.toByte,0x01)
      val asm = """
     ldc 5"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, cpool)
//      val jasmin = new JasminOut( clazz, out)
      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
      assert( asm === res)
    }
 
    test("opcode: Kind.CPRef string") {
//      val clazz = new Clazz()
      val cpool = ConstantString(0x02) :: ConstantUtf8(0x0b, "Hello World") :: Nil
      
      val code = Array[Byte](0x12.toByte,0x01)
      val asm = """
     ldc 'Hello World'"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, cpool)
//      val jasmin = new JasminOut( clazz, out)
      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
      assert( asm === res)
    }
    
    
    test("opcode: Kind.CPRefW") {
      val clazz = new Clazz()
      val cpool = ConstantString(0x02) :: ConstantUtf8(0x0b, "Hello World") :: Nil
      
      val code = Array[Byte](0x13.toByte, 0x00, 0x01)
      val asm = """
     ldc_w 'Hello World'"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, cpool)
//      val jasmin = new JasminOut( clazz, out)
      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
      assert( asm === res)
    }
  
    test("opcode: Kind.CPRefWUByte") {
      val clazz = new Clazz()
      //ConstantInterfaceMethodref
      val cpool = ConstantClazz(0x02) :: ConstantUtf8(0x0b, "[[I") :: Nil
      
      val code = Array[Byte](0xc5.toByte, 0x01, 0x02)
      val asm = """
     multianewarray [[I 0x02"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, cpool)
//      val jasmin = new JasminOut( clazz, out)
      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
      assert( asm === res)
    }
  
    test("opcode: Kind.CPRefWUByteZero") {
      val clazz = new Clazz()
      //ConstantInterfaceMethodref
      val cpool = ConstantInterfaceMethodref(2, 4) :: ConstantClazz(3) :: ConstantUtf8(0x0b, "Class") :: ConstantUtf8(0x0b, "Object") :: Nil
      
      val code = Array[Byte](0xb9.toByte, 0x00, 0x01, 0x03, 00)
      val asm = """
     invokeinterface Class.Object 1"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, cpool)
//      val jasmin = new JasminOut( clazz, out)
      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
    println("\n" + res )
      assert( asm === res)
    }
  
//    def main(args: Array[String]) {
    test("opcode: Kind.Dynamic") {
      //aa,0,0,0,2d,0,0,0,4,0,0,0,b,0,0,0,4d,0,0,0,77,0,0,0,6b,0,0,0,5f,0,0,0,53,0,0,0,65,0,0,0,71,0,0,0,59,bb,0,18,59,b7,0,1c,13,1,a0,b6,0,22,1c,b8,1,2,b6,0
//      val code = Array[Byte](0x15.toByte, 0x05)

      val code = Array[Byte](0xaa.toByte,0,0,0,0x2d.toByte,0,0,0,4,0, 0,0,0x0b.toByte,0,0,0,0x4d.toByte,0,0,0, 0x77.toByte,0,0,0,0x6b.toByte,0,0,0,0x5f.toByte, 0,0,0,0x53.toByte,0,0,0,0x65.toByte,0,0, 0,0x71.toByte,0,0,0,0x59.toByte)//,0xbb.toByte)//,0,18,59)  //,0xb7.toByte,0,0x1c.toByte,13,1,0xa0.toByte,0xb6.toByte,0,0x22.toByte,0x1c.toByte,0xb8.toByte,1,2,0xb6.toByte,0)
//    val asm = """
//     tableswitch 4 11
//    4 0x0050
//    5 0x007a
//    6 0x006e
//    7 0x0062
//    8 0x0056
//    9 0x0068
//    10 0x0074
//    11 0x005c
//    default: 0x002d"""

    val asm = """
     tableswitch 4 11
    4 Loop0
    5 Loop1
    6 Loop2
    7 Loop3
    8 Loop4
    9 Loop5
    10 Loop6
    11 Loop7
    default: Loop8"""

    
    val buff = new ByteArrayOutputStream(1024)
    val out = new PrintStream( buff )
    
      val codeBlock = new DisasmCodeBlock(code, Nil)
//    val jasmin = new JasminOut( new Clazz(), out)
    Preference.displayCodePoint = false 
    val res = codeBlock.mkCode(code, Nil)
    assert( asm === res)
      
    }
  
    test("opcode: Kind.Local") {
      val code = Array[Byte](0x15.toByte, 0x05)
      val asm = """
     iload 0x05"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, Nil)
//      val jasmin = new JasminOut( new Clazz(), out)
      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
      assert( asm === res)
    }
  
    test("opcode: Kind.LocalByte") {
      val code = Array[Byte](0x84.toByte, 0x05)
      val asm = """
     iinc 0x05"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, Nil)
//      val jasmin = new JasminOut( new Clazz(), out)
      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
      assert( asm === res)
    }
  
    test("opcode: Kind.Short") {
      val code = Array[Byte](0x11.toByte, 0x00, 0x05)
      val asm = """
     sipush 0x0005"""
      
      val buff = new ByteArrayOutputStream(1024)
      val out = new PrintStream( buff )
      
      val codeBlock = new DisasmCodeBlock(code, Nil)
//      val jasmin = new JasminOut( new Clazz(), out)
      Preference.displayCodePoint = false 
      val res = codeBlock.mkCode(code, Nil)
      assert( asm === res)
    }
  
    ignore("opcode: Kind.WideNoOperands") {
      pending
    }
  
    ignore("opcode: Kind.WideCPRefW") {
      pending
    }
  
    ignore("opcode: Kind.WideCPRefWShort") {
      pending
    }
  
    ignore("opcode: Kind.Unknown") {
      pending
    }
  
    ignore("RuntimeVisibleAnnotations encode") {
      val txt = "Hello World"
      val encoded =  scala.reflect.generic.ByteCodecs.encode( txt.getBytes() )
      val decode =  scala.reflect.generic.ByteCodecs.decode( encoded )
      assert( new String(encoded) === txt)
    }
    
    test("RuntimeVisibleAnnotations (4.7.16)") {
      val bytes =  Array[Byte](
          0,1,   // num_annotations;
          0,48,  // type_index -- 'Lscala/reflect/ScalaSignature'
          0,1,   // num_element_value_pairs;
          0,49,  // element_name_index -- 'bytes'
          115.toByte,  // tag  -- 's'
          0,50  // const_value_index
          )
      val asm = ""
      
        
        val result = ClazzReader.loadRuntimeVisibleAnnotationsAttribute(new RawClazz(bytes))
        println("RuntimeVisibleAnnotations: " + result)
//      val buff = new ByteArrayOutputStream(1024)
//      val out = new PrintStream( buff )
//    
//      val codeBlock = new DisasmCodeBlock(code, Nil)
////    val jasmin = new JasminOut( new Clazz(), out)
//      Preference.displayCodePoint = false 
//      val res = codeBlock.mkCode(code, Nil)
//      assert( asm === res)

    }
    
    
    
    test("repl") {
      val bytes = Array[Byte](
          0xca.toByte,0xfe.toByte,0xba.toByte,0xbe.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x31.toByte,0x0.toByte,0x18.toByte,0x1.toByte,0x0.toByte,0xa.toByte,0x53.toByte,0x6f.toByte,0x75.toByte,0x72.toByte,0x63.toByte,0x65.toByte,0x46.toByte,0x69.toByte,0x6c.toByte,0x65.toByte,0x1.toByte,0x0.toByte,0x9.toByte,0x3c.toByte,0x63.toByte,0x6f.toByte,0x6e.toByte,0x73.toByte,0x6f.toByte,0x6c.toByte,0x65.toByte,0x3e.toByte,0x1.toByte,0x0.toByte,0x2.toByte,0x68.toByte,0x69.toByte,0x1.toByte,0x0.toByte,0x14.toByte,0x28.toByte,0x29.toByte,0x4c.toByte,0x6a.toByte,0x61.toByte,0x76.toByte,0x61.toByte,0x2f.toByte,0x6c.toByte,0x61.toByte,0x6e.toByte,0x67.toByte,0x2f.toByte,0x53.toByte,0x74.toByte,0x72.toByte,0x69.toByte,0x6e.toByte,0x67.toByte,0x3b.toByte,0x1.toByte,0x0.toByte,0x4.toByte,0x43.toByte,0x6f.toByte,0x64.toByte,0x65.toByte,0x1.toByte,0x0.toByte,0x5.toByte,0x68.toByte,0x65.toByte,0x6c.toByte,0x6c.toByte,0x6f.toByte,0x8.toByte,0x0.toByte,0x6.toByte,0x1.toByte,0x0.toByte,0xf.toByte,0x4c.toByte,0x69.toByte,0x6e.toByte,0x65.toByte,0x4e.toByte,0x75.toByte,0x6d.toByte,0x62.toByte,0x65.toByte,0x72.toByte,0x54.toByte,0x61.toByte,0x62.toByte,0x6c.toByte,0x65.toByte,0x1.toByte,0x0.toByte,0x6.toByte,0x3c.toByte,0x69.toByte,0x6e.toByte,0x69.toByte,0x74.toByte,0x3e.toByte,0x1.toByte,0x0.toByte,0x3.toByte,0x28.toByte,0x29.toByte,0x56.toByte,0x1.toByte,0x0.toByte,0x10.toByte,0x6a.toByte,0x61.toByte,0x76.toByte,0x61.toByte,0x2f.toByte,0x6c.toByte,0x61.toByte,0x6e.toByte,0x67.toByte,0x2f.toByte,0x4f.toByte,0x62.toByte,0x6a.toByte,0x65.toByte,0x63.toByte,0x74.toByte,0x7.toByte,0x0.toByte,0xb.toByte,0xc.toByte,0x0.toByte,0x9.toByte,0x0.toByte,0xa.toByte,0xa.toByte,0x0.toByte,0xc.toByte,0x0.toByte,0xd.toByte,0x1.toByte,0x0.toByte,0x5.toByte,0x53.toByte,0x63.toByte,0x61.toByte,0x6c.toByte,0x61.toByte,0x1.toByte,0x0.toByte,0xc.toByte,0x49.toByte,0x6e.toByte,0x6e.toByte,0x65.toByte,0x72.toByte,0x43.toByte,0x6c.toByte,0x61.toByte,0x73.toByte,0x73.toByte,0x65.toByte,0x73.toByte,0x1.toByte,0x0.toByte,0x1a.toByte,0x24.toByte,0x6c.toByte,0x69.toByte,0x6e.toByte,0x65.toByte,0x31.toByte,0x2f.toByte,0x24.toByte,0x72.toByte,0x65.toByte,0x61.toByte,0x64.toByte,0x24.toByte,0x24.toByte,0x69.toByte,0x77.toByte,0x24.toByte,0x24.toByte,0x69.toByte,0x77.toByte,0x24.toByte,0x46.toByte,0x75.toByte,0x62.toByte,0x61.toByte,0x72.toByte,0x7.toByte,0x0.toByte,0x11.toByte,0x1.toByte,0x0.toByte,0x15.toByte,0x24.toByte,0x6c.toByte,0x69.toByte,0x6e.toByte,0x65.toByte,0x31.toByte,0x2f.toByte,0x24.toByte,0x72.toByte,0x65.toByte,0x61.toByte,0x64.toByte,0x24.toByte,0x24.toByte,0x69.toByte,0x77.toByte,0x24.toByte,0x24.toByte,0x69.toByte,0x77.toByte,0x24.toByte,0x7.toByte,0x0.toByte,0x13.toByte,0x1.toByte,0x0.toByte,0x5.toByte,0x46.toByte,0x75.toByte,0x62.toByte,0x61.toByte,0x72.toByte,0x1.toByte,0x0.toByte,0x11.toByte,0x73.toByte,0x63.toByte,0x61.toByte,0x6c.toByte,0x61.toByte,0x2f.toByte,0x53.toByte,0x63.toByte,0x61.toByte,0x6c.toByte,0x61.toByte,0x4f.toByte,0x62.toByte,0x6a.toByte,0x65.toByte,0x63.toByte,0x74.toByte,0x7.toByte,0x0.toByte,0x16.toByte,0x0.toByte,0x21.toByte,0x0.toByte,0x12.toByte,0x0.toByte,0xc.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x17.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x2.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x3.toByte,0x0.toByte,0x4.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x5.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x1b.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x3.toByte,0x12.toByte,0x7.toByte,0xb0.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x8.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x6.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x7.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x9.toByte,0x0.toByte,0xa.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x5.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x1d.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x5.toByte,0x2a.toByte,0xb7.toByte,0x0.toByte,0xe.toByte,0xb1.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x8.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x6.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x7.toByte,0x0.toByte,0x3.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x2.toByte,0x0.toByte,0x2.toByte,0x0.toByte,0xf.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0x10.toByte,0x0.toByte,0x0.toByte,0x0.toByte,0xa.toByte,0x0.toByte,0x1.toByte,0x0.toByte,0x12.toByte,0x0.toByte,0x14.toByte,0x0.toByte,0x15.toByte,0x0.toByte,0x9.toByte
          )
    val raw = new RawClazz(bytes)
    val clazz = ClazzReader.readClazz(raw)

    Preference.displayCodeBytes = true
    Preference.displayConstPool = true
    Preference.displayCodePoint = false 
   
    new JasminOut(clazz, Console.out).disassemble()
          
          
    }
    
}
