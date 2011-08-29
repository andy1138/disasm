package com.scalalabs.sdis


object DisasmCodeBlock {
  
  def unsign(b:Byte): Int = (b & 0xff)
  
  def readOpcode(code: Array[Byte], pc: Int): Int = unsign(code(pc))
  def readByte(code: Array[Byte], pc: Int): Int = unsign(code(pc + 1))
  def readShort(code: Array[Byte], pc: Int): Int = (unsign(code(pc + 1)) << 8) + unsign(code(pc + 2))
  def readInt(code: Array[Byte], pc: Int): Int = (unsign(code(pc + 1)) << 24) + (unsign(code(pc + 2)) << 16) + (unsign(code(pc + 3)) << 8) + unsign(code(pc + 4))  
}

class DisasmCodeBlock(code:Array[Byte], implicit val cpool: List[ConstantType]) {

  class HexString(n: Int) {
    //    def toHextStringX:String = n.toHexString
    def toHexString(len: Int): String = {
      val s = n.toHexString
      "0x" + ("0" * (len - s.length)) + s
    }
  }

  implicit def intToHexString(n: Int) = new HexString(n) 

  abstract class DynamicProc {
    def addLowHigh(low:Int, high:Int)
    def addJump(idx:Int, jump:Int)
    def addDefault(default:Int)
  }
  
  def optDynamic(code: Array[Byte], pc1: Int, handle:DynamicProc): (String, Int) = {
    var pc = pc1
    val offset = 0 // pc % 4
    pc += offset
    val defaultByte = DisasmCodeBlock.readInt(code, pc)  
    val lowByte = DisasmCodeBlock.readInt(code, pc + 4)  
    val highByte = DisasmCodeBlock.readInt(code, pc + 8)  

    pc += 13

//    var buf = " " + lowByte + " " + highByte
    handle.addLowHigh(lowByte, highByte)
    for (i <- lowByte to highByte) {
      val jump = 3 + DisasmCodeBlock.readInt(code, pc - 1) //((code(pc) & 0xff) << 24) + ((code(pc+1) & 0xff) << 16) + ((code(pc+2) & 0xff) << 8) + (code(pc+3) & 0xff) 
//      buf = buf + "\n    " + i + " " + jump.toHexString(4) 
      handle.addJump(i, jump)
      pc += 4
    }
//    buf = buf + "\n    default: " + defaultByte.toHexString(4)
    handle.addDefault(defaultByte)
    
    //          pc += (highByte - lowByte)
    ("", pc ) // -1,
  }

  def optCPRefW(code: Array[Byte], pc: Int): String = {
//    println("optCPRefW pc: " + pc)
//    val opVal2 = DisasmCodeBlock.readShort(code, pc)
    cpool(DisasmCodeBlock.readShort(code, pc) - 1).mkString
  }

  class AsmLine(val pc: Int, val b: Array[Byte], val txt: String)


  def mkCode(code: Array[Byte], attr:List[AttributesInfo]): String = {
    import com.scalalabs.sdis.opcode.Opcode._

//    println("mkCode called")
    var buff = ""
    var pc = 0;

    val lines = createLines(attr)
    val branches = createBranches(code)

//    try {
    while (pc < code.length) {
      if( Preference.displayLineNosInline ) {
        lines.get(pc) match {
          case Some(label) => buff = buff + "\n" + label.toString()
          case _ =>
        }
      }
      branches.get(pc) match {
        case Some(label) => buff = buff + "\n" + label.toString() + ":"
        case _ =>
      }
      val opVal: Int = DisasmCodeBlock.readOpcode(code, pc)

      buff = buff + "\n" + (if (Preference.displayCodePoint) {
        pc.toHexString(4) + " " + opcode(opVal)._1
      } else {
        "     " + opcode(opVal)._1
      })

      val handle = new DynamicProc {
        def addLowHigh(low:Int, high:Int) { buff = buff + " " + low + " " + high }
        def addJump(idx:Int, jump:Int) { buff = buff + "\n    " + idx + " " +   branches.getOrElse(jump, jump.toHexString(4)) }
        def addDefault(default:Int) {buff = buff + "\n    default: " + branches.getOrElse(default, default.toHexString(4))}
      }
      
      opcode(opVal)._3 match {
        case Kind.Dynamic =>
          val (s, pc1) = optDynamic(code, pc, handle)
//          buff = buff + " " + s
          pc = pc1
        case _ => 
          buff = buff + " " + addKindInfo(opcode(opVal)._3, code, pc, branches)
//          val siz = opcode(opVal)._2
//          buff = buff + " // size: " + ( opsize(opcode(opVal)._3)  +" " +  siz.toHexString(2) ) 
          pc += 1 + opsize(opcode(opVal)._3)

      }

    }
    
//    } catch {
//      case e:java.lang.IndexOutOfBoundsException => println(">>ERROR\n" + buff + "\n>>ERROR END")
//    }
//    println("mkCode end")
    buff
  }

  import com.scalalabs.sdis.opcode.Opcode._
  def addKindInfo(t: Kind.Value, code: Array[Byte], pc: Int, branches: Map[Int, String]): String = {

    t match {

      case Kind.AType => arrayTypeStr(code(pc + 1))  // 1,
      case Kind.Branch =>
        val p = ((pc) + DisasmCodeBlock.readShort(code, pc))  // 2,
        branchStr(p, branches)

      case Kind.BranchW =>
        val p = ((pc) + DisasmCodeBlock.readInt(code, pc))  // 4,
        branchStr(p, branches)
      case Kind.Byte => code(pc + 1).toHexString(2) // 1,
      case Kind.CPRef =>
        val idx = DisasmCodeBlock.readByte(code, pc)
        //          println(">>CPRef " + idx )
        cpool(idx - 1).mkString // 1,
      case Kind.CPRefWUByte => {
        val opVal2 = DisasmCodeBlock.readByte(code, pc)
        val opVal3 = DisasmCodeBlock.readByte(code, pc + 1)
        cpool(opVal2 - 1).mkString + " " + opVal3.toHexString(2)
      }
      case Kind.CPRefWUByteZero => { // 4th byte is always 0
        val opVal2 = DisasmCodeBlock.readShort(code, pc)
        val opVal3 = DisasmCodeBlock.readByte(code, pc + 2)
        cpool(opVal2 - 1).mkString + " " + opVal2 //opVal3.toHexString(2)  
      }
      //        case Kind.Dynamic => optDynamic(code, pc) 
      case Kind.Local =>  DisasmCodeBlock.readByte(code, pc).toHexString(2) //code(pc + 1).toHexString(2) // 1,
      case Kind.LocalByte => DisasmCodeBlock.readByte(code, pc).toHexString(2) //code(pc + 1).toHexString(2) // 1,
      case Kind.Short => DisasmCodeBlock.readShort(code, pc).toHexString(4) // ((code(pc + 1) << 8) + code(pc + 2)).toHexString(4) // 2,
      case Kind.WideCPRefW => DisasmCodeBlock.readByte(code, pc).toHexString(2) + " " + DisasmCodeBlock.readByte(code, pc).toHexString(2) //  code(pc + 1).toHexString(2) + " " + code(pc + 2).toHexString(2) // 2,
      case Kind.WideCPRefWShort => DisasmCodeBlock.readByte(code, pc).toHexString(2) + " " + DisasmCodeBlock.readByte(code, pc).toHexString(2) // code(pc + 1).toHexString(2) + " " + code(pc + 2).toHexString(2) // 2,
      case Kind.CPRefW => optCPRefW(code, pc)

      case _ => ""
    }
  }

  def arrayTypeStr(n: Int): String = {
    n match {
      case 4 => "T_BOOLEAN"
      case 5 => "T_CHAR"
      case 6 => "T_FLOAT"
      case 7 => "T_DOUBLE"
      case 8 => "T_BYTE"
      case 9 => "T_SHORT"
      case 10 => "T_INT"
      case 11 => "T_LONG"
      case x => "UNKNOWN[" + x + "]"
    }
  }

  def branchStr(pc: Int, branches: Map[Int, String]): String =
    branches.get(pc) match {
      case Some(label) => label.toString()
      case _ => "0x" + pc.toHexString(4)
    }

  def createBranches(code: Array[Byte]): Map[Int, String] = {
    import com.scalalabs.sdis.opcode.Opcode._

    var loops = Map[Int, String]()
    var loopidx = 0
    var pc = 0;

    def createLabel(idx: Int): String = {
      val label = "Loop" + loopidx
      loopidx += 1
      loops += (idx -> label)
      label;
    }

    while (pc < code.length) {
      val opVal: Int = DisasmCodeBlock.readOpcode(code,pc) //code(pc) & 0xff
      opcode(opVal)._3 match {
        case Kind.Branch => {
          val i: Int =  pc + DisasmCodeBlock.readShort(code, pc) // ((pc) + ((code(pc + 1) & 0xff) << 8) + code(pc + 2) & 0xff)
          createLabel(i)
        }
        case Kind.BranchW => {
          val i: Int = pc + DisasmCodeBlock.readInt(code, pc) // ((pc) + ((code(pc + 1) & 0xff) << 24) + ((code(pc + 2) & 0xff) << 16) + ((code(pc + 3) & 0xff) << 8) + code(pc + 4) & 0xff)
          createLabel(i)
        }
        case Kind.Dynamic => {
          val handle = new DynamicProc {
            def addLowHigh(low:Int, high:Int) { }
            def addJump(idx:Int, jump:Int) {  createLabel(jump) }
            def addDefault(default:Int) { createLabel(default) }
          }

//          println(">>createBranches - Kind.Dynamic ")
          val (x,y) = optDynamic(code, pc, handle)
//          println(">>createBranches - Kind.Dynamic pc: " + pc + ", " + y)
          pc = y
        }
        case _ =>
      }
      // todo: Kind.Dynamic size
      pc += 1 + opsize(opcode(opVal)._3)

    }

    loops
  }
  
  
  def createLines(attr:List[AttributesInfo]): Map[Int, String] = {
//      def lineNumberList(m: MethodInfo, attr:List[AttributesInfo]):String =  {
    var lines = scala.collection.mutable.Map[Int,String]()
    val ret = attr.map(x => x match {
        case LineNumberTableAttribute(line) => 
          line.flatMap(l =>  lines += (l.startPC ->  ("\n.line " + l.lineNo)   ))
        case _ =>
    })
    lines.toMap
  }
  
}