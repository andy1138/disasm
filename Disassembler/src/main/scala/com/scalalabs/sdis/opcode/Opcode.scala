/**
 * Disassembler
 * (c)2011 andy hicks 
 */

package com.scalalabs.sdis.opcode

object Opcode {

  object Kind extends Enumeration {
  val NoOperands = Value
  val AType = Value
  val Branch = Value
  val BranchW = Value
  val Byte = Value
  val CPRef = Value
  val CPRefW = Value
  val CPRefWUByte = Value
  val CPRefWUByteZero = Value
  val Dynamic = Value
  val Local = Value
  val LocalByte = Value
  val Short = Value
  val WideNoOperands = Value
  val WideCPRefW = Value
  val WideCPRefWShort = Value
  val Unknown = Value
  
  }
  
  val opcode = Array[(String, Int, Kind.Value)] (
  ("nop", 0x00, Kind.NoOperands),
    ("aconst_null", 0x1, Kind.NoOperands),
    ("iconst_m1", 0x2, Kind.NoOperands),
    ("iconst_0", 0x3, Kind.NoOperands),
    ("iconst_1", 0x4, Kind.NoOperands),
    ("iconst_2", 0x5, Kind.NoOperands),
    ("iconst_3", 0x6, Kind.NoOperands),
    ("iconst_4", 0x7, Kind.NoOperands),
    ("iconst_5", 0x8, Kind.NoOperands),
    ("lconst_0", 0x9, Kind.NoOperands),
    ("lconst_1", 0xa, Kind.NoOperands),
    ("fconst_0", 0xb, Kind.NoOperands),
    ("fconst_1", 0xc, Kind.NoOperands),
    ("fconst_2", 0xd, Kind.NoOperands),
    ("dconst_0", 0xe, Kind.NoOperands),
    ("dconst_1", 0xf, Kind.NoOperands),

    ("bipush", 0x10, Kind.Byte),
    ("sipush", 0x11, Kind.Short),
    ("ldc", 0x12, Kind.CPRef),
    ("ldc_w", 0x13, Kind.CPRefW),
    ("ldc2_w", 0x14, Kind.CPRefW),
    ("iload", 0x15, Kind.Local),
    ("lload", 0x16, Kind.Local),
    ("fload", 0x17, Kind.Local),
    ("dload", 0x18, Kind.Local),
    ("aload", 0x19, Kind.Local),
    ("iload_0", 0x1a, Kind.NoOperands),
    ("iload_1", 0x1b, Kind.NoOperands),
    ("iload_2", 0x1c, Kind.NoOperands),
    ("iload_3", 0x1d, Kind.NoOperands),
    ("lload_0", 0x1e, Kind.NoOperands),
    ("lload_1", 0x1f, Kind.NoOperands),

    ("lload_2", 0x20, Kind.NoOperands),
    ("lload_3", 0x21, Kind.NoOperands),
    ("fload_0", 0x22, Kind.NoOperands),
    ("fload_1", 0x23, Kind.NoOperands),
    ("fload_2", 0x24, Kind.NoOperands),
    ("fload_3", 0x25, Kind.NoOperands),
    ("dload_0", 0x26, Kind.NoOperands),
    ("dload_1", 0x27, Kind.NoOperands),
    ("dload_2", 0x28, Kind.NoOperands),
    ("dload_3", 0x29, Kind.NoOperands),
    ("aload_0", 0x2a, Kind.NoOperands),
    ("aload_1", 0x2b, Kind.NoOperands),
    ("aload_2", 0x2c, Kind.NoOperands),
    ("aload_3", 0x2d, Kind.NoOperands),
    ("iaload", 0x2e, Kind.NoOperands),
    ("laload", 0x2f, Kind.NoOperands),

    ("faload", 0x30, Kind.NoOperands),
    ("daload", 0x31, Kind.NoOperands),
    ("aaload", 0x32, Kind.NoOperands),
    ("baload", 0x33, Kind.NoOperands),
    ("caload", 0x34, Kind.NoOperands),
    ("saload", 0x35, Kind.NoOperands),
    ("istore", 0x36, Kind.Local),
    ("lstore", 0x37, Kind.Local),
    ("fstore", 0x38, Kind.Local),
    ("dstore", 0x39, Kind.Local),
    ("astore", 0x3a, Kind.Local),
    ("istore_0", 0x3b, Kind.NoOperands),
    ("istore_1", 0x3c, Kind.NoOperands),
    ("istore_2", 0x3d, Kind.NoOperands),
    ("istore_3", 0x3e, Kind.NoOperands),
    ("lstore_0", 0x3f, Kind.NoOperands),

    ("lstore_1", 0x40, Kind.NoOperands),
    ("lstore_2", 0x41, Kind.NoOperands),
    ("lstore_3", 0x42, Kind.NoOperands),
    ("fstore_0", 0x43, Kind.NoOperands),
    ("fstore_1", 0x44, Kind.NoOperands),
    ("fstore_2", 0x45, Kind.NoOperands),
    ("fstore_3", 0x46, Kind.NoOperands),
    ("dstore_0", 0x47, Kind.NoOperands),
    ("dstore_1", 0x48, Kind.NoOperands),
    ("dstore_2", 0x49, Kind.NoOperands),
    ("dstore_3", 0x4a, Kind.NoOperands),
    ("astore_0", 0x4b, Kind.NoOperands),
    ("astore_1", 0x4c, Kind.NoOperands),
    ("astore_2", 0x4d, Kind.NoOperands),
    ("astore_3", 0x4e, Kind.NoOperands),
    ("iastore", 0x4f, Kind.NoOperands),

    ("lastore", 0x50, Kind.NoOperands),
    ("fastore", 0x51, Kind.NoOperands),
    ("dastore", 0x52, Kind.NoOperands),
    ("aastore", 0x53, Kind.NoOperands),
    ("bastore", 0x54, Kind.NoOperands),
    ("castore", 0x55, Kind.NoOperands),
    ("sastore", 0x56, Kind.NoOperands),
    ("pop", 0x57, Kind.NoOperands),
    ("pop2", 0x58, Kind.NoOperands),
    ("dup", 0x59, Kind.NoOperands),
    ("dup_x1", 0x5a, Kind.NoOperands),
    ("dup_x2", 0x5b, Kind.NoOperands),
    ("dup2", 0x5c, Kind.NoOperands),
    ("dup2_x1", 0x5d, Kind.NoOperands),
    ("dup2_x2", 0x5e, Kind.NoOperands),
    ("swap", 0x5f, Kind.NoOperands),

    ("iadd", 0x60, Kind.NoOperands),
    ("ladd", 0x61, Kind.NoOperands),
    ("fadd", 0x62, Kind.NoOperands),
    ("dadd", 0x63, Kind.NoOperands),
    ("isub", 0x64, Kind.NoOperands),
    ("lsub", 0x65, Kind.NoOperands),
    ("fsub", 0x66, Kind.NoOperands),
    ("dsub", 0x67, Kind.NoOperands),
    ("imul", 0x68, Kind.NoOperands),
    ("lmul", 0x69, Kind.NoOperands),
    ("fmul", 0x6a, Kind.NoOperands),
    ("dmul", 0x6b, Kind.NoOperands),
    ("idiv", 0x6c, Kind.NoOperands),
    ("ldiv", 0x6d, Kind.NoOperands),
    ("fdiv", 0x6e, Kind.NoOperands),
    ("ddiv", 0x6f, Kind.NoOperands),

    ("irem", 0x70, Kind.NoOperands),
    ("lrem", 0x71, Kind.NoOperands),
    ("frem", 0x72, Kind.NoOperands),
    ("drem", 0x73, Kind.NoOperands),
    ("ineg", 0x74, Kind.NoOperands),
    ("lneg", 0x75, Kind.NoOperands),
    ("fneg", 0x76, Kind.NoOperands),
    ("dneg", 0x77, Kind.NoOperands),
    ("ishl", 0x78, Kind.NoOperands),
    ("lshl", 0x79, Kind.NoOperands),
    ("ishr", 0x7a, Kind.NoOperands),
    ("lshr", 0x7b, Kind.NoOperands),
    ("iushr", 0x7c, Kind.NoOperands),
    ("lushr", 0x7d, Kind.NoOperands),
    ("iand", 0x7e, Kind.NoOperands),
    ("land", 0x7f, Kind.NoOperands),

    ("ior", 0x80, Kind.NoOperands),
    ("lor", 0x81, Kind.NoOperands),
    ("ixor", 0x82, Kind.NoOperands),
    ("lxor", 0x83, Kind.NoOperands),
    ("iinc", 0x84, Kind.LocalByte),
    ("i2l", 0x85, Kind.NoOperands),
    ("i2f", 0x86, Kind.NoOperands),
    ("i2d", 0x87, Kind.NoOperands),
    ("l2i", 0x88, Kind.NoOperands),
    ("l2f", 0x89, Kind.NoOperands),
    ("l2d", 0x8a, Kind.NoOperands),
    ("f2i", 0x8b, Kind.NoOperands),
    ("f2l", 0x8c, Kind.NoOperands),
    ("f2d", 0x8d, Kind.NoOperands),
    ("d2i", 0x8e, Kind.NoOperands),
    ("d2l", 0x8f, Kind.NoOperands),

    ("d2f", 0x90, Kind.NoOperands),
    ("i2b", 0x91, Kind.NoOperands),
    ("i2c", 0x92, Kind.NoOperands),
    ("i2s", 0x93, Kind.NoOperands),
    ("lcmp", 0x94, Kind.NoOperands),
    ("fcmpl", 0x95, Kind.NoOperands),
    ("fcmpg", 0x96, Kind.NoOperands),
    ("dcmpl", 0x97, Kind.NoOperands),
    ("dcmpg", 0x98, Kind.NoOperands),
    ("ifeq", 0x99, Kind.Branch),
    ("ifne", 0x9a, Kind.Branch),
    ("iflt", 0x9b, Kind.Branch),
    ("ifge", 0x9c, Kind.Branch),
    ("ifgt", 0x9d, Kind.Branch),
    ("ifle", 0x9e, Kind.Branch),
    ("if_icmpeq", 0x9f, Kind.Branch),

    ("if_icmpne", 0xa0, Kind.Branch),
    ("if_icmplt", 0xa1, Kind.Branch),
    ("if_icmpge", 0xa2, Kind.Branch),
    ("if_icmpgt", 0xa3, Kind.Branch),
    ("if_icmple", 0xa4, Kind.Branch),
    ("if_acmpeq", 0xa5, Kind.Branch),
    ("if_acmpne", 0xa6, Kind.Branch),
    ("goto", 0xa7, Kind.Branch),
    ("jsr", 0xa8, Kind.Branch),
    ("ret", 0xa9, Kind.Local),
    ("tableswitch", 0xaa, Kind.Dynamic),
    ("lookupswitch", 0xab, Kind.Dynamic),
    ("ireturn", 0xac, Kind.NoOperands),
    ("lreturn", 0xad, Kind.NoOperands),
    ("freturn", 0xae, Kind.NoOperands),
    ("dreturn", 0xaf, Kind.NoOperands),
    
    ("areturn", 0xb0, Kind.NoOperands),
    ("return", 0xb1, Kind.NoOperands),
    ("getstatic", 0xb2, Kind.CPRefW),
    ("putstatic", 0xb3, Kind.CPRefW),
    ("getfield", 0xb4, Kind.CPRefW),
    ("putfield", 0xb5, Kind.CPRefW),
    ("invokevirtual", 0xb6, Kind.CPRefW),
    ("invokespecial", 0xb7, Kind.CPRefW),
    ("invokestatic", 0xb8, Kind.CPRefW),
    ("invokeinterface", 0xb9, Kind.CPRefWUByteZero),
    ("invokedynamic", 0xba, Kind.CPRefWUByteZero),
    ("new", 0xbb, Kind.CPRefW),
    ("newarray", 0xbc, Kind.AType),
    ("anewarray", 0xbd, Kind.CPRefW),
    ("arraylength", 0xbe, Kind.NoOperands),
    ("athrow", 0xbf, Kind.NoOperands),
    
    ("checkcast", 0xc0, Kind.CPRefW),
    ("instanceof", 0xc1, Kind.CPRefW),
    ("monitorenter", 0xc2, Kind.NoOperands),
    ("monitorexit", 0xc3, Kind.NoOperands),
    ("not used", 0xc4, Kind.NoOperands),
    // wide 0xc4
    ("multianewarray", 0xc5, Kind.CPRefWUByte),
    ("ifnull", 0xc6, Kind.Branch),
    ("ifnonnull", 0xc7, Kind.Branch),
    ("goto_w", 0xc8, Kind.BranchW),
    ("jsr_w", 0xc9, Kind.BranchW)
    // impdep 0xfe: PicoJava nonpriv
    // impdep 0xff: Picojava priv

    // wide opcodes
//    ILOAD_W(0xc415, WIDE_CPREF_W),
//    LLOAD_W(0xc416, WIDE_CPREF_W),
//    FLOAD_W(0xc417, WIDE_CPREF_W),
//    DLOAD_W(0xc418, WIDE_CPREF_W),
//    ALOAD_W(0xc419, WIDE_CPREF_W),
//    ISTORE_W(0xc436, WIDE_CPREF_W),
//    LSTORE_W(0xc437, WIDE_CPREF_W),
//    FSTORE_W(0xc438, WIDE_CPREF_W),
//    DSTORE_W(0xc439, WIDE_CPREF_W),
//    ASTORE_W(0xc43a, WIDE_CPREF_W),
//    IINC_W(0xc484, WIDE_CPREF_W_SHORT),
//    RET_W(0xc4a9, WIDE_CPREF_W),
  )
  
  
  val opsize = Map[Kind.Value, Int](
  Kind.NoOperands -> 0,
  Kind.AType -> 1,
  Kind.Branch -> 2,
  Kind.BranchW -> 4,
  Kind.Byte -> 1,
  Kind.CPRef -> 1,
  Kind.CPRefW -> 2,
  Kind.CPRefWUByte -> 3,
  Kind.CPRefWUByteZero -> 4,
  Kind.Dynamic -> 0,
  Kind.Local -> 1,
  Kind.LocalByte -> 1,
  Kind.Short -> 2,
  Kind.WideNoOperands -> 0,
  Kind.WideCPRefW -> 2,
  Kind.WideCPRefWShort -> 2,
  Kind.Unknown -> 0
  )
  
}

