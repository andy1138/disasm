package com.scalalabs.sdis

import MethodInfo._

object JasminAttributeFormat {

  def formatCode(c:CodeAttribute, cpool: List[ConstantType]) = {
    val codeBlock = new DisasmCodeBlock(c.code, cpool)
      
    val res = codeBlock.mkCode(c.code, Nil)
    res
  }
  
  def formatLineNumber(l:LineNumberTableAttribute) = {
    "TODO: formatLineNumber "
  }

  
  def formatEnclosingMethod(x:EnclosingMethodAttribute, cpool: List[ConstantType]) = {
    "TODO formatEnclosingMethod "
  }
  
  def formatSourceDebugExtension(x:SourceDebugExtensionAttribute) = {
    "TODO formatSourceDebugExtension "
  }
  
  def formatInnerClasses(innClazzList:InnerClassesAttribute, cpool: List[ConstantType]) = {
    innClazzList.innClasses.foldLeft(""){ (s,innClazz) => 
      s + ".inner " + clazzOrInterface(innClazz.inner_class_access_flags) + " " + 
      acccessFlag(innClazz.inner_class_access_flags) + " " +
      ( if(innClazz.inner_name_index>0) cpool(innClazz.inner_name_index - 1) else "" ) + " " + 
      "inner " + ( if(innClazz.inner_class_info_index>0) cpClazzName(innClazz.inner_class_info_index, cpool) else "" ) + " " + 
      "outer " +( if(innClazz.outer_class_info_index>0) cpClazzName(innClazz.outer_class_info_index, cpool) else "" ) + " " +
      "\n"
      
    }
  }
  
    def cpClazzName(idx: Int,  cpool: List[ConstantType]): String = {
      def cpConstantUtf8(i: Int): String = {
        cpool(i - 1) match {
          case ConstantUtf8(_, txt) => txt
          case _ => ""
        }
      }

      
    cpool(idx - 1) match {
      case ConstantClazz(i) => cpConstantUtf8(i)
      case _ => ""
    }
  }

    def clazzOrInterface(flag:Int): String = {
      if ((flag & ACC_INTERFACE) > 0) "interface " else "class"
    }
    
    
  def acccessFlag(flag: Int): String = {
    (if ((flag & ACC_PUBLIC) > 0) "public " else "") +
      (if ((flag & ACC_PRIVATE) > 0) "private " else "") +
      (if ((flag & ACC_PROTECTED) > 0) "protected " else "") +
      (if ((flag & ACC_STATIC) > 0) "static " else "") +
      (if ((flag & ACC_FINAL) > 0) "final " else "") +
//      (if ((flag & ACC_SYNCHRONIZED) > 0) "synchronized " else "") +
//      (if ((flag & ACC_BRIDGE) > 0) "bridge " else "") +
//      (if ((flag & ACC_VARARGS) > 0) "varargs " else "") +
//      (if ((flag & ACC_NATIVE) > 0) "native " else "") +
      (if ((flag & ACC_INTERFACE) > 0) "interface " else "") +
      (if ((flag & ACC_ABSTRACT) > 0) "abstract " else "") +
      (if ((flag & ACC_STRICT) > 0) "strict " else "") +
      (if ((flag & ACC_SYNTHETIC) > 0) "SYNTHETIC " else "") +
      (if ((flag & ACC_ANNOTATION) > 0) "ANNOTATION " else "") +
      (if ((flag & ACC_ENUM) > 0) "enum " else "")

  }
  
}