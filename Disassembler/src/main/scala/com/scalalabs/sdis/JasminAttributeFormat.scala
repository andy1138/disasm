package com.scalalabs.sdis

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
  
  def formatInnerClasses(x:InnerClassesAttribute, cpool: List[ConstantType]) = {
    "TODO formatInnerClasses "
  }
}