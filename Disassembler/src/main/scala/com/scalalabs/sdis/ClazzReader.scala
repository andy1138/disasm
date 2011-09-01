package com.scalalabs.sdis

object ClazzReader {

  def readClazz(raw: RawClazz): Clazz = {

    var cpool = List[ConstantType]() 
    

    def loadConstantPool(): List[ConstantType] = {
      
      var lst1:List[ConstantType] = Nil
      var loop = 0
      
        val cpSize = raw.u2 -1
        while( loop < cpSize  ) {  {
          val res = findType(raw.u1)
          lst1 = lst1 :+ res 
          loop += 1
          res match {   // if Long or Double the next entry is empty
            case e:ConstantLong => 
              lst1 = lst1 :+ ConstantFILL() 
              loop += 1
            case e:ConstantDouble => 
              lst1 = lst1 :+ ConstantFILL() 
              loop += 1
            case _ =>
          }
          }
        }
      lst1
    }
    
    
    def findType(tag:Int): ConstantType = {
          tag match {
          case 7 => ConstantClazz(raw.u2)
          case 9 => ConstantFieldref(raw.u2, raw.u2)
          case 10 => ConstantMethodref(raw.u2, raw.u2)
          case 11 => ConstantInterfaceMethodref(raw.u2, raw.u2)
          case 8 => ConstantString(raw.u2)
          case 3 => ConstantInteger(raw.u4)
          case 4 => ConstantFloat(raw.u4Float)
          case 5 => ConstantLong((raw.u4.toLong << 32) + raw.u4)   // todo insert null value
          case 6 => ConstantDouble(raw.u4Double)    // todo insert null value
          case 12 => ConstantNameAndType(raw.u2, raw.u2)
          case 1 => { val l = raw.u2; ConstantUtf8(l, expansControlChars( new String(raw.bytes(l)))) }
          }
    }
    
    
    
//    def loadConstantPool(): List[ConstantType] = {
//        var cpLoopCount = 1
//        
//        val cpSize = raw.u2 -1
//        println( "cpool size: " + cpSize)
//        List.tabulate(cpSize  ) { _ => {
//          val tag = raw.u1
//          println("cpool tag: " + tag + "  " + cpLoopCount)
//          
//          val res = (tag match {
//          case 7 => ConstantClazz(raw.u2)
//          case 9 => ConstantFieldref(raw.u2, raw.u2)
//          case 10 => ConstantMethodref(raw.u2, raw.u2)
//          case 11 => ConstantInterfaceMethodref(raw.u2, raw.u2)
//          case 8 => ConstantString(raw.u2)
//          case 3 => ConstantInteger(raw.u4)
//          case 4 => ConstantFloat(raw.u4Float)
//          case 5 => ConstantLong((raw.u4.toLong << 32) + raw.u4)   // todo insert null value
//          case 6 => ConstantDouble(raw.u4Double)    // todo insert null value
//          case 12 => ConstantNameAndType(raw.u2, raw.u2)
//          case 1 => { val l = raw.u2; ConstantUtf8(l, expansControlChars( new String(raw.bytes(l)))) }
//          })
//          println("cpool res: " + res + "  " + cpLoopCount)
//          cpLoopCount += 1
//          res
//        }
//        }
//    }
//    
    def expansControlChars( s: String): String = {
      s.replace("\n", "\\n")
    }
    

    def loadInterfaces():List[Short] = 
        List.tabulate(raw.u2 ) { _ => {
         raw.u2
      }
    }
    

    
    def loadFields():List[FieldInfo] = 
        List.tabulate(raw.u2 ) { _ => {
         new FieldInfo(raw.u2, raw.u2, raw.u2, loadAttributes())
      }
    }

    

    def loadMethods(): List[MethodInfo] =
        List.tabulate(raw.u2 ) { _ => {
        new MethodInfo(raw.u2, raw.u2, raw.u2, loadAttributes())
      }
    }
    

    def loadAttributes(): List[AttributesInfo] = {
      val attrCount = raw.u2
//      println(">>loadAttributes: count: " + attrCount )
      List.tabulate(attrCount ) { _ => {
        loadAttribute()
      }
    }}
    
    
    def loadCodeAttribute(len:Int) =  {
      
      val b = new RawClazz(raw.bytes(len))

      val max_stack = b.u2
      val max_locals = b.u2
      val code_length = b.u4
      val code = b.bytes(code_length)

      val exception_table = List.tabulate(b.u2 ) { _ => {
        new ExceptionInfo(b.u2, b.u2, b.u2, b.u2)
      }} 

      val attCount = b.u2
//      println(">>loadCodeAttribute: attCount: " + attCount)
      val attib = List.tabulate(attCount ) { _ => {
        loadCodeAttr(b)
      }}
      
//      println(">>loadCodeAttribute: attCount: " + attib.toString)

      new CodeAttribute(max_stack, max_locals, code, exception_table.toList, attib )
    }
    
    
   def loadCodeAttr(b:RawClazz): AttributesInfo = {

      val attribute_name_index = b.u2
      val attribute_length = b.u4
//      pr
      
      
//      intln(">>loadCodeAttr: idx: " + attribute_name_index + " len" + attribute_length)

      cpool(attribute_name_index - 1) match {
        case ConstantUtf8(_, "ConstantValue") => new ConstantValueAttribute(b.u2)  // ConstantValue (4.7.2)
        case ConstantUtf8(_, "Code") => loadCodeAttribute(attribute_length)   // Code (4.7.3), 
        // StackMapTable (4.7.4) 
        case ConstantUtf8(_, "Exceptions") => new ExceptionsAttribute(b.bytes(attribute_length))  // Exceptions (4.7.5),
        case ConstantUtf8(_, "InnerClasses") => // InnerClasses (4.7.6),
            val inerClazz = List.tabulate(b.u2 ) { _ => {
              new InnerClasses(b.u2, b.u2, b.u2, b.u2)
            } } 
            new InnerClassesAttribute(inerClazz)
        case ConstantUtf8(_, "EnclosingMethod") => new EnclosingMethodAttribute(b.u2, b.u2)  // EnclosingMethod (4.7.7), 
        case ConstantUtf8(_, "Synthetic") => new SyntheticAttribute()  // Synthetic (4.7.8),
        case ConstantUtf8(_, "Signature") => new SignatureAttribute(b.u2)  // Signature (4.7.9),
        case ConstantUtf8(_, "SourceFile") => new SourceFileAttribute(b.u2)   // SourceFile(4.7.10), 
        case ConstantUtf8(_, "SourceDebugExtension") => new SourceDebugExtensionAttribute(b.bytes(attribute_length)) // SourceDebugExtension (4.7.11), 
        case ConstantUtf8(_, "LineNumberTable") =>     // LineNumberTable (4.7.12), 
            val lines = List.tabulate(b.u2 ) { _ => {
              new LineNumberItem(b.u2, b.u2)
            } } 
            new LineNumberTableAttribute(lines)
          
        case ConstantUtf8(_, "LocalVariableTable") => new LocalVariableTableAttribute(b.bytes(attribute_length))  // LocalVariableTable (4.7.13), 
        case ConstantUtf8(_, "LocalVariableTypeTable") =>  // LocalVariableTypeTable (4.7.14),  
            val lines = List.tabulate(b.u2 ) { _ => {
              new LocalVariableTypeTableItem(b.u2, b.u2, b.u2, b.u2, b.u2)
            } } 
            new LocalVariableTypeTableAttribute(lines)
 
        case ConstantUtf8(_, "Deprecated") => new DeprecatedAttribute()  // Deprecated (4.7.15), 
        case ConstantUtf8(_, "RuntimeVisibleAnnotations") => loadRuntimeVisibleAnnotationsAttribute(b) // new RuntimeVisibleAnnotationsAttribute(b.bytes(attribute_length) )  // RuntimeVisibleAnnotations (4.7.16)
// RuntimeInvisibleAnnotations (4.7.17) 
// RuntimeVisibleParameterAnnotations (4.7.18)
// RuntimeInvisibleParameterAnnotations (4.7.19) 
// AnnotationDefault (4.7.20)
// SourceID (4.7.21)
// CompilationID (4.7.22)
        case ConstantUtf8(_, "ScalaSig") =>  new ScalaSigAttribute(b.u1,b.u1, b.u1)  // ScalaSig
        case ConstantUtf8(_, "Scala") =>  new ScalaAttribute(b.bytes(attribute_length))  // ScalaSig

        case ConstantUtf8(_, s) => new UnknownAttrib(s, b.bytes(attribute_length)) // Unknown
        case x => new UnknownAttrib("", b.bytes(attribute_length))
      }
    }

   

    def loadAttribute(): AttributesInfo = {

      loadCodeAttr(raw)
      
//      val attribute_name_index = raw.u2
//      val attribute_length = raw.u4
//
//      
//      cpool(attribute_name_index - 1) match {
//        case ConstantUtf8(_, "ConstantValue") => new ConstantValueAttribute(raw.u2)
//        case ConstantUtf8(_, "Code") => loadCodeAttribute(attribute_length)
//        case ConstantUtf8(_, "Exceptions") => new ExceptionsAttribute(raw.bytes(attribute_length))
//        case ConstantUtf8(_, "InnerClasses") => new InnerClassesAttribute(raw.bytes(attribute_length))
//        case ConstantUtf8(_, "Synthetic") => new SyntheticAttribute(raw.bytes(attribute_length))
//        case ConstantUtf8(_, "SourceFile") => new SourceFileAttribute(raw.u2)
//        case ConstantUtf8(_, "LineNumberTable") => 
//             val lines = List.tabulate(raw.u2 ) { _ => {
//              new LineNumberItem(raw.u2, raw.u2)
//            }} 
//            new LineNumberTableAttribute(lines)
//        case ConstantUtf8(_, "LocalVariableTable") => new LocalVariableTableAttribute(raw.bytes(attribute_length))
//        case ConstantUtf8(_, "Deprecated") => new DeprecatedAttribute(raw.bytes(attribute_length))
//        case ConstantUtf8(_, "Signature") => new SignatureAttrib(raw.bytes(attribute_length))
//        case ConstantUtf8(_, "RuntimeVisibleAnnotations") => {
//          new RuntimeVisibleAnnotationsAttrib(raw.bytes(attribute_length) /*loadAnnotations()*/)
//        }
//
//        case ConstantUtf8(_, "ScalaSig") => {println(">>ScalaSigAttribute len: " + attribute_length);  new ScalaSigAttribute(raw.u1,raw.u1, raw.u1)}
//
//        case ConstantUtf8(_, s) => new UnknownAttrib(s, raw.bytes(attribute_length))
//        case x => new UnknownAttrib("", raw.bytes(attribute_length))
//      }
    }

    
    val clazz = new Clazz

    clazz.magic = raw.u4

    clazz.minor_version = raw.u2
    clazz.major_version = raw.u2

    cpool = loadConstantPool()
    clazz.cpool = cpool
    
    clazz.access_flags = raw.u2
    clazz.this_class = raw.u2
    clazz.super_class = raw.u2

    clazz.interfaces = loadInterfaces()
    clazz.fields = loadFields()
    clazz.methods = loadMethods()
    clazz.attributes = loadAttributes()
    
    
    clazz
  }

  
  
   def loadRuntimeVisibleAnnotationsAttribute(b:RawClazz): RuntimeVisibleAnnotationsAttribute = {
     
     
     def loadAnnotation(num_annotations:Int):RVAnnonItem = {
         val element_name_index = b.u2
         val num_element_value_pairs = b.u2
         val lst = List.tabulate(num_element_value_pairs ) { _ => {
           loadPairItems()
         }}
         RVAnnonItem(element_name_index, lst)
     }
     
     def loadPairItems() = {
       val type_index = b.u2
//       val num_element_value_pairs = b.u2
//       val lst = List.tabulate(num_element_value_pairs ) { _ => {
         val lst = loadElementValue()
//       }}
       RVElementValuePairsItem(type_index, lst :: Nil)
     }
     
     def loadElementValue(): ElementValue = {
           val tag = b.u1
           tag match {
             case 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's'  => ConstElementValue(tag, b.u2)
             case 'e' => EumConstElementValue(tag, b.u2, b.u2)
             case 'c' => ClassInfoElementValue(tag, b.u2)
//             case '@' => AnnotationElementValue(tag,)
//             case '[' => ArrayElementValue(tag)
             case _ => new ElementValue(tag)
           }
       
     }
     
     
     val num_annotations = b.u2
 
     val annotations = List.tabulate(num_annotations ) { _ => {
       val type_index = b.u2
       val num_element_value_pairs = b.u2
       loadAnnotation(num_annotations)
       
     }}

     new RuntimeVisibleAnnotationsAttribute(annotations )
   }
  
}


