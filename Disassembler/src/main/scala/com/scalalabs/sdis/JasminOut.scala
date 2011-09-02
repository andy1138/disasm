package com.scalalabs.sdis

import scala.reflect.NameTransformer
import java.io.PrintStream

import scala.collection.mutable.Map

/**
 * Output in jasmin compatible format
 * @see http://jasmin.sourceforge.net/xt.html
 */
class JasminOut(clazz: Clazz, out: PrintStream) {

  implicit val cpool = clazz.cpool

  def disassemble() {
    out.println(".magic " + "0x" + clazz.magic.toHexString)
    out.println(".bytecode " + clazz.major_version + "." + clazz.minor_version)
    out.print(scalaSigName())
    sourceFile()
    clazzName()
    out.print(superName())

    out.print(mkInterfacesString)
    out.print(innerClazzList())
    if (Preference.displayConstPool) mkConstantPoolString
    mkFieldsString
    formatMethods
    mkClassAttribString
    out.println("\n")
  }

  def mkInterfacesString: String =  {
//    val x = clazz.interfaces.map(idx => interfaceName(idx).mkString)
//    ".implements " + x.mkString("\n.implements ")
    
    clazz.interfaces.foldLeft("") { (s, idx) =>
      s +  ".implements " + interfaceName(idx).mkString + "\n"
      
    }
    
  }
  
  def innerClazzList(): String = {
    clazz.attributes.foldLeft("") { (s, v) =>
      s + (v match {
        case x: InnerClassesAttribute => JasminAttributeFormat.formatInnerClasses(x, cpool)
        case _ => ""
      }) 
    }
  }

  def mkFieldsString {
    def mkDesc(idx: Int): String = clazz.cpool(idx - 1) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }

    def mkName(idx: Int): String = clazz.cpool(idx - 1) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }

    clazz.fields.foreach(f => {
      out.println(".field " + mkAccessFlagString(f.access_flags) + mkName(f.name_index) + "" + mkDesc(f.descriptor_index))
      f.attributes.foreach(a => {
        a match {
          case v: ConstantValueAttribute => out.println("  .unknown " + v)
          case v: SyntheticAttribute => out.println("  .unknown " + v)
          case SignatureAttribute(signature_index) => out.println("  .signature \"" + cpool(signature_index - 1) + "\"")
          case v: DeprecatedAttribute => out.println("  .deprecated ")
          case v: RuntimeVisibleAnnotationsAttribute => out.println("  .unknown " + v)
          case v: RuntimeInvisibleAnnotationsAttribute => out.println("  .unknown " + v)
          case v => out.println("  .unknown " + v)
        }

      })
      if (f.attributes.length > 0) {
        out.println(".end field")
      }
    })
    //    val r = clazz.fields.mkString(".field ", "\n.field ", "")
    //    out.println(r)
  }

  def mkConstantPoolString {
    out.println("\n\nConstant pool:")
    val s = "const #" + clazz.cpool.zipWithIndex.map(x => ("" + (x._2 + 1) + " " + x._1)).mkString("\nconst #") + "\n"
    out.println(s)
  }

  def formatMethods {
    clazz.methods.map(m => formatAMethod(m)) //  _.mkString(c.cpool))
  }

  def formatAMethod(m: MethodInfo) {
    out.print(methodName(m))
    out.println(methodAttributes(m))
    out.println(".end method")
    //    out.println( mkAttribString )
  }

  def methodAttributes(m: MethodInfo): String = {
    m.attributes.foldLeft("") { (s, attrib) => s + methodAttrib(attrib) }
  }

  def methodAttrib(attrib: AttributesInfo): String = {
    attrib match {
      case c: CodeAttribute => JasminAttributeFormat.formatCode(c, clazz.cpool)
      case l: LineNumberTableAttribute => "\n" + JasminAttributeFormat.formatLineNumber(l)
      case i: SignatureAttribute => "" //JasminAttributeFormat.formatSignature(s, clazz.cpool)
      case LocalVariableTableAttribute(_) => "\n.locals"
      case r: RuntimeVisibleAnnotationsAttribute => "\n.runtimeVisibleAnnotations " + r
      case _: DeprecatedAttribute => ""
      case x => "\n//Unknown " + x

    }
  }

  def mkAccessFlagString(flag: Int): String = {
    import MethodInfo._
    (if ((flag & ACC_PUBLIC) > 0) "public " else "") +
      (if ((flag & ACC_PRIVATE) > 0) "private " else "") +
      (if ((flag & ACC_PROTECTED) > 0) "protected " else "") +
      (if ((flag & ACC_STATIC) > 0) "static " else "") +
      (if ((flag & ACC_FINAL) > 0) "final " else "") +
      (if ((flag & ACC_SYNCHRONIZED) > 0) "synchronized " else "") +
      (if ((flag & ACC_BRIDGE) > 0) "bridge " else "") +
      (if ((flag & ACC_VARARGS) > 0) "varargs " else "") +
      (if ((flag & ACC_NATIVE) > 0) "native " else "") +
      (if ((flag & ACC_INTERFACE) > 0) "interface " else "") +
      (if ((flag & ACC_ABSTRACT) > 0) "abstract " else "") +
      (if ((flag & ACC_STRICT) > 0) "strict " else "") +
      (if ((flag & ACC_SYNTHETIC) > 0) "SYNTHETIC " else "") +
      (if ((flag & ACC_ANNOTATION) > 0) "ANNOTATION " else "") +
      (if ((flag & ACC_ENUM) > 0) "enum " else "")

  }

  def mkClassAttribString {
    val str1: String = clazz.attributes.foldLeft("") { (s, v) =>
      s + (v match {
        case x: EnclosingMethodAttribute => JasminAttributeFormat.formatEnclosingMethod(x, cpool)
        case x: SyntheticAttribute => ".synthetic"
        //         case x:SignatureAttribute =>
        case x: SourceDebugExtensionAttribute => JasminAttributeFormat.formatSourceDebugExtension(x)
        //         Deprecated
        case x: RuntimeInvisibleAnnotationsAttribute =>

        case _: SourceFileAttribute => "" // nothing
        case _: ScalaSigAttribute => "" // nothing
        case ScalaAttribute(b) => ".scala " + b.length + " " + b.mkString(", ") // nothing
        case RuntimeVisibleAnnotationsAttribute(x) => //"\n.runtimeVisibleAnnotations "  + (x.toList.mkString(","))
          x.foldLeft("\n") { (s1, z) => s1 + mkRuntimeVisibleAnnotationsAttributeString(z) }
        case x: InnerClassesAttribute => "" //JasminAttributeFormat.formatInnerClasses(x, cpool)
        case x => "\n//Unknown " + x + " " + v
      }) + 
      "\n"
    }
    out.println(str1)
  }

  //   private object unpickler extends scala.reflect.generic.UnPickler {
  //    val global = _
  //  }

  def mkRuntimeVisibleAnnotationsAttributeString(item: RVAnnonItem): String = {
    val name = cpool(item.type_index - 1)
    println(">>mkRuntimeVisibleAnnotationsAttributeString " + name)
    name match {
      case ConstantUtf8(_, "Lscala/reflect/ScalaSignature;") =>
        ".scalaSignature " +
          item.element_value_pairs.foldLeft(" ") { (s, v) =>

            val value = v.element_value_pairs match {
              case c: ConstElementValue =>
                cpool(c.const_value_index - 1) match {
                  case ConstantUtf8(_, txt) =>
                    val txt1 = txt.getBytes()
                    txt1.mkString(",")
                  //                scala.reflect.generic.ByteCodecs.decode( txt1 )
                  //                txt1.foldLeft("") { (a,b) => a + "," + (b.toInt & 0xff).toHexString + "(" + b.toChar + ")" }
                  case _ =>
                }
              case e => "other: " + e
            }

            s + cpool(v.element_name_index - 1) + " " + value
          }
      case ConstantUtf8(_, n) => ".runtimeVisibleAnnotations " + n
      case _ => ".runtimeVisibleAnnotations " + name
    }
  }

  //  def sourceFileOLD() {
  //    clazz.attributes.map( _ match {
  //      case SourceFileAttribute(idx) => cpool(idx - 1) match {
  //        case ConstantUtf8(_, txt) => out.println(".source '" + txt + "'")
  //        case _ =>
  //      }
  //      case _ =>
  //    })
  //  }

  def sourceFile() {
    clazz.attributes.map(_ match {
      case SourceFileAttribute(idx) => cpConstantUtf8(idx) match {
        case Some(txt) => out.println(".source '" + txt + "'")
        case None =>
      }
      case _ =>
    })
  }

  def clazzName() {
    cpClazzName(clazz.this_class) match {
      case Some(name) => out.println(".class '" + name + "'")
      case None => out.println(".class MISSING idx: " + clazz.this_class)
    }
  }

  def superName():String = {
     ".super '" + cpClazzName(clazz.super_class).getOrElse("MISSING: cpool:" + clazz.super_class) + "'\n"
//    constPoolClazz(clazz.super_class) match {
//      case Some(name) => out.println(".super '" + name + "'")
//      case None => out.println(".super MISSING idx: " + clazz.super_class)
//    }
  }

  def cpClazzName(idx: Int): Option[String] = {
    clazz.cpool(idx - 1) match {
      case ConstantClazz(idx) => cpConstantUtf8(idx)
      case _ => None
    }
  }

  def cpConstantUtf8(idx: Int): Option[String] = {
    cpool(idx - 1) match {
      case ConstantUtf8(_, txt) => Some(txt)
      case _ => None
    }
  }

  def methodName(m: MethodInfo):String =  {
    def findParams: String = clazz.cpool(m.descriptor_index - 1) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }

    def findName = clazz.cpool(m.name_index - 1) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }

    "\n; " + NameTransformer.decode(findName) + "\n" +
    ".method " + mkAccessFlagString(m.access_flags) + findName + " " + findParams +
    isDeprecated(m.attributes) +
    isSignature(m.attributes)
//    out.println("")
//    out.println("")
//    out.println("; " + NameTransformer.decode(mkName))
//    out.println(".method " + mkAccessFlagString(m.access_flags) + mkName + " " + mkParams + "  " + m.descriptor_index)
//    outDeprecated(m.attributes)
//    outSignature(m.attributes)
  }

  def isSignature(attrib: List[AttributesInfo]):String =  {
    attrib.foldLeft("")((s,x) => x match {
      case SignatureAttribute(signature_index) => s + "\n.signature \"" + cpool(signature_index - 1) + "\""
      case _ => s
    })
  }

  def isDeprecated(attrib: List[AttributesInfo]): String = {
    attrib.foldLeft("")((s, x) => x match {
      case _: DeprecatedAttribute => s + "\n.deprecated"
      case _ => s
    })
  }

  def scalaSigName():String =  {
    clazz.attributes.foldLeft("")((s,v) => v match {
      case ScalaSigAttribute(major, minor, entries) => s + ".scalasig " + major + "." + minor + "  " + entries +"\n"
      case _ => s 
    })
  }

  def interfaceName(idx: Short): Option[String] = {
//    println(">>interfaceName  idx: " + idx + " cpool: " + clazz.cpool.length)
    Some((clazz.cpool(idx - 1) match {
      case ConstantClazz(idx) => clazz.cpool(idx - 1) match {
        case ConstantUtf8(_, txt) => "'" + txt + "'"
        case _ => "" + (clazz.this_class - 1)
      }
      case _ => "" + (clazz.this_class - 1)

    }).mkString(""))
  }

}