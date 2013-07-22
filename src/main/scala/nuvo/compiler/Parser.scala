package nuvo.compiler

import scala.util.parsing.combinator._

import scala.Some


/**
 * This class is the parser for the nuvo.space type specification format.
 * nuvo.space types can be defined as follows:
 *
 *    case class Locator(port: Int, addr: java.net.Inet4Address)
 *    case class Header(mvl: Int, guid: Array[Byte])
 *
 *    case class Shape(color: String, x: Int, y: Int, size: Int) {
 *      lazy val key = color
 *    }
 *
 *    Once a Scala case class is fed to the nuvoc compiler the
 *    following helper types are generated:
 *
 *    class ShapeHelper {
 *      def encode(buf: ByteBuffer, e: Encoding): ByteBuffer {
 *          e match {
 *             case NuvoSF => NuvoSF.encode(buf, this)
 *             case JsonSF => JsonSF.encode(buf, this)
 *             default => throw UnsupportedEncoding
 *      }
 *      object ShapeHelperNuvoSF {
 *         def encode(buf: ByteBuffer, s: Shape): ByteBuffer
 *         def decode(buf: ByteBuffer): Shape
 *      }
 *      object ShapeHelperJsonSF {
 *         def encode(buf: ByteBuffer, s: Shape): ByteBuffer
 *         def decode(buf: ByteBuffer): Shape
 *      }
 *    }
 *
 *    val s = Shape("Red", 10, 20, 100)
 *    val buf = nuvo.io.ByteBuffer.direct(1024)
 *    // encoding could be an implicit parameter. It could
 *    // be used explicitly when necessary
 *    buf.put(s);
 */

abstract class Type(val name: String, val alignment: Int)

abstract class PrimitiveType(n: String, a: Int) extends Type(n, a)
case object ByteType extends PrimitiveType("Byte", 1)
case object CharType extends PrimitiveType("Char", 1)
case object ShortType extends PrimitiveType("Short", 2)
case object IntType extends PrimitiveType("Int", 4)
case object LongType extends PrimitiveType("Long", 8)
case object FloatType extends PrimitiveType("Float",4)
case object DoubleType extends PrimitiveType("Double", 8)
case object StringType extends PrimitiveType("String", 4)

case object RawBufferType extends Type("RawBuffer", 4)

case class FunctionType(from: String, to: String) extends Type("FunctionType", 4)



case class CaseType(n: String)  extends Type(n, 1)

case class OptionType(ot: Type) extends Type("Option["+ot.name+"]", 4)

case class ArrayType(elemType: Type) extends Type("Array", 4)
case class ListType(elemType: Type) extends Type("List", 4)

abstract class KeyType
case class PrimitiveKey(attribute: String) extends KeyType
case class CaseKey(keyType: CaseType) extends KeyType
case class TupleKey(attributes: List[String]) extends KeyType
case object NoKey extends KeyType

/**
 * A <code>TranslationUnit</code> is a file containing a single package
 * declaration and a list of types.
 *
 * Notice that for keeping the parser simpler we are assuming that imported
 * types are fully qualified and thus no import directive is accepted at
 * a top level.
 *
 * These restrictions might be removed in future version of the generator.
 *
 * @param pkg the package declaration
 * @param types the list of type declared in this translation unit
 */
case class TranslationUnit(pkg: Option[String], types: List[NuvoType])

sealed abstract class NuvoType(val name: String, val pkg: String, val attributes: List[Attribute], val key: KeyType) {
  def isAbstract: Boolean
}
/**
 * A *ConcreteNuvoType*  represent a type that can be used by the nuvo.io
 * communication middleware. This type are currently restricted version of
 * Scala case classes, which have to define a key value exposing the
 * key list as a tuple.
 * @param name
 * @param attributes
 * @param key
 */
case class ConcreteNuvoType(override val name: String, override val pkg: String,
                            override val attributes: List[Attribute], override val key: KeyType,
                            parent: Option[AbstractNuvoType])
  extends NuvoType(name, pkg, attributes, key) {
  val isAbstract = false
}
case class AbstractNuvoType(override val name: String, override val pkg: String,
                            override val attributes: List[Attribute], override val key: KeyType)
  extends NuvoType(name, pkg, attributes, key) {
  val isAbstract = true
}

case class Attribute(aname: String, atype: Type)




class Parser extends JavaTokenParsers {

  // Skip comments as if they where white-spaces
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  private var superTypeMap = Map[String, AbstractNuvoType]()

  var currentPackage = ""
  // Revise grammar to allow multiple types to be declared in a single file.
  def root = pkgDecl~imptDecl~reptTypeDecl ^^ { case pkgdecl~imptdecl~reptdecl => TranslationUnit(pkgdecl, reptdecl) }
  def imptDecl = rep("import"~repsep(ident, "."))
  def reptTypeDecl = rep(nuvoTypeDecl) ^^ { x => x}
  def pkgDecl = opt("package"~pkgname) ^^ {case Some("package"~pkgname) => {
      currentPackage = pkgname
      Some(pkgname)
    }
  case _ => None
  }

  def nuvoTypeDecl = (abstractNuvoTypeDecl | concreteNuvoTypeDecl)

  // ---------------------------------------
  // Abstract Type
  // ---------------------------------------
  def abstractNuvoTypeDecl = opt("sealed")~"abstract"~"class"~ident~opt(abstractValListDecl)~opt("extends"~baseTypeDecl)~opt(typeBodyDecl) ^^ {
    case _~"abstract"~"class"~ident~Some(abstractValListDecl)~Some("extends"~baseTypeDecl)~Some(typeBodyDecl) => {
      val at = AbstractNuvoType(ident, currentPackage, abstractValListDecl, typeBodyDecl)
      superTypeMap = superTypeMap + (ident -> at)
      at
    }
    case _~"abstract"~"class"~ident~Some(abstractValListDecl)~Some("extends"~baseTypeDecl)~None => {
      val at = AbstractNuvoType(ident, currentPackage, abstractValListDecl, NoKey)
      superTypeMap = superTypeMap + (ident -> at)
      at
    }
    case _~"abstract"~"class"~ident~Some(abstractValListDecl)~None~Some(typeBodyDecl) => {
      val at = AbstractNuvoType(ident, currentPackage, abstractValListDecl, typeBodyDecl)
      superTypeMap = superTypeMap + (ident -> at)
      at
    }
    case _~"abstract"~"class"~ident~Some(abstractValListDecl)~None~None => {
      val at = AbstractNuvoType(ident, currentPackage, abstractValListDecl, NoKey)
      superTypeMap = superTypeMap + (ident -> at)
      at
    }
    case _~"abstract"~"class"~ident~None~Some("extends"~baseTypeDecl)~Some(typeBodyDecl) => {
      val at = AbstractNuvoType(ident, currentPackage, List(), NoKey)
      superTypeMap = superTypeMap + (ident -> at)
      at
    }
    case _~"abstract"~"class"~ident~None~None~Some(typeBodyDecl) => {
      val at = AbstractNuvoType(ident, currentPackage, List(), NoKey)
      superTypeMap = superTypeMap + (ident -> at)
      at
    }
    case _~"abstract"~"class"~ident~None~None~None => {
      val at = AbstractNuvoType(ident, currentPackage, List(), NoKey)
      superTypeMap = superTypeMap + (ident -> at)
      at
    }
  }
  // ---------------------------------------
  // Concrete Type
  // ---------------------------------------
  // ConcreteNuvoType(name: String, pkg: String, attributes: List[Attribute], key: KeyType, parent: Option[AbstractNuvoType])
  def concreteNuvoTypeDecl = "case"~"class"~ident~attribsDecl~opt("extends"~baseTypeDecl)~opt(typeBodyDecl) ^^ {
    case "case"~"class"~ident~attribsDecl~Some("extends"~baseTypeDecl)~Some(typeBodyDecl) => {
      ConcreteNuvoType(ident, currentPackage, attribsDecl, typeBodyDecl, superTypeMap.get(baseTypeDecl))
    }
    case "case"~"class"~ident~attribsDecl~None~Some(typeBodyDecl) => {
      ConcreteNuvoType(ident, currentPackage, attribsDecl, typeBodyDecl, None)
    }
    case "case"~"class"~ident~attribsDecl~Some("extends"~baseTypeDecl)~None => {
      ConcreteNuvoType(ident, currentPackage, attribsDecl, NoKey, superTypeMap.get(baseTypeDecl))
    }
    case "case"~"class"~ident~attribsDecl~None~None => {
      ConcreteNuvoType(ident, currentPackage, attribsDecl, NoKey, None)
    }
  }

  def baseTypeDecl: Parser[String] = ident~opt(genericParameter)~opt(ctorArgListDecl) ^^ {
    case ident~_~_ => ident
  }

  def ctorArgListDecl: Parser[Any]  = "("~repsep((baseTypeDecl| expr), ",")~")"
  def genericParameter = "["~ident~"]"
  def argListDecl = "("~repsep(ident, ",")~")"

  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | decimalNumber | ident |"("~expr~")"

  def typeBodyDecl = "{"~opt(keyDecl)~opt(opsDecl)~"}" ^^ {
    case "{"~Some(keyDecl)~_~"}" => keyDecl
    case "{"~None~_~"}" => NoKey
  }

  def name = ident
  def abstractValDecl = opt("val")~name~":"~atype ^^ {
    case _~name~":"~atype=> Attribute(name, atype)
  }

  def abstractValListDecl = "("~repsep(abstractValDecl, ",")~")" ^^  {x => x._1._2}


  def pkgname = repsep(name, ".") ^^ (x => x.head + ("" /: x.tail)(_ + "." + _))


  def attrName = ident
  def attrib = opt("override"~"val")~attrName~":"~atype ^^ {case _~attrName~":"~atype => Attribute(attrName, atype)}
  def attribsDecl = "("~repsep(attrib, ",")~")" ^^ (x => x._1._2)


  // def atype = byteBufferType | primitiveType | arrayType | listType | caseType
  def atype = sequenceType | primitiveType | functionType | caseType

  def primitiveType = byteType | charType | shortType | intType | longType |floatType | doubleType | stringType

  def sequenceType =  byteBufferType | arrayType | listType

  def functionType = funcFromType~"=>"~funcToType ^^ { case funcFromType~"=>"~funcToType => FunctionType(funcFromType, funcToType)}

  def byteType = "Byte" ^^ (x => ByteType)
  def charType = "Char" ^^ (x => CharType)
  def shortType = "Short" ^^ (x => ShortType)
  def intType = "Int" ^^ (x => IntType)
  def longType = "Long" ^^ (x => LongType)
  def floatType = "Float" ^^ (x => FloatType)
  def doubleType = "Double" ^^ (x => DoubleType)
  def stringType = "String" ^^ (x => StringType)
  def byteBufferType = "RawBuffer" ^^ ( x => RawBufferType)

  def funcFromType = ident ^^ { case ident => ident}
  def funcToType = ident ^^ { case ident => ident}


  def caseType = ident ^^ {case ident => CaseType(ident)}

  def arrayType = primitiveArrayType | caseArrayType
  def primitiveArrayType  = "Array["~primitiveType~"]"  ^^ { case "Array["~primitiveType~"]" => ArrayType(primitiveType)}
  def caseArrayType = "Array["~caseType~"]"  ^^ { case "Array["~caseType~"]" => ArrayType(caseType)}

  def listType = primitiveListType | caseListType
  def primitiveListType  = "List["~primitiveType~"]"  ^^ { case "List["~primitiveType~"]" => ListType(primitiveType)}
  def caseListType = "List["~caseType~"]"  ^^ { case "List["~caseType~"]" => ListType(caseType)}


  def keyDecl =   tupleKeyDecl

  def tupleKeyDecl = opt("lazy"~"val"~"key"~"="~keyList) ^^ {
    case Some("lazy"~"val"~"key"~"="~keyList) => {
      if (keyList.isEmpty) NoKey else TupleKey(keyList)
    }
    case _ => {
      NoKey
    }
  }

  def keyList = atomKeyList | tupleKeyList | emptyKeyList
  def tupleKeyList = "("~repsep(ident, ",")~")" ^^ (x => x._1._2)
  def atomKeyList = ident ^^ {case ident => List(ident)}
  def emptyKeyList = "("~")" ^^ { _ => List() }


  def qualifier = ("private" | "public" | "protected")~opt("["~repsep(name, ".")~"]")
  def opsDecl = rep(opDecl)
  def opDecl = opt("override")~opt(qualifier)~"def"~ident~attribsDecl~opt(rtDecl)~opBody
  def rtDecl = ":"~ident
  def opBody =  msaOptBody | msOptBody
  def msaOptBody = "="~"{"<~anyStmt~>"}"
  def msOptBody = "{"<~anyStmt~>"}"
  def anyStmt = """.*""".r
}

