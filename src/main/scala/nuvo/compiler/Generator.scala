package nuvo.compiler


abstract class GeneratedCode
case class Line(line: String) extends GeneratedCode
case class Block(block: List[GeneratedCode]) extends GeneratedCode

import java.security.MessageDigest

object Generator {
  val NDRGenerator  = new Generator(List(NuvoSF))
  val JSONGenerator = new Generator(List(JsonSF))
  val AllGenerator  = new Generator(List(NuvoSF, JsonSF))
  def  md5Gen = MessageDigest.getInstance("MD5")


  def generateTypeHash(typeName: String): (Long, Long) = {
    val hash = md5Gen.digest(typeName.getBytes)
    val buf = java.nio.ByteBuffer.wrap(hash)
    (buf.getLong(), buf.getLong())
  }

}
/**
 * This class generates the serializers for the list of formats passed as argument
 * @param encoders the list of format for which serializers should be generated
 */
class Generator(val encoders: List[SerializationFormat]) {

  def generatePreamble(tu: TranslationUnit): List[GeneratedCode] = {
    tu.pkg.map {
      s => List(
        Line("package " + tu.pkg.get),
        Line(""),
        // Line("import nuvo.compiler._"),
        Line("import nuvo.nio.prelude._"),
        Line("import nuvo.nio._"),
        Line("import nuvo.core.Tuple"),
        Line("")
      ) ::: generateTypeRegistration(tu)} getOrElse(Nil) ++
      List (
        // Line("import nuvo.compiler._"),
        Line("import nuvo.nio._"),
        Line("")
      ) ::: generateTypeRegistration(tu)
  }

  def generateTypeHashDeclaration(typeName: String): String = {
    val (low, high) = Generator.generateTypeHash(typeName)
    s"val typeHash = ($low"+s"L, $high"+s"L) // $typeName"
  }

  def generateTypeRegistration(tu: TranslationUnit): List[GeneratedCode] = {
    tu.types.filter {
      case cnt: ConcreteNuvoType => true
      case _ => false
    }.map(t => "\"" + t.pkg + "." + t.name + "\"") match {
      case x::xs => {
        val name = tu.types.head.name
        List(
          Line(s"object $name"+"TypeRegistration {"),
          Block(
            List(
              Line("val typeList = List(" + (x /: xs) (_ + ", " + _ ) + ")"),
              Line(""),
              Line("var registerTypeOK = {typeList.foreach(nuvo.nio.SerializerCache.registerType(_)); true}")
            )
          ),
          Line("}"),
          Line("")
        )
      }
      case Nil => List()
    }
  }

  def generateTranslationUnit(tu: TranslationUnit): List[GeneratedCode] = {

    generatePreamble(tu) ++ {
      tu.types map { t =>
        t match {
          case cnt: ConcreteNuvoType => {
            List(
              Line("object "+  cnt.name +s"Helper {"),
              Line(""),
              Block(Line(generateTypeHashDeclaration(cnt.pkg + "." + cnt.name)) :: generateType(cnt)),
              Line("}"),
              Line("")
            )
          }
          case ant: AbstractNuvoType => List()
        }
      }
    }.flatten
  }

  def generateType(nt: NuvoType): List[GeneratedCode] = {

    val body = nt.key match {
      case NoKey =>
        generateSerializerDispatcher(nt) ++
          generateSerializer(nt) ++
          generateDeserializerDispatcher(nt) ++
          generateDeserializer(nt)
      case _ =>
        generateSerializerDispatcher(nt) ++
          generateKeySerializerDispatcher(nt) ++
          generateSerializer(nt) ++
          generateDeserializerDispatcher(nt) ++
          generateKeyDeserializerDispatcher(nt) ++
          generateDeserializer(nt)
    }
    body

  }

  /**
   * Generates the method that dispatch the serialization/deserialization
   * request to the correct implementation depending on the format.
   * @param t
   * @return
   */
  def generateSerializerDispatcher(t: NuvoType): List[GeneratedCode] = t match {
    case nt: ConcreteNuvoType => {
      val serializersList = encoders map {
        case NuvoSF => Line("case NuvoSF  => serializeNuvoSF(buf, t)")
        case JsonSF => Line("case JsonSF  => serializeJsonSF(buf, t)")
      }

      List(
        Line("def serialize(buf: RawBuffer, t: " + nt.name + ", format: SerializationFormat) {"),
        Block(
          List(Line("format match {")) ++ serializersList ++ List(Line("}"))
        ),
        Line("}"),
        Line(" ")
      )
    }
    case ant: AbstractNuvoType => List()
  }

  def generateKeySerializerDispatcher(t: NuvoType): List[GeneratedCode] = t match {
    case nt: ConcreteNuvoType => {
      val keySerializersList = encoders map {
        case NuvoSF => Line("case NuvoSF  => serializeNuvoSF(buf, t)")
        case JsonSF => Line("case JsonSF  => serializeJsonSF(buf, t)")
      }
      List(
        Line("def serializeKey(buf: RawBuffer, t: " + nt.name + ", format: SerializationFormat) {"),
        Block(
          List(Line("format match {")) ++ keySerializersList ++ List(Line("}"))
        ),
        Line("}"),
        Line(" ")
      )
    }
    case ant: AbstractNuvoType => List()
  }

  def generateDeserializerDispatcher(t: NuvoType): List[GeneratedCode] =  t match {
    case nt: ConcreteNuvoType => {
      val deserializersList = encoders map {
        case NuvoSF => Line("case NuvoSF  => deserializeNuvoSF(buf)")
        case JsonSF => Line("case JsonSF  => deserializeJsonSF(buf)")
      }

      List(
        Line("def deserialize(buf: RawBuffer,format: SerializationFormat):" + nt.name + " = {"),
        Block(
          List(Line("format match {")) ++ deserializersList ++ List(Line("}"))
        ),
        Line("}"),
        Line("")
      )
    }
    case ant: AbstractNuvoType => List()
  }

  def generateKeyDeserializerDispatcher(t: NuvoType): List[GeneratedCode] = t match {
    case nt: ConcreteNuvoType => {
      val keyDeserializersList = encoders map {
        case NuvoSF => Line("case NuvoSF  => deserializeNuvoSF(buf)")
        case JsonSF => Line("case JsonSF  => deserializeJsonSF(buf)")
      }
      List(
        Line("def deserializeKey(buf: RawBuffer,format: SerializationFormat) = {"),
        Block(
          List(Line("format match {")) ++ keyDeserializersList ++ List(Line("}"))
        ),
        Line("}"),
        Line("")
      )
    }
    case ant: AbstractNuvoType => List()
  }

  def generateSerializer(nt: NuvoType): List[GeneratedCode] = {
    (encoders map (sf => {Serializer(sf) encodeType (nt)})).flatten ++ (encoders map (sf => {Serializer(sf) encodeTypeKey (nt)})).flatten
  }

  def generateDeserializer(nt: NuvoType): List[GeneratedCode] = {
    (encoders map (sf => {Serializer(sf) decodeType(nt)})).flatten ++
      (encoders map (sf => {Serializer(sf) decodeTypeNoHeader(nt)})).flatten ++
      (encoders map (sf => {Serializer(sf) decodeTypeKey(nt)})).flatten ++
      (encoders map (sf => {Serializer(sf) decodeTypeKeyNoHeader(nt)})).flatten
  }

}
