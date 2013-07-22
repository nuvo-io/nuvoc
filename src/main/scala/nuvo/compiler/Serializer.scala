package nuvo.compiler

abstract class SerializationFormat
// NuvoSF: Nuvo Data Representation
object NuvoSF extends SerializationFormat

// CdrSF: Common Data Representation
object CdrSF extends SerializationFormat

// JsonSFSF: JavaScript Object Notation
object JsonSF extends SerializationFormat

object Serializer {
  def apply(format: SerializationFormat) =
    format match {
      case NuvoSF => NuvoSFSerializer
      case JsonSF => JsonSFSerializer
    }
}
abstract class Serializer {
  val prefix = "__nuvoc_" // prefix used to avoid clash with user defined attributes

  def encodeType(nt: NuvoType): List[GeneratedCode]
  def decodeType(nt: NuvoType): List[GeneratedCode]
  def decodeTypeNoHeader(nt: NuvoType): List[GeneratedCode]
  def encodeTypeKey(nt: NuvoType): List[GeneratedCode]
  def decodeTypeKey(nt: NuvoType): List[GeneratedCode]
  def decodeTypeKeyNoHeader(nt: NuvoType): List[GeneratedCode]
}

object JsonSFSerializer extends Serializer {
  def encodeType(nt: NuvoType): List[GeneratedCode] = ???
  def decodeType(nt: NuvoType): List[GeneratedCode] = ???
  def decodeTypeNoHeader(nt: NuvoType): List[GeneratedCode] = ???
  def encodeTypeKey(nt: NuvoType): List[GeneratedCode] = ???
  def decodeTypeKey(nt: NuvoType): List[GeneratedCode] = ???
  def decodeTypeKeyNoHeader(nt: NuvoType): List[GeneratedCode] = ???
}

object NuvoSFSerializer extends Serializer {


  def encodeAttribute(buf: String)(attr: Attribute): List[Line] =
    this.encodeAttribute(buf, "t.")(attr)

  def encodeAttribute(buf: String, scope: String)(attr: Attribute): List[Line] = {
    val typeName = attr.atype.name
    val attrName = attr.aname
    val intTypeName = IntType.name

    attr.atype match {
      case RawBufferType => {
        val pos = prefix + "pos"
        val lim = prefix + "lim"
        List(
          Line(s"val $pos = $scope" +s"$attrName.position()"),
          Line(s"val $lim = $scope" +s"$attrName.limit()"),
          // Line(scope + s"$attrName.flip()"),
          Line(s"$buf.putInt($lim - $pos)"),
          Line(s"$buf.put($scope" + s"$attrName)"),
          // Line( scope + s"$attrName.clear()"),
          // Line(scope + s"$attrName.limit($lim)"),
          Line(scope + s"$attrName.position($pos)")
        )
      }
      case p: PrimitiveType => { List(Line(s"$buf.put$typeName($scope" + s"$attrName)")) }

      case FunctionType(from, to) => { List(Line(s"$buf.putObject($scope"+ s"$attrName, JavaSF)")) }
        /*
      case FunctionType(from, to) => { List(
        Line(s"val $prefix"+"funName = " + s"$scope"+ s"$attrName"+".getClass.getName"),
        Line(s"println($prefix"+"funName)"),
        Line(s"$buf.putString($prefix"+"funName)")
      )
      }
          */
      case a: ArrayType => {
        val arrayElemTypeName = a.elemType.name
        val arrayserializer = a.elemType match {
          case ByteType =>
            Line(s"$buf.put($scope"+ s"$attrName)")
          case p: PrimitiveType =>
            Line(scope + s"$attrName.foreach($buf.put$arrayElemTypeName (_))")
          case CaseType(name) => {
            Line(scope + s"$attrName.foreach { x => $buf.puObject(x) }")
          }
        }
        List (
          Line(s"$buf.put$intTypeName($scope" + s"$attrName.length)"),
          arrayserializer
        )
      }
      case ListType(etype) => {
        val elemsEncoding = etype match {
          case p: PrimitiveType => {
            List(Line(scope + s"$attrName.foreach( $buf.put" + etype.name + "(_))"))
          }
          case CaseType(name) => {
            List(Line(scope + s"$attrName.foreach { x => $buf.putObject(x) }"))
          }
        }
        List(Line(s"$buf.putInt($scope" + s"$attrName.length)")) ++ elemsEncoding
      }
      case CaseType(name) => {
        List (
          Line(s"$buf.putObject($scope" + s"$attrName)")
        )
      }

    }
  }

  def encodeType(nt: NuvoType): List[GeneratedCode] = nt match {
    case cnt: ConcreteNuvoType => {
      val buf = "buf"
      val MEL = prefix + "MEL"
      val serializedDataLength = prefix + "serializedDataLength"
      val startPosition = prefix + "startPosition"
      val fqName = nt.pkg + "." + nt.name
      val nameLen = fqName.length()

      List(
        Line(s"final def serializeNuvoSF($buf: RawBuffer, t: " + nt.name + ") {"),
        Block(
          List(
            Line("buf.order(ByteOrder.nativeOrder)"),
            Line(s"val $startPosition = $buf.position"),
            Line(s"$buf.position($startPosition + 4)"),
            Line(s"$buf.putInt($nameLen)"),
            Line(s"$buf.put(" + "\""+ fqName +  "\".getBytes())")
          ) :::
            cnt.attributes.map(encodeAttribute(buf)).flatten :::
            List(
              Line(s"val $serializedDataLength = $buf.position - $startPosition - 4"),
              Line(s"val $MEL = ($buf.order.value << 24) | ($serializedDataLength & 0x00ffffff)"),
              Line("buf.order(ByteOrder.littleEndian)"),
              Line(s"$buf.putInt($startPosition, $MEL)")
            )
        ),
        Line("}"),
        Line("")
      )
    }
    case ant: AbstractNuvoType => List()
  }

  def encodeNakedTypeKey(nt: ConcreteNuvoType): List[GeneratedCode] = {
    val buf = "buf"
    val MEL = prefix + "MEL"
    val serializedDataLength = prefix + "serializedDataLength"
    val startPosition = prefix + "startPosition"
    val fqName = nt.pkg + "." + nt.name
    val nameLen = fqName.length()
    val keyAccessor = "k"

    nt.key match {

      case NoKey => List[Line]()

      case TupleKey(keyMembers) => {
        val keyAttr = nt.attributes.filter(a => keyMembers.contains(a.aname))

        val tupleAccessor = keyAttr match {
          case List(a) => List(keyAccessor)
          case a::as => {
            (1 to keyAttr.length) map ("_" + _)
          }
        }

        val tupleAttr =
          (keyAttr zip tupleAccessor) map (e => Attribute(e._2, e._1.atype))

        val keyType = tupleAttr match {
          case List(attr) => attr.atype.name
          case x::xs => "(" + (tupleAttr.head.atype.name /: tupleAttr.tail) (_ + ", " + _.atype.name) + ")"
        }

        val scope = tupleAttr match {
          case List(attr) => ""
          case x::xs => s"$keyAccessor."
        }
        List(
          Line(s"final def serializeNakedKeyNuvoSF($buf: RawBuffer, $keyAccessor: $keyType) = {"),
          Block {
            List(  Line(s"val $startPosition = $buf.position"),
              Line(s"$buf.position($startPosition + 4)"),
              Line(s"$buf.putInt($nameLen)"),
              Line(s"$buf.put(" + "\""+ fqName +  "\".getBytes())")
            ) :::
              (for (attr <- tupleAttr)
              yield encodeAttribute(buf, scope)(Attribute(attr.aname, attr.atype))).flatten :::
              List(
                Line(s"val $serializedDataLength = $buf.position - $startPosition - 4"),
                Line(s"val $MEL = ($buf.order.value << 24) | ($serializedDataLength & 0x00ffffff)"),
                Line(s"$buf.putInt($startPosition, $MEL)")
              )
          },
          Line("}")
        )
      }
    }
  }

  def encodeTypeKey(t: NuvoType): List[GeneratedCode] = t match {
    case nt: ConcreteNuvoType => {
      val buf = "buf"
      val MEL = prefix + "MEL"
      val serializedDataLength = prefix + "serializedDataLength"
      val startPosition = prefix + "startPosition"
      val fqName = nt.pkg + "." + nt.name
      val nameLen = fqName.length()
      nt.key match {
        case NoKey => List[Line]()

        case TupleKey(Nil) => {
          List(
            Line(s"final def serializeKeyNuvoSF($buf: RawBuffer, t: " + nt.name + ") = ()"),
            Line("")
          )
        }
        case TupleKey(attrNames) => {
          List(
            Line(s"final def serializeKeyNuvoSF($buf: RawBuffer, t: " + nt.name + ") = {"),
            Block {
              List(  Line(s"val $startPosition = $buf.position"),
                Line(s"$buf.position($startPosition + 4)"),
                Line(s"$buf.putInt($nameLen)"),
                Line(s"$buf.put(" + "\""+ fqName +  "\".getBytes())")
              ) :::
                ((for (aname <- attrNames;
                       attr <- nt.attributes.find(_.aname == aname)) yield encodeAttribute(buf)(attr)).flatten) :::
                List(
                  Line(s"val $serializedDataLength = $buf.position - $startPosition - 4"),
                  Line(s"val $MEL = ($buf.order.value << 24) | ($serializedDataLength & 0x00ffffff)"),
                  Line(s"$buf.putInt($startPosition, $MEL)")
                )
            },
            Line("}"),
            Line("")
          )  ::: encodeNakedTypeKey(nt)
        }
      }
    }
    case a: AbstractNuvoType => List()
  }
  def decodeAttribute(buf: String)(attr: Attribute): List[Line] = {
    val typeName = attr.atype.name
    val attrName = attr.aname
    val pAttrName = prefix + attrName
    val len = prefix + attrName + "Len"
    val bufPos = prefix + "bufPos"
    val bufLim = prefix + "bufLimit"
    val tmpBuf= prefix + attrName + "tmpBuf"

    attr.atype match {
      case p: PrimitiveType => List(Line(s"val $attrName = $buf.get$typeName()"))

      case FunctionType(from, to) => { List(Line(s"val $attrName = $buf.getObject[$from => $to](JavaSF)")) }
        /*
      case FunctionType(from, to) => {
        List(
          Line(s"val $prefix"+"FunName = buf.getString()"),
          Line(s"val $attrName = Class.forName($prefix"+"FunName).newInstance"+s".asInstanceOf[$from => $to]")
        )
      }
          */
      case RawBufferType => {
        List(
          Line(s"val $len = $buf.getInt()"),
          Line(s"val $bufPos = $buf.position"),
          Line(s"val $bufLim = $buf.limit()"),
          Line(s"val $attrName = RawBuffer.allocate($len)"),
          Line(s"$buf.limit($bufPos + $len)"),
          Line(s"$attrName.put($buf)"),
          Line(s"$buf.limit($bufLim)"),
          Line(s"$attrName.flip()")
        )
      }
      case a: ArrayType => {
        a.elemType match {
          case ByteType => {
            val len = prefix + attrName + "Len"
            List(
              Line(s"val  $len = $buf.get" + IntType.name + "()"),
              Line(s"val $attrName = new Array[Byte]($len)"),
              Line(s"$buf.get($attrName)")
            )
          }
          case p: PrimitiveType => {
            List(
              Line(s"val $len= $buf.get" + IntType.name + "()"),
              Line(s"val $attrName = new Array["+ a.elemType.name + s"]($len)"),
              Line(s"(1 to $len) foreach ($attrName(_) = $buf.get" + a.elemType.name + "())")
            )
          }
          case CaseType(name) => {
            List(
              Line(s"val $len = $buf.get" + IntType.name + "()"),
              Line(s"val $attrName = new Array[$name]($len)"),
              Line(s"(1 to $len) foreach (i => $attrName(i) =  $buf.getObject[$name]())")
            )
          }
        }
      }
      case ListType(etype) => {
        val elemsDecoding = etype match {
          case p: PrimitiveType => {
            List(Line(s"val $attrName = (1 to $len map ( x => $buf.get"+etype.name +")).toList"))
          }

          case CaseType(name) => {
            List(Line(s"val $attrName = (1 to $len map { x => buf.getObject[$name]() }).toList"))
          }
        }
        List(Line(s"val $len = $buf.getInt()")) ++ elemsDecoding
      }
      case CaseType(name) => {
        List (
          Line(s"val $attrName = $buf.getObject[$name]()")
        )
      }
    }
  }


  def decodeType(t: NuvoType): List[GeneratedCode] = t match {
    case nt: ConcreteNuvoType => {
      val alist = nt.attributes map (_.aname)
      val args = alist.head + (" " /: alist.tail) (_ + ", " + _)
      val buf = "buf"
      val MEL = prefix + "MEL"
      val endianness =  prefix + "endianess"
      val serializeDataLength = prefix + "serializeDataLength"
      val startPosition = prefix + "startPosition"
      val fqName = prefix + "fqName"
      val nameLen = prefix + "nameLen"
      val bbuf = prefix + "bbuf"
      val actualTypeName = nt.pkg + "." + nt.name
      val s = "\"bbuf\""
      List(
        Line(s"final def deserializeNuvoSF($buf: RawBuffer) : " + nt.name+ " = {"),
        Block(
          List(
            Line(s"$buf.order(LittleEndian)"),
            Line(s"val $MEL = $buf.getInt()"),
            Line(s"val $endianness =  ($MEL >> 24).toByte"),
            Line(s"val $serializeDataLength =  ($MEL & 0x00ffffff)"),
            Line(s"$buf.order($endianness match { case LittleEndian.value => LittleEndian; case BigEndian.value  => BigEndian; case _ => { $buf.position($buf.position + $serializeDataLength) ; throw new RuntimeException("+ "\"Invalid Format\")}})"),
            Line(s"val $startPosition =  $buf.position"),
            Line(s"val $nameLen = $buf.getInt()"),
            Line(s"val $bbuf = new Array[Byte]($nameLen)"),
            Line(s"$buf.get($bbuf)"),
            Line(s"val $fqName = new String($bbuf)"),
            Line(s"if ($fqName != " + "\"" + actualTypeName + "\"" + ") throw new RuntimeException(\"Cannot deserialize  +" + fqName + "+ as a " + actualTypeName +"\")")
          ) :::
            (nt.attributes.map(decodeAttribute(buf)).flatten) :::
            List(
              Line(s"$buf.position($startPosition + $serializeDataLength)"),
              Line("new "+ nt.name + "("+ args+")")
            )
        ),
        Line("}"),
        Line("")
      )
    }
    case a: AbstractNuvoType => List()
  }


  def decodeTypeNoHeader(t: NuvoType) =  t match {
    case nt: ConcreteNuvoType => {
      val alist = nt.attributes map (_.aname)
      val args = alist.head + (" " /: alist.tail) (_ + ", " + _)
      val buf = "buf"

      List(
        Line(s"def deserializeNoHeaderNuvoSF($buf: RawBuffer) : " + nt.name+ " = {"),
        Block(
          (nt.attributes.map(decodeAttribute(buf)).flatten) :::
            List(
              Line("new "+ nt.name + "("+ args+")")
            )
        ),
        Line("}"),
        Line("")
      )
    }
    case a: AbstractNuvoType => List()
  }


  def decodeTypeKey(t: NuvoType): List[GeneratedCode] = t match {
    case nt: ConcreteNuvoType => {
      val buf = "buf"
      val MEL = prefix + "MEL"
      val endianness =  prefix + "endianess"
      val serializeDataLength = prefix + "serializeDataLength"
      val startPosition = prefix + "startPosition"
      val fqName = prefix + "fqName"
      val nameLen = prefix + "nameLen"
      val bbuf = prefix + "bbuf"
      val actualTypeName = nt.pkg + "." + nt.name
      val s = "\"bbuf\""
      val result = prefix + "result"
      nt.key match {
        case NoKey => List()

        case TupleKey(Nil) => {
          List(Line(s"final def deserializeKeyNuvoSF($buf: RawBuffer) = ()"))
        }

        case TupleKey(attrNames) => {
          val returnValue =  attrNames match {
            case List(aname) => aname
            case x::xs => "("+ (attrNames.head /: attrNames.tail)(_ +", " + _) + ")"
          }

          List(
            Line(s"final def deserializeKeyNuvoSF($buf: RawBuffer) = {"),
            Block(
              List(
                Line(s"$buf.order(LittleEndian)"),
                Line(s"val $MEL = $buf.getInt()"),
                Line(s"val $endianness =  ($MEL >> 24).toByte"),
                Line(s"val $serializeDataLength =  ($MEL & 0x00ffffff)"),
                Line(s"$buf.order($endianness match { case LittleEndian.value => LittleEndian; case BigEndian.value  => BigEndian; case _ => { $buf.position($buf.position + $serializeDataLength) ; throw new RuntimeException("+ "\"Invalid Format\")}})"),
                Line(s"val $startPosition =  $buf.position"),
                Line(s"val $nameLen = $buf.getInt()"),
                Line(s"val $bbuf = new Array[Byte]($nameLen)"),
                Line(s"$buf.get($bbuf)"),
                Line(s"val $fqName = new String($bbuf)"),
                Line(s"if ($fqName != " + "\"" + actualTypeName + "\"" + ") throw new RuntimeException(\"Cannot deserialize  +" + fqName + "+ as a " + actualTypeName +"\")")
              ) :::

                (for (aname <- attrNames;
                      attr <- nt.attributes.find(_.aname == aname)) yield decodeAttribute(buf)(attr)).flatten :::
                List(Line(s"val $result = $returnValue")):::
                List(
                  Line(s"$buf.position($startPosition + $serializeDataLength)"),
                  Line(s"$result")
                )
            ),
            Line("}")
          )
        }
      }
    }
    case a: AbstractNuvoType => List()
  }

  def decodeTypeKeyNoHeader(t: NuvoType): List[GeneratedCode] = t match {
    case nt: ConcreteNuvoType => {
      val buf = "buf"
      val result = prefix + "result"
      nt.key match {
        case NoKey => List()

        case TupleKey(Nil) =>
          List(Line(s"final def deserializeKeyNoHeaderNuvoSF($buf: RawBuffer) = ()"))

        case TupleKey(attrNames) => {
          val returnValue =  attrNames match {
            case List(aname) => aname
            case x::xs => "("+ (attrNames.head /: attrNames.tail)(_ +", " + _) + ")"
          }
          List(
            Line(s"final def deserializeKeyNoHeaderNuvoSF($buf: RawBuffer) = {"),
            Block(
              (for (aname <- attrNames;
                    attr <- nt.attributes.find(_.aname == aname)) yield decodeAttribute(buf)(attr)).flatten :::
                List(Line(s"val $result = $returnValue")):::
                List(Line(s"$result"))
            ),
            Line("}")
          )
        }
      }
    }
    case a: AbstractNuvoType => List()
  }
}

