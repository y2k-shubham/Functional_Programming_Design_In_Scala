
abstract class JSON {

  def convertToString: String = this match {
    case JSeq(elems) => {
      val open: String = "[\n\t"
      val close: String = "\n]"
      val body: String = elems.map(_.convertToString).
        mkString(",\n").
        replaceAll("\n", "\n\t")

      open + body + close
    }

    case JObj(binding) => {
      val open: String = "{\n\t"
      val close: String = "\n}"
      val body: String = binding.toList.map { tup: (String, JSON) =>
        tup._1 + ": " + tup._2.convertToString
      }.mkString(",\n").
        replaceAll("\n", "\n\t")

      open + body + close
    }

    case JStr(str) => str

    case JNum(num) => num.toString

    case JBool(bool) => bool.toString

    case JNull => "NULL"

    case invalid => throw new IllegalArgumentException(s"Unkown JSON: ${invalid}")
  }
}

case class JSeq(elems: List[JSON]) extends JSON {

  override def toString: String = {
    val open: String = "[\n\t"
    val close: String = "\n]"
    val body: String = elems.map(_.toString).
      mkString(",\n").
      replaceAll("\n", "\n\t")

    open + body + close
  }
}

case class JObj(binding: Map[String, JSON]) extends JSON {

  override def toString: String = {
    val open: String = "{\n\t"
    val close: String = "\n}"
    val body: String = binding.toList.map { tup: (String, JSON) =>
      s"${tup._1}: ${tup._2}"
    }.mkString(",\n").
      replaceAll("\n", "\n\t")

    open + body + close
  }
}

case class JNum(num: Double) extends JSON {

  override def toString: String = num.toString
}

case class JStr(str: String) extends JSON {

  override def toString: String = str
}

case class JBool(bool: Boolean) extends JSON {

  override def toString: String = bool.toString
}

case object JNull extends JSON {

  override def toString: String = "NULL"
}

/**
  * data = {
  * "firstName": "John",
  * "secondName": "Smith",
  * "address": {
  * "streetAddress": "21 2nd Street",
  * "state": "NY",
  * "postalCode": 10021
  * },
  * "phoneNumbers": [
  * {
  * "type": "home",
  * "number": "212 555-1234"
  * },
  * {
  * "type": "fax",
  * "number": "666 555-4567"
  * }
  * ]
  * }
  */
val data: JSON = JObj(Map(
  "firstName" -> JStr("John"),
  "secondName" -> JStr("Smith"),
  "address" -> JObj(Map(
    "streetAddress" -> JStr("21 2nd Street"),
    "state" -> JStr("NY"),
    "postalCode" -> JNum(10021)
  )),
  "phoneNumbers" -> JSeq(List(
    JObj(Map(
      "type" -> JStr("home"),
      "number" -> JStr("212 555-1234")
    )),
    JObj(Map(
      "type" -> JStr("fax"),
      "number" -> JStr("666 555-4567")
    ))
  ))
))

print(data)
print(data.convertToString)
