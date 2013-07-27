package nuvoc.compiler.test

import org.scalatest._

import nuvo.compiler.Parser
/**
 * Test for basic nuvo types.
 */
class BasicNuvoTypeSpec extends FlatSpec {


  "nuvoc" should "parse an abstract class" in {
    val parser = new Parser()
    val abstractNuvoType =
      """
        sealed abstract class AbstractNuvoType  extends Tuple
      """
    val nt = parser.parseAll(parser.root, abstractNuvoType)
    val success = () => {
      if (nt.successful) true
      else {
        println(nt)
        false
      }
    }
    assert(success())
  }

  it should "parse a case class with attributes" in {
    val parser = new Parser
    val caseType =
      """
        case class Point(x: Int, y: Int)
      """

    val nt = parser.parseAll(parser.root, caseType)
    val success = () => {
      if (nt.successful) true
      else {
        println(nt)
        false
      }
    }
    assert(success())
  }

  it should "parse a case class with optional attributes" in {
    val parser = new Parser
    val caseType =
      """
        case class Point(x: Int, y: Option[Int])
      """

    val nt = parser.parseAll(parser.root, caseType)
    val success = () => {
      if (nt.successful) true
      else {
        println(nt)
        false
      }
    }
    assert(success())
  }
  it should "parse a case class  with empty key" in {
    val parser = new Parser
    val caseType =
      """
        case class Point(x: Int, y: Int) extends Tuple {
          lazy val key = ()
        }
      """

    val nt = parser.parseAll(parser.root, caseType)
    val success = () => {
      if (nt.successful) true
      else {
        println(nt)
        false
      }
    }
    assert(success())
  }


  it should "parse an algebraic data type" in {
    val parser = new Parser
    val caseType =
      """
        case class NoKey(msg: String) extends Tuple {
          lazy val key = ()
        }

        case class Bounds(w: Int, h: Int)
        sealed abstract class Shape(val color: String, val b: Bounds) extends Tuple {
          lazy val key = color
        }

        case class Circle(override val color: String, x: Float, y: Float, r: Float)
        extends Shape(color, x, y, Bounds((r*2), (r*2)))

        case class Rectangle(override val color: String, ts: Long, x: Float, y: Float, length: Float, width: Float) extends Shape(x, y)

        case class Triangle(override val color: String, ts: Long, x: Float, y: Float, base: Float, height: Float) extends Shape(x, y)
      """

    val nt = parser.parseAll(parser.root, caseType)

    val success = () => {
      if (nt.successful) true
      else {
        println(nt)
        false
      }
    }

    assert(success())
  }

  it should "parse Paxos Types" in {
    val parser = new Parser
    val caseType = """
        abstract class PaxosSamples extends Tuple

        case class Accepted(id: Int, statekey: Long, acceptor: Uuid, serialNumber: Int, proposer: Uuid, epoch: Int) extends PaxosSamples {
          lazy val key = id
        }

        case class Accept(id: Int, statekey: Long, serialNumber: Int, proposer: Uuid, epoch: Int, value: Array[Byte]) extends PaxosSamples {
          lazy val key = id
        }
      """
    val nt = parser.parseAll(parser.root, caseType)

    val success = () => {
      if (nt.successful) true
      else {
        println(nt)
        false
      }
    }

    assert(success())
  }
}
