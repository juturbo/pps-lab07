package ex1

import org.scalatest.matchers.should.Matchers.*
import Parsers.*

class ParserWithScalaTest extends org.scalatest.funsuite.AnyFunSuite :

  test("Basic Parser"):
    val parser = new BasicParser(Set('a', 'b', 'c'))
    parser.parseAll("aabc".toList) should be (true)
    parser.parseAll("aabcdc".toList) should be (false)
    parser.parseAll("".toList) should be (true)

  test("Not Empty Parser"):
    def parserNE = new NonEmptyParser(Set('0', '1'))
    parserNE.parseAll("0101".toList) should be (true)
    parserNE.parseAll("0123".toList) should be (false)
    parserNE.parseAll(List()) should be (false)

  test("Not Two Consecutive Parser"):
    def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    parserNTC.parseAll("XYZ".toList) should be (true)
    parserNTC.parseAll("XYYZ".toList) should be (false)
    parserNTC.parseAll("".toList) should be (true)

  test("Not Empty And Not Two Consecutive Parser"):
    def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    parserNTCNE.parseAll("XYZ".toList) should be (true)
    parserNTCNE.parseAll("XYYZ".toList) should be (false)
    parserNTCNE.parseAll("".toList) should be (false)

  test("String Parser"):
    def sparser: Parser[Char] = "abc".charParser()
    sparser.parseAll("aabc".toList) should be (true)
    sparser.parseAll("aabcdc".toList) should be (false)
    sparser.parseAll("".toList) should be (true)
