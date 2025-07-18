package ex1

/** Consider the Parser example shown in previous lesson. Analogously to
  * NonEmpty, create a mixin NotTwoConsecutive, which adds the idea that one
  * cannot parse two consecutive elements which are equal. Use it (as a mixin)
  * to build class NotTwoConsecutiveParser, used in the testing code at the end.
  * Note we also test that the two mixins can work together!!
  */

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?
  def end: Boolean // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean =
    (seq forall parse) & end // note &, not &&

object Parsers:
  extension (s: String)
    def charParser(): Parser[Char] = new BasicParser(s.toSet)

class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end: Boolean = true

trait NonEmpty[T] extends Parser[T]:
  private[this] var empty = true
  abstract override def parse(t: T): Boolean =
    empty = false;
    super.parse(t) // who is super??
  abstract override def end: Boolean = !empty && super.end

class NonEmptyParser(chars: Set[Char])
    extends BasicParser(chars)
    with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:
  private[this] var previous: Option[T] = None
  private[this] var twoCons = false

  abstract override def parse(t: T): Boolean =
    if previous contains t
      then twoCons = true
      else previous = Some(t);
    super.parse(t)
  abstract override def end: Boolean = !twoCons && super.end

class NotTwoConsecutiveParser(chars: Set[Char])
    extends BasicParser(chars) with NotTwoConsecutive[Char]

trait ShortenThenN[T](n: Int) extends Parser[T]:
  var count: Int = 0
  abstract override def parse(t: T): Boolean =
    count += 1
    super.parse(t)

  abstract override def end: Boolean = !(count > n) && super.end

class ShortenThenNParser(chars: Set[Char])(n: Int)
  extends BasicParser(chars) with ShortenThenN[Char](n)

@main def checkParsers(): Unit =
  import ex1.Parsers.charParser
  println("Test Basic parser [true, false, true]")
  def parser = new BasicParser(Set('a', 'b', 'c'))
  println(parser.parseAll("aabc".toList)) // true
  println(parser.parseAll("aabcdc".toList)) // false
  println(parser.parseAll("".toList)) // true

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  println("Test Non Empty parser [true, false, false]")
  def parserNE = new NonEmptyParser(Set('0', '1'))
  println(parserNE.parseAll("0101".toList)) // true
  println(parserNE.parseAll("0123".toList)) // false
  println(parserNE.parseAll(List())) // false

  // NotTwoConsecutive[Char] -> BasicParser -> Parser[Char]
  println("Test Not Two Cons parser [true, false, true]")
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  println(parserNTC.parseAll("XYZ".toList)) // true
  println(parserNTC.parseAll("XYYZ".toList)) // false
  println(parserNTC.parseAll("".toList)) // true

  // note we do not need a class name here, we use the structural type
  println("Test parser NTCNE [true, false, false]")
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z'))
    with NotTwoConsecutive[Char]
    with NonEmpty[Char]
  println(parserNTCNE.parseAll("XYZ".toList)) // true
  println(parserNTCNE.parseAll("XYYZ".toList)) // false
  println(parserNTCNE.parseAll("".toList)) // false

  println("Test sparser [true, false, true]")
  def sparser: Parser[Char] = "abc".charParser()
  println(sparser.parseAll("aabc".toList)) // true
  println(sparser.parseAll("aabcdc".toList)) // false
  println(sparser.parseAll("".toList)) // true

  println("Test ShortenThenN parser [true, false, true]")
  def parserSTN = new ShortenThenNParser(Set('a', 'b', 'c'))(5)
  println(parserSTN.parseAll("aabc".toList)) // true
  println(parserSTN.parseAll("aabbcc".toList)) // false
  println(parserSTN.parseAll("ac".toList)) // true
  println(parserSTN.parseAll("".toList)) // true
