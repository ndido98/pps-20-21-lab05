package u05lab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import u05lab.code.Sequence._

class SequenceTests {

    val listAllSome: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val someList: Option[List[Int]] = Some(List(1, 2, 3))
    val listWithNone: List[Option[Int]] = List(Some(1), None, Some(3))

    @Test
    def testAllSome(): Unit = {
        assertEquals(someList, sequence(listAllSome))
    }

    @Test
    def testWithNone(): Unit = {
        assertEquals(None, sequence(listWithNone))
    }
}
