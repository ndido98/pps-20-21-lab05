package u05lab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class ListsTests {

    import u05lab.code.List

    val testList: List[Int] = List(10, 20, 30, 40)

    @Test
    def testZipRight(): Unit = {
        assertEquals(List.nil, List.nil.zipRight)
        assertEquals(List((10, 0), (20, 1), (30, 2), (40, 3)), testList.zipRight)
    }

    @Test
    def testPartition(): Unit = {
        assertEquals((List(20, 30, 40), List(10)), testList.partition(_ > 15))
        assertEquals((List(10, 20, 30, 40), List.nil), testList.partition(_ > 0))
        assertEquals((List.nil, List(10, 20, 30, 40)), testList.partition(_ > 50))
    }

    @Test
    def testSpan(): Unit = {
        assertEquals((List.nil, List(10, 20, 30, 40)), testList.span(_ > 15))
        assertEquals((List(10), List(20, 30, 40)), testList.span(_ < 15))
    }

    @Test
    def testReduce(): Unit = {
        assertEquals(100, testList.reduce(_ + _))
        assertThrows(classOf[UnsupportedOperationException], () => List.nil[Int].reduce(_ + _))
    }

    @Test
    def testTakeRight(): Unit = {
        assertEquals(List(10, 20, 30, 40), testList.takeRight(4))
        assertEquals(List(20, 30, 40), testList.takeRight(3))
        assertEquals(List(30, 40), testList.takeRight(2))
        assertEquals(List(40), testList.takeRight(1))
        assertEquals(List.nil, testList.takeRight(0))
    }

    @Test
    def testCollect(): Unit = {
        assertEquals(List(9, 39), testList.collect { case x if x < 15 || x > 35 => x - 1})
    }
}
