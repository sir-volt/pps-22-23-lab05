package u05lab.ex1

import org.junit.Test
import org.junit.Assert.*

class ListTest:

  val listTest = List(1, 2, 3, 4)
  val isEven: Int => Boolean = n => n % 2 == 0
  val half: PartialFunction[Int, Int] = {
    case x if isEven(x) => x / 2
  }

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), listTest.zipRight)

  @Test def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), listTest.partition(_ % 2 == 0))

  @Test def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), listTest.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), listTest.span(_ < 3))

  @Test def testReduce(): Unit =
    assertEquals(10, listTest.reduce(_ + _))
    assertEquals(10, List(10).reduce(_ + _))

  @Test def testTakeRight(): Unit =
    assertEquals(List(2, 3, 4), listTest.takeRight(3))

  @Test def testCollect(): Unit =
    assertEquals(List(1, 2), listTest.collect(half))
