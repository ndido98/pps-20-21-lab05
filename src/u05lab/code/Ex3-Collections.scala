package u05lab.code

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {

    case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
        override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
    }

    def measure[T](msg: String = "")(expr: => T)(nTries: Int = 3): Seq[MeasurementResults[T]] = {
        val measurements: Array[MeasurementResults[T]] = Array.ofDim(nTries)
        for (i <- 0 until nTries) {
            val startTime = System.nanoTime()
            val res = expr
            val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
            if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " ns; " + duration.toMillis + " ms")
            measurements(i) = MeasurementResults(res, duration)
        }
        val avgNanos = (measurements map (_.duration.toNanos)).sum / measurements.length.toDouble
        val avgMillis = (measurements map (_.duration.toMillis)).sum / measurements.length.toDouble
        if (msg.nonEmpty) println(
            msg + " -- Average of " + nTries + " tries -- " +
                avgNanos.formatted("%.4f") + " ns; " + avgMillis.formatted("%.4f") + " ms")
        measurements
    }
}


object CollectionsTest extends App {

    import PerformanceUtils._
    import scala.collection.immutable.List

    def _linearRead(name: String)(first: => Int)(last: => Int)(size: => Int): Unit = {
        measure(name + " first")(first)()
        measure(name + " last")(last)()
        measure(name + " size")(size)()
    }
    def _linearInsert(name: String)(prepend: => Any)(append: => Any)(insert: => Any): Unit = {
        measure(name + " prepend")(prepend)(1)
        measure(name + " append")(append)(1)
        measure(name + " insert")(insert)(1)
    }
    def _linearDelete(name: String)(first: => Any)(last: => Any)(middle: => Any): Unit = {
        measure(name + " delete first")(first)(1)
        measure(name + " delete last")(last)(1)
        measure(name + " delete middle")(middle)(1)
    }

    val range = 1 to 100000

    val immutableInsert: Seq[_] => Unit = s => {
        val (front, back) = s splitAt 50000
        front ++ List(-1) ++ back
    }
    val immutableDelete: Seq[_] => Unit = s => {
        val (front, back) = s splitAt 50000
        front ++ (back drop 1)
    }

    /* Linear sequences: List, ListBuffer */
    // Create
    val list = measure("List")(range.toList)().head.result
    val listBuffer = measure("ListBuffer")(ListBuffer.from(range))().head.result
    // Read
    _linearRead("List")(list.head)(list.last)(list.size)
    _linearRead("List buffer")(listBuffer.head)(listBuffer.last)(listBuffer.size)
    // Update
    _linearInsert("List")(0 +: list)(list :+ 100001)(immutableInsert(list))
    _linearInsert("List buffer")(listBuffer.prepend(0))(listBuffer.append(100001))(listBuffer.insert(50000, -1))
    // Delete
    _linearDelete("List")(list drop 1)(list take (list.size - 1))(immutableDelete(list))
    _linearDelete("List buffer")(listBuffer.remove(0))(listBuffer.remove(listBuffer.size - 1))(listBuffer.remove(50000))

    /* Indexed sequences: Vector, Array, ArrayBuffer */
    // Create
    val vector = measure("Vector")(range.toVector)().head.result
    val array = measure("Array")(range.toArray)().head.result
    val arrayBuffer = measure("Array buffer")(ArrayBuffer.from(range))().head.result
    // Read
    _linearRead("Vector")(vector.head)(vector.last)(vector.size)
    _linearRead("Array")(array.head)(array.last)(array.length)
    _linearRead("Array buffer")(arrayBuffer.head)(arrayBuffer.last)(arrayBuffer.length)
    // Update
    _linearInsert("Vector")(0 +: vector)(vector :+ 100001)(immutableInsert(vector))
    _linearInsert("Array")(0 +: array)(array :+ 100001)(immutableInsert(array))
    _linearInsert("Array buffer")(arrayBuffer.prepend(0))(arrayBuffer.append(100001))(arrayBuffer.insert(50000, -1))
    // Delete
    _linearDelete("Vector")(vector drop 1)(vector take (vector.size - 1))(immutableDelete(vector))
    _linearDelete("Array")(array drop 1)(array take (array.length - 1))(immutableDelete(array))
    _linearDelete("Array buffer")(arrayBuffer.remove(0))(arrayBuffer.remove(arrayBuffer.length - 1))(arrayBuffer.remove(50000))

    /* Sets */
    // Create
    val set = measure("Set")(range.toSet)().head.result
    val mutableSet = measure("Mutable set")(mutable.HashSet.from(range))().head.result
    // Read
    measure("Set read")(set contains 50000)()
    measure("Mutable set read")(mutableSet contains 50000)()
    // Update
    measure("Set add")(set + -1)(1)
    measure("Mutable set add")(mutableSet.add(-1))(1)
    // Delete
    measure("Set delete")(set - 5)(1)
    measure("Mutable set delete")(mutableSet.remove(-1))(1)

    /* Maps */
    // Create
    val map = measure("Map")(range.map(e => (e, e)).toMap)().head.result
    val mutableMap = measure("Mutable map")(mutable.HashMap.from(range.map(e => (e, e))))().head.result
    // Read
    measure("Map read")(map(50000))()
    measure("Mutable map read")(mutableMap(50000))()
    // Update
    measure("Map add")(map + (-1 -> -1))(1)
    measure("Mutable map add")(mutableMap.addOne(-1, -1))(1)
    measure("Map edit")(map + (50000 -> 50001))(1)
    measure("Mutable map edit")(mutableMap.update(50000, 50001))(1)
    // Delete
    measure("Map delete")(map - 50000)(1)
    measure("Mutable map delete")(mutableMap.remove(50000))(1)
}