import collection.mutable.Stack
import org.scalatest._

import scala.collection.immutable.TreeMap
import scala.collection.mutable

/**
  * Created by jcohen66 on 9/16/16.
  */


class PNLSpec extends FlatSpec with Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be(2)
    stack.pop() should be(1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a[NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }

  "PositionService" should "calculate net position" in {

    val f_t0 = Fill("F", 0, "MSFT", 42.43, 300, 'B')
    val f_t1 = Fill("F", 1, "MSFT", 42.43, 300, 'B')
    val f_t2 = Fill("F", 1, "MSFT", 42.44, 300, 'B')
    val f_t3 = Fill("F", 1, "MSFT", 42.44, 100, 'S')

    PositionService.transact(f_t0)
    PositionService.transact(f_t1)
    PositionService.transact(f_t2)
    PositionService.transact(f_t3)

    PositionService.position(f_t0.symbol).position should be(800)

  }

  "PNLService" should "calculate pnl" in {

    val f_t0 = Fill("F", 0, "MSFT", 42.43, 300, 'B')
    val f_t1 = Fill("F", 1, "MSFT", 42.43, 300, 'B')
    val f_t2 = Fill("F", 2, "MSFT", 42.44, 300, 'B')
    val f_t3 = Fill("F", 3, "MSFT", 42.44, 100, 'S')

    PositionService.transact(f_t0)
    PositionService.transact(f_t1)
    PositionService.transact(f_t2)
    PositionService.transact(f_t3)

    val symbol = "MSFT"

    PNLService.transact(f_t0)
    PNLService.transact(f_t1)
    PNLService.transact(f_t2)
    PNLService.transact(f_t3)


    val m2m = 42.50
    val pos = PositionService.position(symbol)

    println()
    println(PNLService.entries(symbol))
    println("PnL is: " + ((pos.position * m2m) - PNLService.entries(symbol).foldLeft(0.0)(_ + _.cash)) )
  }

  "A nested map" should "lookup stuff" in {

    val p1 = Position("MSFT", 0, 300)
    val p2 = Position("MSFT", 1, 200)
    val p3 = Position("MSFT", 3, 100)

    val s = mutable.SortedSet(p1, p2, p3)

    val p4 = Position("MSFT", 5, -100)

    s.add(p4)

    println(s.to(Position("MSFT", 4, 300)).last)



  }

  "A PNLServer" should "calculate line items" in {

    val f_t0 = Fill("F", 0, "MSFT", 42.43, 300, 'B')
    val f_t1 = Fill("F", 1, "MSFT", 42.43, 300, 'B')
    val f_t2 = Fill("F", 2, "MSFT", 42.44, 300, 'B')
    val f_t3 = Fill("F", 3, "MSFT", 42.44, 100, 'S')

    PositionService.transact(f_t0)
    PositionService.transact(f_t1)
    PositionService.transact(f_t2)
    PositionService.transact(f_t3)

    val symbol = "MSFT"

    PNLService.transact2(f_t0)
    PNLService.transact2(f_t1)
    PNLService.transact2(f_t2)
    PNLService.transact2(f_t3)

    val milliseconds = 0
    val m2m = 42.50d
    val posm = PositionService.position(symbol, milliseconds)
    val pnlp = PNLService.pnl(symbol, milliseconds)
    println()

    println(posm)
    println(pnlp)

    println(pnlp.netCash)
    // println(posm * m2m)


    println(PNLService.entries(symbol))
    // println("PnL is: " + (posm * m2m - pnlp.netCash))

  }

  "A timestamp" should "convert to a joda time" in {

    println(new org.joda.time.DateTime(1388534400000L))
      println(new org.joda.time.DateTime(1388534400000L))
      println(new org.joda.time.DateTime(1388538000000L))
      println(new org.joda.time.DateTime(1388538000000L))
      println(new org.joda.time.DateTime(1388541600000L))
      println(new org.joda.time.DateTime(1388541600000L))
      println(new org.joda.time.DateTime(1388545200000L))
      println(new org.joda.time.DateTime(1388545200000L))
      println(new org.joda.time.DateTime(1388548800000L))
      println(new org.joda.time.DateTime(1388548800000L))
      println(new org.joda.time.DateTime(1388552400000L))
      println(new org.joda.time.DateTime(1388552400000L))
      println(new org.joda.time.DateTime(1388556000000L))
      println(new org.joda.time.DateTime(1388556000000L))
      println(new org.joda.time.DateTime(1388559600000L))
      println(new org.joda.time.DateTime(1388559600000L))
  }

  "A SortedSet" should "return the last position" in {

    val symbol = "MSFT"
    val p1 = Position(symbol, 0, 100)
    val p2 = Position(symbol, 4, 200)

    val positions = mutable.Map.empty[String, mutable.SortedSet[Position]]
    // positions.put(symbol, mutable.SortedSet(p1))
    positions.put(symbol, mutable.SortedSet(p1))


    val ss = positions(symbol)
    ss.add(p2)
    println(ss)

    val last = ss.last
    val size = 100
    val p3 = Position(symbol, 6, last.position + size)
    ss.add(p3)

    println(positions)

    println(ss.to(Position("",5,0)).last)
  }
}
