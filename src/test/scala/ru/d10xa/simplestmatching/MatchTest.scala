package ru.d10xa.simplestmatching

import org.scalatest.FunSuiteLike
import org.scalatest.Matchers

class MatchTest extends FunSuiteLike with Matchers {

  import ru.d10xa.simplestmatching.Match._
  import ru.d10xa.simplestmatching.TsvReader._

  def genClient(i: Int): Client = Client(s"C$i", i.toLong, stocks = Map('A' -> i.toLong, 'B' -> i.toLong))

  test("line to client") {
    val line = "C1\t1000\t130\t240\t760\t320"
    val client = Client(
      "C1",
      1000,
      Map('A' -> 130L, 'B' -> 240L, 'C' -> 760L, 'D' -> 320L)
    )

    lineToClient(line) shouldEqual client
  }

  test("line to order") {
    val line = "C8\tb\tC\t15\t4"
    val order = Order("C8", Buy, 'C', 15L, 4L)

    lineToOrder(line) shouldEqual order
  }

  test("is same transaction") {
    val buy = Order("C1", Buy, 'A', 2L, 3L)
  }

  test("buy") {
    val c = Order("C1", Buy, 'A', 10L, 2L)
      .doOperation(Client("C1", 100L, Map('A' -> 10L)))
    c shouldEqual Client("C1", 80L, Map('A' -> 12L))
  }

  test("sell") {
    val c = Order("C1", Sell, 'A', 3L, 2L)
      .doOperation(Client("C1", 100L, Map('A' -> 10L)))
    c shouldEqual Client("C1", 106L, Map('A' -> 8L))
  }

  test("apply simple transaction") {
    val clients = Seq(
      Client("C1", 10L, Map('A' -> 10L)),
      Client("C2", 10L, Map('A' -> 10L))
    ).map(c => c.id -> c).toMap
    val orders = List(
      Order("C1", Buy, 'A', 1, 1),
      Order("C2", Sell, 'A', 1, 1)
    )
    val res = applyOrders(clients, orders, EmptyTransactions)
    res shouldEqual Seq(
      Client("C1", 9L, Map('A' -> 11L)),
      Client("C2", 11L, Map('A' -> 9L))
    ).map(c => c.id -> c).toMap
  }

  test("equal transactions") {
    val clients = Seq(
      Client("C1", 10L, Map('A' -> 10L)),
      Client("C2", 10L, Map('A' -> 10L))
    ).map(c => c.id -> c).toMap
    val orders = List(
      Order("C1", Buy, 'A', 1, 1),
      Order("C1", Buy, 'A', 1, 1),
      Order("C2", Sell, 'A', 1, 1),
      Order("C2", Sell, 'A', 1, 1)
    )
    val res = applyOrders(clients, orders, EmptyTransactions)
    res shouldEqual Seq(
      Client("C1", 8L, Map('A' -> 12L)),
      Client("C2", 12L, Map('A' -> 8L))
    ).map(c => c.id -> c).toMap
  }

  test("do not consider self transactions") {
    val clients = (1 to 5).map(genClient).map(c => c.id -> c).toMap
    val orders = List(
      Order("C1", Buy, 'A', 1, 2),
      Order("C1", Sell, 'A', 1, 2)
    )
    val res = applyOrders(clients, orders, EmptyTransactions)
    res shouldEqual clients
  }

  test("do not consider open transactions") {
    val clients = (1 to 5).map(genClient).map(c => c.id -> c).toMap
    val orders = List(
      Order("C1", Buy, 'A', 1, 2)
    )
    val res = applyOrders(clients, orders, EmptyTransactions)
    res shouldEqual clients
  }

  test("some transactions") {
    val clients = Seq(
      Client("C1", 100L, Map('A' -> 10L, 'B' -> 20L)),
      Client("C2", 100L, Map('A' -> 50L, 'B' -> 60L))
    ).map(c => c.id -> c).toMap
    val orders = List(
      Order("C1", Buy, 'A', 1, 2),
      Order("C1", Buy, 'B', 2, 3),
      Order("C2", Sell, 'B', 2, 3),
      Order("C2", Sell, 'A', 1, 2),
      // following not consider
      Order("C2", Sell, 'A', 1, 2), // not finished,
      Order("C2", Sell, 'A', 1, 1), // self
      Order("C2", Buy, 'A', 1, 1) // self
    )
    val res = applyOrders(clients, orders, EmptyTransactions)
    res shouldEqual Seq(
      Client("C1", 92L, Map('A' -> 12L, 'B' -> 23L)),
      Client("C2", 108L, Map('A' -> 48L, 'B' -> 57L))
    ).map(c => c.id -> c).toMap
  }

}
