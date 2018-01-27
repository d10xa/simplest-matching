package ru.d10xa.simplestmatching

import scala.annotation.tailrec

object Match {
  type Stock = Char
  type ClientId = String
  final case class Client(id: ClientId, balance: Long, stocks: Map[Stock, Long]) {
    private def stocksAsString: String = stocks
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString("\t")
    override def toString: ClientId = s"$id\t$balance\t$stocksAsString"
  }
  sealed trait Operation {
    def balanceOp: (Long, Long) => Long
    def stockOp: (Long, Long) => Long
  }
  final case object Sell extends Operation {
    override def balanceOp: (Long, Long) => Long = _ + _
    override def stockOp: (Long, Long) => Long = _ - _
  }
  final case object Buy extends Operation {
    override def balanceOp: (Long, Long) => Long = _ - _
    override def stockOp: (Long, Long) => Long = _ + _
  }
  final case class Order(clientId: ClientId, operation: Operation, stock: Stock, price: Long, quantity: Long) {
    def key: OrderKey = OrderKey(stock, price, quantity)
    def doOperation(client: Client): Client = {
      require(client.id == clientId)
      client.copy(
        balance = operation.balanceOp(client.balance, quantity * price),
        stocks = client.stocks + (stock -> operation.stockOp(client.stocks(stock), quantity))
      )
    }
  }
  final case class OrderKey(stock: Stock, price: Long, quantity: Long)
  trait Transactions {
    def apply(order: Order): Option[Order]
    def +(order: Order): Transactions
    def -(order: Order): Transactions
  }
  object EmptyTransactions extends Transactions {
    override def +(order: Order): Transactions = new NonEmptyTransactions(List(order))
    override def -(order: Order): Transactions = throw new IllegalStateException("call minus on empty")
    override def apply(order: Order): Option[Order] = None
  }
  final class NonEmptyTransactions(orders: List[Order]) extends Transactions {
    override def +(order: Order): Transactions = new NonEmptyTransactions(orders :+ order)
    override def -(order: Order): Transactions =
      new NonEmptyTransactions(orders.patch(orders.indexWhere(order.key == _.key), Nil, 1))
    override def apply(order: Order): Option[Order] = orders.find(_.key == order.key)
  }

  implicit class ClientMapImplicits(clients: Map[ClientId, Client]) {
    def update(client: Client): Map[ClientId, Client] =
      clients + (client.id -> client)
    def executeTransaction(a: Order, b: Order): Map[ClientId, Client] = {
      val x = a.doOperation(clients(a.clientId))
      val y = b.doOperation(clients(b.clientId))
      update(x).update(y)
    }
  }

  @tailrec
  def applyOrders(
    clients: Map[ClientId, Client],
    orders: List[Order],
    openTransactions: Transactions
  ): Map[ClientId, Client] = {
    orders match {
      case Nil =>
        clients
      case x :: xs =>
        val (a, b, c) = openTransactions(x) match {
          case None =>
            (clients, xs, openTransactions + x)
          case Some(open) if open.clientId == x.clientId && open.operation != x.operation =>
            (clients, xs, openTransactions - x)
          case Some(open) if open.operation == x.operation =>
            (clients, xs, openTransactions + x)
          case Some(open) =>
            (clients.executeTransaction(x, open), xs, openTransactions - x)
        }
        applyOrders(a, b, c)
    }
  }

  def calculateResult(clients: List[Client], orders: List[Order]): List[Client] =
    applyOrders(clients.map(cli => cli.id -> cli).toMap, orders, EmptyTransactions)
      .values.toList.sortBy(_.id)

}
