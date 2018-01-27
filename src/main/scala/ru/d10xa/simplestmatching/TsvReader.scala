package ru.d10xa.simplestmatching

import ru.d10xa.simplestmatching.Match.Buy
import ru.d10xa.simplestmatching.Match.Client
import ru.d10xa.simplestmatching.Match.Operation
import ru.d10xa.simplestmatching.Match.Order
import ru.d10xa.simplestmatching.Match.Sell
import ru.d10xa.simplestmatching.Match.Stock

import scala.io.Source

object TsvReader {

  private def parse[T](lines: Iterator[String], f: String => T): List[T] = lines.toList.map(f)

  def lineToClient(line: String): Client = {
    def mkBalance(stocks: List[Long]): Map[Stock, Long] = ("ABCD" zip stocks).toMap
    line.split("\t").toList match {
      case clientId :: balance :: stocks =>
        Client(clientId, balance.toLong, mkBalance(stocks.map(_.toLong)))
    }
  }

  def lineToOrder(line: String): Order = {
    def mkOperation(str: String): Operation = str match {
      case "s" => Sell
      case "b" => Buy
      case _ => throw new UnsupportedOperationException
    }
    line.split("\t").toList match {
      case clientId :: op :: stock :: price :: quantity :: Nil =>
        Order(clientId, mkOperation(op), stock.head, price.toLong, quantity.toLong)
    }
  }

  def readClients(): List[Client] = parse(Source.fromResource("clients.txt").getLines(), lineToClient)

  def readOrders(): List[Order] = parse(Source.fromResource("orders.txt").getLines(), lineToOrder)

}
