package ru.d10xa.simplestmatching

import ru.d10xa.simplestmatching.Match.Client
import ru.d10xa.simplestmatching.Match.Order
import ru.d10xa.simplestmatching.Match.calculateResult

object Main {
  def main(args: Array[String]): Unit = {
    val clients: List[Client] = TsvReader.readClients()
    val orders: List[Order] = TsvReader.readOrders()
    calculateResult(clients, orders) foreach println
  }
}
