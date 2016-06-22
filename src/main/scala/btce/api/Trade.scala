package btce.api

import org.joda.time.DateTime
import trader.Trader

object SortingOrder extends Enumeration {
  type SortingOrder = Value
  val Asc = Value("ASC")
  val Desc = Value("DESC")
}

object Currency {
  
  val USD = Trader.Currency("USD")
  val RUR = Trader.Currency("RUR")
  val EUR = Trader.Currency("EUR")
  val BTC = Trader.Currency("BTC")
  val LTC = Trader.Currency("LTC")
  val NMC = Trader.Currency("NMC")
  val NVC = Trader.Currency("NVC")
  val TRC = Trader.Currency("TRC")
  val PPC = Trader.Currency("PPC")
  
}




