package trader



import akka.actor.Actor
import scala.concurrent.duration.Duration

class TradeBot(
    pair:Trader.Pair, 
    startCurrency:Trader.Currency, // we are going to maximize the amount of this currency
    startAmount:Double,
    rateDelta:Double,
    fee:Double,
    orderConfirmationDuration:Duration) extends Actor {
  
  import context._
  
  /**
   * @param amount is current amount of pair._2
   * @param rate is last operation currency rate
   */
  def waitingToBuy( amount:Double, rate:Double ):Receive = {
    case t@Trader.TickerInfo( p, buy,sell, _ ) if p == pair =>
//      println("ticker", buy)
      if( ( rate < 0 ) || ( buy < rate*(1 - ( rateDelta+fee )) ) ){
        // place an order
        parent ! TradeBot.PlaceOrder(pair, amount, buy, Trader.TradeOperation.Buy, orderConfirmationDuration )
        become( placingOrder( amount, rate, Trader.TradeOperation.Buy ) )
      }
  }

  /**
   * @param amount is current amount of pair._1 currency
   * @param rate is last operation currency rate
   */  
  def waitingToSell( amount:Double, rate:Double ):Receive = {
    case t@Trader.TickerInfo( p, buy,sell, _ ) if p == pair =>
//      println("ticker", sell)
      if( ( rate < 0 ) || ( sell > rate*(1 + ( rateDelta+fee )) ) ){
        // place an order
        parent ! TradeBot.PlaceOrder(pair, amount, sell, Trader.TradeOperation.Sell, orderConfirmationDuration )
        become( placingOrder( amount, rate, Trader.TradeOperation.Sell ) )
      }
  }
  
  def placingOrder(  amount:Double, rate:Double,operation:Trader.TradeOperation.Value ):Receive = {
    case TradeBot.OrderPlaced(orderId, amount, rate) => 
      become(waitingForOrderToConfirm(orderId, amount, rate, operation))
  }
  
  
  // TODO implement a better order identity check
  def waitingForOrderToConfirm(orderId:Trader.OrderId, amount:Double, rate:Double, operation:Trader.TradeOperation.Value):Receive = {
    case TradeBot.OrderConfirmed(xOrderId) if xOrderId.label == orderId.label => 
      
      if( operation == Trader.TradeOperation.Sell ){
        val newAmount = (amount*rate)*(1 - fee)
        println(s"Sell Order succeeded: $newAmount ${pair._2.label}, rate: $rate")
        become(waitingToBuy( newAmount, rate))
      } else {
        val newAmount = (amount/rate)*(1-fee)
        println(s"Buy Order succeeded: $newAmount ${pair._1.label}, rate: $rate")
        become(waitingToSell(newAmount, rate))
      }
    case TradeBot.OrderConfirmationTimeout(xOrderId) if xOrderId.label == orderId.label =>
      parent ! TradeBot.CancelOrder(orderId)
      // timeout hit... cancel an order and let's wait a bit more
      if( operation == Trader.TradeOperation.Sell ){
        become(waitingToSell(amount, rate))
      } else {
        become(waitingToBuy( amount, rate))
      }      
  }
  
  def receive = 
    if( pair._1 == startCurrency )
    	waitingToSell( startAmount, -1)
    else
    	waitingToBuy( startAmount, -1)

}

object TradeBot {
  case class PlaceOrder( pair:Trader.Pair, amount:Double, rate:Double, operation:Trader.TradeOperation.Value, confirmationDuration:Duration)
  case class OrderPlaced( orderId:Trader.OrderId, amount:Double, rate:Double)
  case class OrderConfirmed( orderId:Trader.OrderId )
  case class OrderConfirmationTimeout( orderId:Trader.OrderId)
  case class CancelOrder( orderId:Trader.OrderId)
}