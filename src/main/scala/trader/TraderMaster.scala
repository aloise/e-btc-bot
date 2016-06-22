package trader

import akka.actor._
import scala.concurrent.duration._
import scala.util.Random



object TraderMaster {
  case class AddBot( pair:Trader.Pair, botProps: Props )
  case class StopBot( pair:Trader.Pair, bot:ActorRef )
  case class Ticker(info:Trader.TickerInfo)
  
}

class TestOrderId(id:String) extends Trader.OrderId {
  def label = id
}

class TraderMaster(tradeApi:Trader.Api, tickerDuration:Duration = 60.seconds ) extends Actor {
	
  
  // schedule ticker updates
  
  def onReceive(
      
      bots:List[(Trader.Pair, ActorRef)] = List()
      
  ):Receive = {
    
    case TraderMaster.AddBot( p, botProps ) =>
      println("Add Bot")
      val bot = context.actorOf( botProps)
      context.become( onReceive( (p,bot) :: bots )  )

    case TraderMaster.StopBot( pair, bot ) =>
      bot ! PoisonPill
      context.become( onReceive( bots.filter( _ != ( pair, bot )  ) ) )
    
    // trade bot events
    case TradeBot.PlaceOrder( pair, amount, rate, operation, confirmationDuration) =>
      // confirm immediately
      val testOrderId = new TestOrderId("ORDER_"+Random.nextInt.toString)
      sender ! TradeBot.OrderPlaced( testOrderId, amount, rate )
      sender ! TradeBot.OrderConfirmed( testOrderId )
      
//  	case OrderPlaced( orderId:Trader.OrderId, amount:Double, rate:Double) =>
//  	case class OrderConfirmed( orderId:Trader.OrderId )
//  	case class OrderConfirmationTimeout( orderId:Trader.OrderId)
  	case TradeBot.CancelOrder( orderId ) =>
  	  // ok, let's assume it's cancelled
  	  
  	// api events
  	  
    case ticker@TraderMaster.Ticker(info) => {
      
    	bots.
    		filter{ case (p, b) => p == info.pair }.
    		foreach{ _._2 ! info }
    }
     
      
  }
  
  def receive = onReceive(List())
  
}

