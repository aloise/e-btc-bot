package trader

import scala.concurrent.Future

object Trader { 

	trait OrderId extends Any {
	  def label:String
	}
  
	case class Currency(label:String) {
	  
	  
	  
	}	
	
	
	class Pair(a:Currency,b:Currency) extends Tuple2[Currency ,Currency ](a,b) {
	    def shortName( delimiter:String = "_" ) = {
	      a.label.toLowerCase + delimiter + b.label.toLowerCase
	    }
	    
	    override def toString = 
	      "Pair"+super.toString
	}
	
	object Pair {
	    def fromString( pair:String, delimiter:String = "_" ):Option[Pair] = {
	      val pairs = pair.split(delimiter, 2)
	      if(pairs.length == 2){
	    	  Some( new Pair( Currency(pairs(0)), Currency(pairs(1)) ) )
	      } else {
	        None
	      }
	      
	    }	
	    
/*	    def apply(label:String ) = {
	      new Currency(label.toUpperCase)
	    }*/
	}
	
	object TradeOperation extends Enumeration {
	  type Operation = Value
	  val Sell = Value("sell")
	  val Buy = Value("buy")
	}  
	

	case class OrderInfo( id:OrderId, pair:Pair, amount:Double, operation:TradeOperation.Value, rate: Double, isCompleted:Boolean = false)
  	
  	case class TickerInfo( pair:Pair, buy:Double, sell:Double, volume:Double )
	
	
  	
	trait Api {
	  
		def createOrder( pair:Trader.Pair, operation:Trader.TradeOperation.Value, rate:Double, amount:Double ):Future[OrderId]
		
		def cancelOrder( orderId:OrderId ):Future[Boolean]
		
		def getOrder( orderId:OrderId ):Future[OrderInfo]
		
		def ticker( pair: Trader.Pair ):Future[TickerInfo]
		
		// [0..1)
		def commissionRate( pair:Trader.Pair ):Future[Double]
	  
	}
	

	

}

