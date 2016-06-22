import scala.util.{Success, Failure}
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.event.Logging
import akka.io.IO
import spray.can.Http
import spray.httpx.SprayJsonSupport
import spray.client.pipelining._
import spray.util._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import java.util.Date
import spray.can.Http
import spray.util._
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import btce.api.{ TradeApi, BtceTradeApi}
import btce.api.BtceOrderId
import trader.Trader
import com.github.tototoshi.csv._
import java.io.File
import scala.io.Source
import akka.actor.Props
import trader.TraderMaster
import trader.TradeBot
import btce.api.Currency


object Main {
	
	implicit val system = ActorSystem()
  
	val conf = ConfigFactory.load("application")
	
	def main(args: Array[String]):Unit = {
/*	  	  val api = new  TradeApi( conf.getString("btce.api.key"), conf.getString("btce.api.secret") )
	  	  val r = api.tradeHistory( pair = Trader.Pair.fromString("btc_usd").get )
//	  	  val r = api.getActiveOrders()
	  	  r.onComplete( println(_) )*/
	  
	  	  
	  	  
	  val bapi = new BtceTradeApi(conf.getString("btce.api.key"), conf.getString("btce.api.secret"))
	  val orderInfo = bapi.getOrder( BtceOrderId(97161862))
	  orderInfo.onSuccess{ case o => println(o)}
	  orderInfo.onFailure{ case e => println(e) }
	}
  
	def main2(args: Array[String]) = {
	  
//	  println( conf )
/*	  
	  val api = new  BtceTradeApi( conf.getString("btce.api.key"), conf.getString("btce.api.secret") )

	  val ticker = api.ticker(Trader.Pair.fromString("btc_usd").get)

	  ticker.onComplete{ case t => println(t) }*/
	  
	  val testCsvFile = "/home/aloise/work/desktop/e-btc-bot/data/btceUSD.csv"
	    
	  val lines = Source.fromFile( new File(testCsvFile)).getLines
	  val items = lines.flatMap{ line => 
	    val a = line.split(',').map( _.trim )
	    if(a.length == 3){
	      Some( a(0).toInt, a(1).toDouble, a(2).toDouble )
	    } else {
	      None
	    }
	  }
	  
//	  items.foreach( t => println(t))
	  
	  val tradeMaster = system.actorOf( Props( classOf[TraderMaster], null, 60.seconds ) )
	  val pair = Trader.Pair.fromString("btc_usd").get
	  tradeMaster ! TraderMaster.AddBot( pair, Props( classOf[TradeBot], pair, new Trader.Currency("usd"), 1.0, 0.01, 0.002, 1.hour ) )
	  
	  var i = 1000000
	  val drop = i
	  items.drop(drop).foreach{ t => 
//	    println( i, t._2)
	    i += 1
	    tradeMaster ! TraderMaster.Ticker( Trader.TickerInfo( pair, t._2 , t._2, t._3 ) )
	    
	  }
	  
	}

	
	def shutdown(): Unit = {
			IO(Http).ask(Http.CloseAll)(1.second).await
			system.shutdown
    
  }	
	
}



