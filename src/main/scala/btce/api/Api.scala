package btce.api

import java.util.Date
import javax.crypto.spec.SecretKeySpec
import scala.collection.mutable
import scala.util.Try
import javax.crypto.Mac
import org.apache.commons.codec.binary.Hex
import spray.http._
import spray.http.{ HttpEntity, ContentType }
import spray.client.pipelining._
import scala.concurrent._
import akka.spray._
import scala.util.{Success, Failure}
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.event.Logging
import akka.io.IO
import spray.can.Http
import spray.client.pipelining._
import spray.util._
import spray.http.HttpHeaders.RawHeader
import play.api.libs.json._
import play.api.libs.functional._
import spray.httpx.unmarshalling._
import spray.httpx.ResponseTransformation._
import scala.util.control.Exception.catching
import scala.util.control.NonFatal
import MediaTypes._
import HttpMethods._
import scala.util.Failure
import scala.concurrent.Promise
import trader.Trader



abstract class JsonApi(implicit val system:ActorSystem ) {
  
  	
	import system.dispatcher // execution context for futures    
			
	protected def encodePostParams( postParams: Map[String, String] = Map() ) = 
			postParams.map{ case (k,v) => k + "=" + v }.mkString("&")  	
	
	protected def request( apiUrl:String, method:HttpMethod = GET, postParams: Map[String, String] = Map(), headers: Map[String,String] = Map() ):Future[JsValue] = {

		val pipeline = sendReceive
	  
		def toPlayJson(entity: HttpEntity):JsValue = entity match {
			  case HttpEntity.NonEmpty(contentType, data) => Json.parse(data.toByteArray)
			  case _ => throw new Exception("empty_content")	  
		}	
		
		
		
	  
	  
		val postData = encodePostParams( postParams )
									
		
		val request = HttpRequest(
		    method = method, 
		    uri = apiUrl, 
		    entity = HttpEntity(`application/x-www-form-urlencoded` , postData.getBytes() ), 
		    headers = headers.map{ case (k,v) => RawHeader(k,v) }.toList
		)
 
		val response = pipeline(request)
		
		response.map { response => response match { 
		  case HttpResponse( StatusCodes.OK, entity, headers,_ ) => toPlayJson(entity)
		  case _ => throw new Exception("http_response_error")
		}}
				
  	}			
			
}

class PublicApi(implicit system:ActorSystem ) extends JsonApi()(system) {
	
  
	private val apiUrl = "https://btc-e.com/api/2/"
	  
	def fee(pair: Trader.Pair ) = {
	  request( apiUrl + pair.shortName() + "/fee" )
	}
	
	def ticker(pair: Trader.Pair) = {
		request( apiUrl + pair.shortName() + "/ticker" )
	}
	
	def trades(pair: Trader.Pair) = {
		request( apiUrl + pair.shortName() + "/trades" )
	}	
	
	def depth(pair: Trader.Pair) = {
		request( apiUrl + pair.shortName() + "/depth" )
	}
	

	
  	
	
}

class TradeApi(apiKey:String, apiSecret:String)(implicit system:ActorSystem ) extends JsonApi()(system) {
  
	import system.dispatcher // execution context for futures  
  
	private val apiUrl = "https://btc-e.com/tapi"
	
	private def nonce = (new Date().getTime/100 - 13859888422L).toString
	
	def info = signedRequest("getInfo")
	
	def transactionHistory( from:Int = 0, count:Int = 1000, fromId:Int = 0, endId:Int=0, order: SortingOrder.Value = SortingOrder.Asc, since:Date = null, end:Date = null ) = {
	  
	  val postParams = Map[String,Option[String]](
	      "from" -> Some(from.toString),
	      "count" -> Some(count.toString),
	      "from_id" -> ( if( fromId > 0 ){  Some(fromId.toString) } else None ),
	      "end_id" -> ( if( endId > 0 ){ Some(endId.toString) } else None ),
	      "order" -> Some( order.toString ),
	      "since" -> ( if( since != null ){ Some((since.getTime/1000).toString) } else None ),
	      "end" -> ( if( end != null ){ Some((end.getTime/1000).toString) } else None )
	  ).filter{ case (k,v) => !v.isEmpty }.map{ case (k,v) => k -> v.get }
	  
	  signedRequest("TransHistory", postParams)
	  
	}
	
	def tradeHistory( from:Int = 0, count:Int = 1000, fromId:Int = 0, endId:Int=0, order: SortingOrder.Value = SortingOrder.Asc, since:Date = null, end:Date = null, pair: Trader.Pair = null ) = {
	  
	  val postParams = Map(
	      "from" -> Some(from.toString),
	      "count" -> Some(count.toString),
	      "from_id" -> ( if( fromId > 0 ){  Some(fromId.toString) } else None ),
	      "end_id" -> ( if( endId > 0 ){ Some(endId.toString) } else None ),
	      "order" -> Some( order.toString ),
	      "since" -> ( if( since != null ){ Some((since.getTime/1000).toString) } else None ),
	      "end" -> ( if( end != null ){ Some((end.getTime/1000).toString) } else None ),
	      "pair" -> ( if( pair != null ){ Some(pair.shortName() ) } else None )
	  ).filter{ case (k,v) => !v.isEmpty }.map{ case (k,v) => k -> v.get }
	  
	  signedRequest("TradeHistory", postParams)	  
	}
	
	def getActiveOrders(pair: Trader.Pair = null ) = {
	  
	  val postParams = if(pair!=null) Map( "pair" -> pair.shortName() ) else Map[String,String]()

	   signedRequest("ActiveOrders", postParams )
	}
	
	def createOrder( pair:Trader.Pair, operation:Trader.TradeOperation.Value, rate:Double, amount:Double ) = {
	  val postParams = Map(
	      "pair" -> pair.shortName(),
	      "type" -> operation.toString,
	      "rate" -> rate.toString,
	      "amount" -> amount.toString
	  )
	  
	  signedRequest("Trade", postParams )
	}
	
	def cancelOrder(orderId:Long) = {
	  signedRequest("CancelOrder",Map("order_id" -> orderId.toString))
	}
	  
	private def signedRequest( method:String, arguments: Map[String, String] = Map() ):Future[JsValue] = {

	    val requestTry = Try{ 
					val key = new SecretKeySpec( apiSecret.getBytes( "UTF-8"), "HmacSHA512" )
					val mac = Mac.getInstance( "HmacSHA512" )
					mac.init(key)
					
					val postData = arguments ++ Map( "method" -> method, "nonce" -> nonce )
					
					val headers = Map(
					    "Key" -> apiKey,
					    "Sign" -> Hex.encodeHexString( mac.doFinal( encodePostParams(postData).getBytes( "UTF-8")))
					)
					( postData, headers )
		} 
				
		requestTry match {
		  case Success(( postData, headers )) => request(apiUrl, POST, postData, headers )
		  case Failure(t) => future { 
			  throw t 
		  }
		}
				
  	}  
  
}

object PublicApi {
  object Response {
    case class Fee(trade:Double)
    
    case class TradeInfo( date:Int,price:Double,amount:Double,tid:Int,price_currency:String,item:String,trade_type:String )
    type Trades = List[TradeInfo]
    
    implicit val pa0 = Json.reads[Fee]
  }
}

object TradeApi {
  
	implicit val btceTickerInfo = Json.reads[TradeApi.BtceTickerInfo]
	implicit val a0 = Json.reads[TradeApi.GetInfoFunds]
	implicit val a1 = Json.reads[TradeApi.GetInfoRights]
    implicit val a2 = Json.reads[TradeApi.GetInfo]
	implicit val a3 = Json.reads[TradeApi.BtceOrderInfo]
	implicit val a4 = Json.reads[TradeApi.BtceTradeInfo]
    
    case class GetInfoFunds( usd:Double, btc:Double, ltc:Double, nmc:Double, rur:Double, eur:Double, nvc:Double, trc:Double, ppc:Double, ftc:Double, xpm:Double )
    
    case class GetInfoRights(info:Int,trade:Int,withdraw:Int )
    
    case class GetInfo(
        funds:GetInfoFunds,
        rights:GetInfoRights,
        transaction_count:Int, 
        open_orders:Int,
        server_time:Int
    )    
    

   
/*	case class FundsInfo(usd: Double, rur: Double, eur: Double, btc: Double, ltc: Double, nmc: Double, nvc: Double, trc: Double, ppc: Double) {
	  override def toString = s"USD: $usd RUR: $rur EUR: $eur BTC: $btc LTC: $ltc NMC: $nmc NVC: $nvc TRC: $trc PPC: $ppc"
	}
	
	case class TransactionInfo(id: Long, transType: Int, amount: Double, currency: Trader.Currency, desc: String, status: Int, timestamp: DateTime, orderId: Option[Long])
	
	case class OrderInfo(orderId: Long, currency1: Trader.Currency, currency2:  Trader.Currency, orderType:  Trader.TradeOperation.Value,
	                     amount: Double, rate: Double, time: DateTime, status: Int)
	
	case class ArchivedTrade(tradeId: Long, currency1:  Trader.Currency, currency2:  Trader.Currency, tradeType:  Trader.TradeOperation.Value,
	                         amount: Double, rate: Double, order_id: Long, isYour: Boolean, time: DateTime)
	
	case class TradeCommandResult(received: Double, remains: Double, order_id: Long, funds: FundsInfo)
	
	case class RawTransactionInfo(`type`: Int, amount: Double, currency: String, desc: String, status: Int, timestamp: Long)
	
	case class RawOrderInfo(pair: String, `type`: String, amount: Double, rate: Double, timestamp_created: Long, status: Int)
	
	case class RawAccountTradeInfo(pair: String, `type`: String, amount: Double, rate: Double, order_id: Long, is_your_order: Short, timestamp: Long)
*/	
	case class BtceTickerInfo( high:Double,low:Double,avg:Double,vol:Double,vol_cur:Double,last:Double,buy:Double,sell:Double,updated:Int,server_time:Int )
    
	case class BtceOrderInfo( pair:String, `type`:String, amount:Double, rate:Double, timestamp_created:Int, status:Int )
	
	case class BtceTradeInfo( pair:String, order_id:Int,  `type`:String,  amount:Double, rate:Double, timestamp:Int, is_your_order:Int )
  
}

  class BtceOrderId(val id:Long) extends Trader.OrderId {
    override def label = id.toString
  }
  
  object BtceOrderId {
    def apply( orderId:String ) = new BtceOrderId(orderId.toLong)
    def apply( orderId:Long ) = new BtceOrderId(orderId)
  }

class BtceTradeApi(apiKey:String, apiSecret:String)(implicit system:ActorSystem ) extends Trader.Api {
  
  import system.dispatcher // execution context for futures      
  import TradeApi._
  
  protected var ordersCache = Map[Trader.OrderId, Trader.OrderInfo]()
  
  protected val tApi = new TradeApi(apiKey, apiSecret)(system)
  protected val pApi = new PublicApi()(system)


  
  implicit val orderReader = new Format[BtceOrderId] {
	  def reads(js: JsValue) = JsSuccess( new BtceOrderId( js.as[Int] ) )
	  def writes( o:BtceOrderId ) = JsNumber( o.id ) 
  }
  private val jsResultReader = ( JsPath \ "success").read[Int]
  private val jsOrderIdReader = ( JsPath \ "return" \ "order_id" ).read[BtceOrderId]
  private val feeReader = ( __ \ "trade" ).read[Double]
  
	def createOrder( pair:Trader.Pair, operation:Trader.TradeOperation.Value, rate:Double, amount:Double ):Future[Trader.OrderId] = {
    
	  tApi.createOrder(pair, operation, rate, amount).map { jsResult =>
	    jsResultReader.reads( jsResult ) match {
	      case JsSuccess( r:Int, _ ) if r > 0 => {
	      	jsOrderIdReader.reads( jsResult ) match {
	      	  case JsSuccess(orderId, _) => orderId
	      	  case _ => throw new IllegalArgumentException("API failure - order_id was not found")
	      	} 
	      }
	      case _ => throw new IllegalArgumentException("API failure - success == 0")
	    }


	  }
  	}
	
	def cancelOrder( orderId:Trader.OrderId ):Future[Boolean] = {
	  orderId match {
	    case oId:BtceOrderId =>
		  tApi.cancelOrder( oId.id ).map { jsResult =>
		    jsResultReader.reads(jsResult) match {
		      case JsSuccess( r:Int, _ ) if r > 0 => true
		      case _ => false
		    }
		  }
	    case _ => Future{ false }
	  }

	}
	
	def ticker( pair: Trader.Pair ): Future[Trader.TickerInfo] = {
	  import TradeApi._
	  
	  
	  val reader = ( __ \ "ticker" ).read[TradeApi.BtceTickerInfo]
	  
	  pApi.ticker(pair).map { jsResult =>
	    
	  	jsResult.validate( reader ).map { tickerInfo =>
	  		Trader.TickerInfo( pair, tickerInfo.buy, tickerInfo.sell, tickerInfo.vol )
	  	}.recoverTotal {
	  	  case e =>
	  	    throw new IllegalArgumentException("Ticker info format is wrong")
	  	}
	  }
	}
	
	protected def tradeOrderInfoFromBtceTradeInfo( b: BtceTradeInfo ):Trader.OrderInfo = {
	    	      val pair = Trader.Pair.fromString( b.pair ) 
    	    	  val operation = Trader.TradeOperation.withName( b.`type` )
    	    	  val orderId = BtceOrderId(b.order_id.toString)
    	    	  Trader.OrderInfo( orderId, pair.get, b.amount, operation, b.rate, b.is_your_order > 0 )	  
	}
	
	protected def searchInTradeHistory( orderId:Trader.OrderId ):Future[Trader.OrderInfo] = {
	    val tradeInfoReader = (__ \ "return").read( Reads.map[TradeApi.BtceTradeInfo] )
	    tApi.tradeHistory().map{ jsValue =>
//	      println(jsValue)
	    	tradeInfoReader.reads(jsValue) match {
	    	  case JsSuccess( tradeInfoMap, _ ) =>
	    	     
	    	    // cache results
	    	    
	    	    
	    	    val orders = tradeInfoMap.flatMap { case (id, op) => 
	    	        val pair = Trader.Pair.fromString( op.pair ) 
	    	        val operation = Trader.TradeOperation.withName( op.`type` )
	    	        if( op.is_your_order > 0 ){
	    	        	Some(orderId -> Trader.OrderInfo( orderId, pair.get, op.amount, operation, op.rate, op.is_your_order > 0 ))
	    	        } else {
	    	          None
	    	        }
	    	      }
	    	      
	    	    ordersCache.synchronized {
	    	    	ordersCache = ordersCache ++ orders
	    	    }
	    	    
	    	    val value = tradeInfoMap.find{ case (id, tradeOperationInfo) => tradeOperationInfo.order_id.toString == orderId.label }
	    	    
	    	    try{
	    	    	value.map{ case (id, b ) => tradeOrderInfoFromBtceTradeInfo(b) }.get
	    	    } catch {
	    	      case e => throw new IllegalArgumentException(s"API failure - order not found in trade history : ${orderId.toString}")
	    	    }

	    	    
	    	  case JsError(ex) => throw new IllegalArgumentException(s"API failure - order not found in trade history : ${orderId.toString}") 
	    	}
	    }
	  }

	
	
	protected def tradeOrderInfoFromBtceOrderInfo( orderId : String, o : TradeApi.BtceOrderInfo ):Trader.OrderInfo = o match {
	  
	  case TradeApi.BtceOrderInfo( xpair, xtype, xamount, xrate, timestamp_created, status ) =>
	  	  val pair = Trader.Pair.fromString( xpair ) 
	  	  val operation = Trader.TradeOperation.withName( xtype )
	  	  val newOrderId = BtceOrderId(orderId)
	  	  Trader.OrderInfo( newOrderId, pair.get, xamount, operation, xrate, status > 0 )	  
	} 
  
	def getOrder( orderId:Trader.OrderId ):Future[Trader.OrderInfo] = {
	  
	  ordersCache.get(orderId).map( Future.successful(_) ).getOrElse{
	  
		  val futureResult = tApi.getActiveOrders().map{ jsValue =>
		    	
		  		val ordersReader = ( __ \ "return" ).read( Reads.map[TradeApi.BtceOrderInfo] )
		  		
		  		ordersReader.reads(jsValue) match {
		    	  case JsSuccess( orderMap, _ ) =>
		    	    val orderOpt = orderMap.find{ case (ordrerId, orderData) => orderId.toString == orderId.label }
		    	    
	    	    	orderOpt match {
	    	    	  	case Some( (orderId, t ) ) => tradeOrderInfoFromBtceOrderInfo(orderId, t)
	    	    		case _ => throw new IllegalArgumentException(s"API failure - order not found ${orderId.toString}") 
	    	    	}
		    	    
		    	  case e => throw new IllegalArgumentException("API failure - getOrder failed ")
		    	}
		  }
		  
		  futureResult.recoverWith{ case ex => 
		    searchInTradeHistory(orderId) 
		  }
	  }
		  
	  
	  
	}
	
	// [0..1)
	def commissionRate( pair:Trader.Pair ):Future[Double] = {
	  pApi.fee(pair).map { jsValue =>
	    feeReader.reads(jsValue) match {
	      case JsSuccess( fee:Double, _ ) => fee
	      case _ => throw new IllegalArgumentException("API failure - ticker info not found")
	    }
	  }
	}
  
}

