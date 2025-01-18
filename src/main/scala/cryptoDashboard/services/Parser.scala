package cryptoDashboard.services

import java.time.{Instant, LocalDate, ZoneOffset}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object Parser:

  def chartParser(json: String, id: String): Array[(LocalDate, Double, Double, Double)] =
    val data: ujson.Value = ujson.read(json)
    val dataObj: mutable.Map[String, ujson.Value] = data.obj


    val datesEpoch: Array[Long] = dataObj("prices").arr.map(_(0).num.toLong).toArray

    var dates: Array[LocalDate] = datesEpoch.map(epochToDate(_))
    if dates.length == 366 then dates = dates.dropRight(1)


    for (i <- 1 until dates.length) do
      if (dates(i) != dates(i - 1).plusDays(1)) throw ParserError("Crypto's data is missing days", id)


    val prices: Array[Double] = dataObj("prices").arr.map(_(1).num).toArray
    val marketCap: Array[Double] = dataObj("market_caps").arr.map(_(1).num).toArray
    val volume: Array[Double] = dataObj("total_volumes").arr.map(_(1).num).toArray


    if prices.contains(0.0) then
      throw ParserError("Crypto's chart data is likely faulty", id)

    if marketCap.contains(0.0) then
      throw ParserError("Crypto's marketcap data is likely faulty", id)

    if volume.contains(0.0) then
      throw ParserError("Crypto's volume data is likely faulty", id)


    val r =  dates.zip(prices).zip(marketCap).map((x,y) => (x._1, x._2, y)).zip(volume).map((x,y) => (x._1, x._2, x._3, y))

    if (r.length != 365) throw ParserError("Crypto doesn't have enough chart data", id)
    else r


  
  def priceParser(json: String, id: String): (Double, Double, Double, Double) =
    val data: ujson.Value = ujson.read(json)
    val dataObj: mutable.Map[String, ujson.Value] = data.obj

    val price: Double = dataObj(id)("usd").num
    val marketCap: Double = dataObj(id)("usd_market_cap").num
    val volume: Double = dataObj(id)("usd_24h_vol").num
    val change: Double = dataObj(id)("usd_24h_change").num

    val r = (price, marketCap, volume, change)
    r


  
  def marketCapPercentagesParser(json: String): Array[(String, Double)] =

    val data: ujson.Value = ujson.read(json)
    val dataObj: mutable.Map[String, ujson.Value] = data.obj

    val r = dataObj("data")("market_cap_percentage").obj.toArray.map((a,b) => (a, b.num))
    if r.isEmpty then throw ParserError("Fetched general market data was empty", "")
    else r


  def epochToDate(epoch: Long): LocalDate =
    Instant.ofEpochMilli(epoch).atZone(ZoneOffset.UTC).toLocalDate


  case class ParserError(description: String, id: String)
          extends java.lang.Exception(description)
