package cryptoDashboard.services

import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.stage.StageStyle
import cryptoDashboard.services.Parser.ParserError
import sttp.client4.Response
import sttp.client4.quick.*

import java.time.*



object API:

  val apiKey = sys.env.getOrElse("API_KEY", "")


  def chart(id: String, interval: String): Array[(LocalDate, Double, Double, Double)]=
    val response: Response[String] = quickRequest
      .get(uri"https://api.coingecko.com/api/v3/coins/$id/market_chart?vs_currency=usd&days=365&interval=$interval&precision=full&x_cg_demo_api_key=$apiKey")
      .send()


    if response.code.toString != "200" then
      throw RequestError("chart", id)
    else
      try
        Parser.chartParser(response.body, id)
      catch
        case e => throw e




  def price(id: String): (Double, Double, Double, Double)  =
    val response: Response[String] = quickRequest
      .get(uri"https://api.coingecko.com/api/v3/simple/price?ids=$id&vs_currencies=usd&include_market_cap=true&include_24hr_vol=true&include_24hr_change=true&precision=full&x_cg_demo_api_key=$apiKey")
      .send()


    if response.code.toString != "200" then
      throw RequestError("price", id)
    else
      try
        Parser.priceParser(response.body, id)
      catch
        case e => throw e
      


  def marketCapPercentages: Array[(String, Double)] =
    val response: Response[String] = quickRequest
      .get(uri"https://api.coingecko.com/api/v3/global")
      .send()

    try
      if response.code.toString != "200" then
        throw RequestError("API didn't respond", "")
      else
        try
          Parser.marketCapPercentagesParser(response.body)
        catch
          case e => throw e
    catch
      case _ => new Alert(AlertType.Information) {
            title = "API Error"
            headerText = "Either you don't have internet connection or you have exceeded the api limits temporarily"
        }.showAndWait()
        throw new RuntimeException("Dashboard could not be opened, because error with API")



  case class RequestError(description: String, id: String)
            extends java.lang.Exception(description)









