package cryptoDashboard.crypto

import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.XYChart

import java.time
import java.time.LocalDate


class Crypto(val id: String, val price: Double, val marketcap: Double, val volume24: Double, val change24: Double, val history: Array[(LocalDate, Double, Double, Double)]):



  def cryptoToSeries(interval: String, dataTo: String) =
    var seriesData = this.history
    interval match
      case "weekly" => seriesData = seriesData.filter((date, y1, y2, y3) => date.getDayOfWeek.getValue == 1)
      case "monthly" => seriesData = seriesData.filter((date, y1, y2, y3) => date.getDayOfMonth == 1)
      case _ =>

    val chartData = seriesData.map((date, y1, y2, y3) => dataTo match
      case "graph" => XYChart.Data[String, Number](date.toString, y1)
      case "bar" => XYChart.Data[String, Number](date.toString, y3)
    )

    val dataBuffer = ObservableBuffer.from(chartData.sortBy(_.getXValue))

    val dataSeries = XYChart.Series[String, Number](this.id, dataBuffer)

    dataSeries