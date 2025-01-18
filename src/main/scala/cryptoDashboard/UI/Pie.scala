package cryptoDashboard.UI

import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.PieChart
import scalafx.scene.control.Tooltip
import cryptoDashboard.services.API
import cryptoDashboard.app.Dashboard

class Pie:

  val marketData = ObservableBuffer.from(Dashboard.marketData.map((a,b) => PieChart.Data(a,b)))
  val othersPercentage = 100 - Dashboard.marketData.map((a,b) => b).sum
  
  marketData.add(PieChart.Data("Others", othersPercentage))


  val ui = new PieChart {
    title = "Market Cap percentages of the whole market"
    data = marketData
  }

  marketData.foreach(i =>
    val tooltip = new Tooltip(s"${i.getName} : ${i.getPieValue.toString.take(5)} %")
    Tooltip.install(i.getNode, tooltip)
  )



