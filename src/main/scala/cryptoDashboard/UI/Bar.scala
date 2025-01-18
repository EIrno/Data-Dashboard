package cryptoDashboard.UI


import javafx.event.{ActionEvent, EventHandler}
import scalafx.Includes.{jfxMouseEvent2sfx, observableList2ObservableBuffer}
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer.Remove
import scalafx.geometry.Orientation
import scalafx.geometry.Orientation.{Horizontal, Vertical}
import scalafx.scene.Cursor
import scalafx.scene.chart.*
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.*
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.layout.BorderPane
import scalafx.scene.paint.Color
import scalafx.stage.StageStyle
import cryptoDashboard.services.API

import java.time.LocalDate
import java.time.temporal.TemporalAmount
import cryptoDashboard.crypto.Crypto
import cryptoDashboard.app.Dashboard
import cryptoDashboard.services.utils.Formatter

import java.text.DecimalFormat



class Bar:

  var dataSeries: ObservableBuffer[javafx.scene.chart.XYChart.Series[String, Number]] = ObservableBuffer()
  var colors = scala.collection.mutable.Map[String,String]()
  var interval: String = "daily"


  val xAxis = new CategoryAxis() {
    animated = false
  }
  val yAxis = new NumberAxis()



  var barChart = new BarChart(xAxis, yAxis) {
    title = "Volume"
    data = dataSeries
    legendVisible = false
    animated = false
  }


  def addSeries(id: String, seriesColor: String = "00FF00"): Unit =

    val newSeries = Dashboard.watchlist(id).cryptoToSeries(interval, "bar")
    barChart.getData.add(newSeries)

    if !colors.contains(id) then
      colors.addOne((id, seriesColor))

    barChart.title = "Volume: " + dataSeries.map(_.getName).mkString(", ")
    color(id, colors(id))
    tooltips(newSeries)


  def deleteSeries(id: String) =
    barChart.getData.removeIf(_.getName == id)
    colors.remove(id)
    barChart.title = "Volume: " + dataSeries.map(_.getName).mkString(", ")



  def changeInterval(newInterval: String) =

    interval = newInterval

    val newSeriesBuffer = barChart.getData.map(_.getName).map((i) => Dashboard.watchlist(i).cryptoToSeries(interval, "bar"))

    barChart.getData.clear()

    for series <- newSeriesBuffer do
      barChart.getData.addOne(series)

      if colors.keys.exists(_ == series.getName) then color(series.getName, colors(series.getName))
      else color(series.getName)
      tooltips(series)




  def color(id: String, color: String = "00FF00") =

    colors.update(id, color)

    val index = barChart.data.value.indexWhere(_.getName ==  id)

    for point <- barChart.data.value(index).getData do
        point.getNode.setStyle(s"-fx-background-color: #$color, #$color;\n" +
          s"-fx-stroke: #$color;\n" +
          s"-fx-fill: #$color;"
        )

    cryptoMenu.items.find(_.getText == id) match
      case Some(menu) => menu.setStyle(s"-fx-background-color: #$color;")
      case _ =>


  def tooltips(series: javafx.scene.chart.XYChart.Series[String, Number]) =
    for point <- series.getData do
      val tooltip = new Tooltip(s"${point.getXValue}, ${Formatter.formatDollarNumber(point.getYValue.doubleValue)}")
      Tooltip.install(point.getNode, tooltip)

      point.getNode.setOnMousePressed(alertEvent)


      def alertEvent(event : MouseEvent): Unit =
          if (event.button == MouseButton.Secondary) then
            val alert = new Alert(AlertType.Information) {
              title = "Color Picker"
              headerText = "Pick a color for the series"
              graphic = new ColorPicker(Color.Blue){
                onAction = (event : ActionEvent) => {
                  color(series.getName, value.value.toString.drop(2).dropRight(2))
                }
              }
            }
            alert.showAndWait()
          else
            val dayInfoOption = Dashboard.watchlist(series.getName).history.find(_._1.toString == point.getXValue)
            val dayInfo = dayInfoOption.getOrElse(LocalDate.now(), 0.0, 0.0, 0.0)


            val alert = new Alert(AlertType.Information) {
              title = series.getName
              headerText = s"${point.getXValue}\n" +
                s"Price: ${Formatter.formatDollarNumber(point.getYValue.doubleValue)}\n" +
                s"Marketcap: ${Formatter.formatDollarNumber(dayInfo._3)} \n" +
                s"Volume: ${Formatter.formatDollarNumber(dayInfo._4)}\n"
            }
            alert.showAndWait()



  val cryptoMenu = new MenuButton("select crypto(s)") {
    items = Dashboard.watchlistID.map((i) => createMenuItem(i)).toSeq
  }

  Dashboard.watchlistID.onChange((buffer, changes) =>
    cryptoMenu.items = Dashboard.watchlistID.map(i => createMenuItem(i))
    for change <-changes do
      change match
        case Remove(pos, removed) => deleteSeries(removed.head)
        case _ =>
  )

  dataSeries.onChange((buffer, changes) =>
    cryptoMenu.items = Dashboard.watchlistID.map(i => createMenuItem(i))
  )


  def createMenuItem(newItem: String) =
    new CheckMenuItem(newItem) {
        selected = dataSeries.find(_.getName == newItem) match
          case Some(found) => true
          case _ => false

        if selected.value && colors.contains(newItem) then
          style = (s"-fx-background-color: #${colors(newItem)};")

        selected.onChange((_, oldValue, newValue) =>
          if newValue then
            addSeries(newItem)
          else
            deleteSeries(newItem)
        )
      }



  private val contentToolBar = new ToolBar {
      orientation = Orientation.Horizontal
      items = Seq(
        cryptoMenu,
      )
    }

  val ui: BorderPane = new BorderPane {
      prefWidth = 1200
      prefHeight = 800
      center = barChart
      bottom = contentToolBar
    }
