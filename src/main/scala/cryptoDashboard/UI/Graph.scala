package cryptoDashboard.UI

import javafx.event.{ActionEvent, EventHandler}
import scalafx.Includes.{jfxMouseEvent2sfx, observableList2ObservableBuffer}
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer.{Add, Remove}
import scalafx.geometry.Orientation
import scalafx.geometry.Orientation.{Horizontal, Vertical}
import scalafx.scene.Cursor
import scalafx.scene.Node.sfxNode2jfx
import scalafx.scene.chart.{CategoryAxis, LineChart, NumberAxis, XYChart}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.*
import scalafx.scene.input.{DragEvent, MouseButton, MouseDragEvent, MouseEvent}
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.text.Text
import scalafx.stage.StageStyle


import cryptoDashboard.services.API
import cryptoDashboard.services.utils.Formatter




import java.time.LocalDate
import scala.collection.mutable.ArrayBuffer
import cryptoDashboard.app.Dashboard
import cryptoDashboard.crypto.Crypto
import scalafx.scene.shape.Rectangle

import java.text.DecimalFormat




class Graph:

  var dataSeries: ObservableBuffer[javafx.scene.chart.XYChart.Series[String, Number]] = ObservableBuffer()
  var colors = scala.collection.mutable.Map[String,String]()
  var interval: String = "daily"



  val xAxis = new CategoryAxis() {
    animated = false
  }
  val yAxis = new NumberAxis() {
    animated = false
  }


  var lineChart = new LineChart(xAxis, yAxis) {
    title = "Price"
    data = dataSeries
    createSymbols = true
    legendVisible = false
    animated = false
  }


  def addSeries(id: String, seriesColor: String = "00FF00"): Unit =
    val newSeries = Dashboard.watchlist(id).cryptoToSeries(interval, "graph")

    lineChart.getData.add(newSeries)

    if !colors.contains(id) then
      colors.addOne((id, seriesColor))

    lineChart.title = "Price: " + dataSeries.map(_.getName).mkString(", ")
    color(id, colors(id))
    tooltips(newSeries)



  def deleteSeries(id: String) =
    lineChart.getData.removeIf(_.getName == id)
    colors.remove(id)
    lineChart.title = "Price: " + dataSeries.map(_.getName).mkString(", ")




  def changeInterval(newInterval: String) =

    interval = newInterval

    val newSeriesBuffer = lineChart.getData.map(_.getName).map((i) => Dashboard.watchlist(i).cryptoToSeries(interval, "graph"))

    lineChart.getData.clear()

    for i <- newSeriesBuffer do
      lineChart.getData.addOne(i)

      if colors.keys.exists(_ == i.getName) then color(i.getName, colors(i.getName))
      else color(i.getName)

      tooltips(i)




  def color(id: String, color: String = "00FF00") =

    colors.update(id, color)

    val index = lineChart.data.value.indexWhere(_.getName ==  id)

    lineChart.data.value(index).getNode.setStyle(s"-fx-background-color: #$color, white;\n" +
          s"-fx-stroke: #$color;"
        )

    for point <- lineChart.data.value(index).getData do
        point.getNode.setStyle(s"-fx-background-color: #$color, white;\n" +
          s"-fx-stroke: #$color;"
        )

    cryptoMenu.items.find(_.getText == id) match
      case Some(menu) => menu.setStyle(s"-fx-background-color: #$color;")
      case _ =>





  var rectangleTool = false
  val rectangleToolButton = new ToggleButton("Measure Tool")

  rectangleToolButton.onAction = (event : ActionEvent) => {
    if rectangleToolButton.isSelected then rectangleTool = true
    else rectangleTool = false
  }


  val rectangle = new Rectangle {
    fill = Color.rgb(0, 0, 255, 0.3)
    stroke = Color.Blue
    strokeWidth = 1
    width = 0
    height = 0
    visible = false
  }



  var startX: Double = 0
  var startY: Double = 0

  def handleMousePressed(event: MouseEvent): Unit =
    if rectangleTool then

      startX = event.x
      startY = event.y

      rectangle.visible = true

      rectangle.x = event.x
      rectangle.y = event.y
      rectangle.width = 0
      rectangle.height = 0


      lineChart.cursor = Cursor.Crosshair


  def handleMouseDragged(event: MouseEvent): Unit =

    val x = Math.min(event.x, startX)
    val y = Math.min(event.y, startY)

    rectangle.x = x
    rectangle.y = y

    rectangle.width = Math.abs(event.getX - startX)
    rectangle.height = Math.abs(event.getY - startY)


  def handleMouseReleased(event: MouseEvent): Unit =

    if rectangleTool then

      val startYValue = yAxis.getValueForDisplay(startY - 38.0).doubleValue
      val endYValue = yAxis.getValueForDisplay(event.y - 38.0).doubleValue

      val percentage = -((startYValue - endYValue) / startYValue) * 100

      lineChart.cursor = Cursor.Default

      val infoText = s"Start: ${Formatter.formatDollarNumber(startYValue)}\nEnd: ${Formatter.formatDollarNumber(endYValue)}\nPercent: ${percentage.toString.take(5)} %"


      val alert = new Alert(AlertType.Information){
        title = "Measure tool"
        headerText = infoText
        buttonTypes = Seq(ButtonType.OK)
      }
      alert.showAndWait() match
        case Some(ButtonType.OK) => rectangle.visible = false
        case _ => rectangle.visible = false


  lineChart.onMousePressed = handleMousePressed
  lineChart.onMouseReleased = handleMouseReleased
  lineChart.onMouseDragged = handleMouseDragged



  def tooltips(series: javafx.scene.chart.XYChart.Series[String, Number]) =
    for point <- series.getData do
      val tooltip = new Tooltip(s"${point.getXValue}, ${Formatter.formatDollarNumber(point.getYValue.doubleValue)}")
      Tooltip.install(point.getNode, tooltip)

      point.getNode.setOnMousePressed(alertEvent)


      def alertEvent(event : MouseEvent): Unit =
        if !rectangleTool then
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



  val intervalChoice = new ChoiceBox[String] {
    items = ObservableBuffer("daily", "weekly", "monthly")
    value = "interval"
  }

  intervalChoice.onAction = (event: ActionEvent) => {
    changeInterval(intervalChoice.value.getValue)
  }


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
        intervalChoice,
        rectangleToolButton
      )
    }

  val ui: BorderPane = new BorderPane {
      prefWidth = 1200
      prefHeight = 800
      center = lineChart
      bottom = contentToolBar
      children += rectangle
    }
