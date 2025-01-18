package cryptoDashboard.UI

import javafx.event.{ActionEvent, EventHandler}
import scalafx.beans.binding.Bindings
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.beans.value.ObservableValue
import scalafx.collections.ObservableBuffer.Remove
import scalafx.geometry.{Orientation, Pos}
import scalafx.scene.control.*
import scalafx.scene.layout.BorderPane
import scalafx.scene.text.{Font, Text}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import cryptoDashboard.app.Dashboard
import cryptoDashboard.services.utils.Formatter



class Card:

  var selectedCrypto: String = ""

  def data: Map[String, String] = Map(
    "price" -> price,
    "marketCap" -> marketCap,
    "volume24h" -> volume24h,
    "yearEnding" -> yearEnding,
    "yearToDate" -> yearToDate,
    "average30" -> average30,
    "range30" -> range(30),
    "range365" -> range(365)
  )

  var dataVisible: Map[String, String] = Map()


  def price = "Price: " + Formatter.formatDollarNumber(Dashboard.watchlist(selectedCrypto).price) +  "(" + change24h + ")"

  def marketCap = "Marketcap: " + Formatter.formatDollarNumber(Dashboard.watchlist(selectedCrypto).marketcap)

  def volume24h = "24h volume: " + Formatter.formatDollarNumber(Dashboard.watchlist(selectedCrypto).volume24)

  def change24h = Dashboard.watchlist(selectedCrypto).change24.toString.take(4) + " %"

  def range(days: Int) =
    val history = Dashboard.watchlist(selectedCrypto).history.map(i => (i._1, i._2)).takeRight(days)
    val top = history.maxBy(_._2)
    val bottom = history.minBy(_._2)
    s"Price range $days days: ${Formatter.formatDollarNumber(bottom._2)} – ${Formatter.formatDollarNumber(top._2)}"

  def yearEnding =
    val crypto = Dashboard.watchlist(selectedCrypto)
    val prices = crypto.history.map(_._2)
    "Year-ending: " + (((prices.last - prices.head) / prices.head) * 100).toString.take(5) + " %"

  def yearToDate =
    val crypto = Dashboard.watchlist(selectedCrypto)
    val yearLast = crypto.history.find(_._1.getDayOfYear == 365).get
    "Year-to-date: " + (((crypto.price - yearLast._2) /  yearLast._2) * 100).toString.take(5) + " %"

  def average30 =
    val prices = Dashboard.watchlist(selectedCrypto).history.map(_._2).takeRight(30)
    "average 30 days: " + Formatter.formatDollarNumber(prices.sum / 30.0)




  def createCard(): String =
    if selectedCrypto == "" || selectedCrypto == null then "select Crypto"
    else
      dataVisible = data.filter(i => dataVisible.contains(i._1))
      cardMenu.items = cardMenuItems()
      changeText()


  def changeText(): String =
    selectedCrypto + "\n" +  dataVisible.values.mkString("\n")

  def remove(dataPoint: String) =
    dataVisible = dataVisible.removed(dataPoint)
    label.text = changeText()

  def add(dataPoint: String) =
    dataVisible = dataVisible ++ Map(dataPoint -> data(dataPoint))
    cardMenu.items = cardMenuItems()
    label.text = changeText()
    cryptoSelector.value = selectedCrypto



  val label = new Label(createCard()){
    font = Font.font(10)
  }


  val cryptoSelector = new ChoiceBox[String]{
    items = Dashboard.watchlistID

    onAction = (event: ActionEvent) => {

      if value.value == null then
        label.text = "select crypto"
      else
        selectedCrypto = value.value
        label.text = createCard()
    }
  }



  Dashboard.watchlistID.onChange((buffer, changes) =>
    cryptoSelector.items = Dashboard.watchlistID
    for change <-changes do
      change match
        case Remove(pos, removed) =>
          if removed.head == selectedCrypto then
            selectedCrypto  = ""
        case _ =>
  )


  val cardMenu = new MenuButton("select data")



  def cardMenuItems(): Seq[CheckMenuItem] =
    data.keys.map((i) =>
      new CheckMenuItem(i) {
        selected = if dataVisible.contains(i) then true else false
        selected.onChange((_, oldValue, newValue) =>
          if newValue then
            add(i)
          else
            remove(i)
        )
      }
    ).toSeq



  private val contentToolBar = new ToolBar {
      orientation = Orientation.Horizontal
      items = Seq(
        cryptoSelector,
        cardMenu
      )
    }


  val ui: BorderPane = new BorderPane {
    prefWidth = 1200
    prefHeight = 800
    bottom = contentToolBar
    center = label
  }

  ui.boundsInLocal.onChange( (_, _, _) =>
    label.font = Font.font((ui.getHeight + ui.getWidth) / 40)
  )



