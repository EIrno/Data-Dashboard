package cryptoDashboard.app

import cryptoDashboard.services.*
import cryptoDashboard.services.API.RequestError
import cryptoDashboard.services.Parser.ParserError
import cryptoDashboard.UI.Component
import cryptoDashboard.crypto.Crypto
import javafx.event.ActionEvent
import os.read.inputStream
import scalafx.collections.ObservableBuffer
import scalafx.collections.transformation.FilteredBuffer
import scalafx.geometry.Orientation.{Horizontal, Vertical}
import scalafx.scene.control
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.*
import scalafx.stage.FileChooser

import java.io.{File, FileInputStream, FileOutputStream, InputStream}
import java.net.URL
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using



object Dashboard:

  val watchlist: scala.collection.mutable.Map[String,Crypto] = scala.collection.mutable.Map()
  val watchlistID = ObservableBuffer[String]()

  val marketData = API.marketCapPercentages

  def addToWatchlist(id: String, builder: Boolean = false)  =
    if !watchlistID.contains(id) then
      try
        val graphData = API.chart(id, "daily")
        val priceData = API.price(id)
        watchlist(id) = Crypto(id, priceData._1, priceData._2, priceData._3, priceData._4, graphData)
        watchlistID.addOne(id)
      catch
        case e:RequestError => new Alert(AlertType.Information) {
          title = "API Error"
          headerText = e.id + " was not added to the dashboard"
          contentText = "Either you don't have internet connection or you have exceeded the api limits temporarily"
        }.showAndWait()
        case e:ParserError => new Alert(AlertType.Information) {
          title = "Data Error"
          headerText = e.id + " was not added to the dashboard"
          contentText = "Reason: " + e.description
        }.showAndWait()
        case _ => new Alert(AlertType.Information) {
          title = "Unexpected error ocurred"
        }.showAndWait()
    else
      if !builder then
        new Alert(AlertType.Information) {
            title = "Crypto is already in the watchlist"
          }.showAndWait()


  def deleteFromWatchlist(id: String) =
    watchlist.remove(id)
    watchlistID.remove(watchlistID.indexOf(id))




  //////////////////////////////////////////////////////



  val path = Paths.get("src/resources/data/cryptoList.json").toAbsolutePath.toString

  val json = ujson.read(Source.fromFile(path, "UTF-8").mkString)

  val symbolIdMap = json.arr.map((i) => (i.obj("id").str, i.obj("id").str)).toMap

  val data = ObservableBuffer.from(symbolIdMap.keys.toList.sorted(Ordering.by(_.length)))

  val filteredData = new FilteredBuffer[String](data)

  val listView = new ListView[String](filteredData){
    prefHeight = 200
  }

  val searchField = new TextField {
      promptText = "Search..."
    }

  searchField.textProperty().addListener((_, _, newValue) =>
      filteredData.setPredicate(item =>
        if (newValue == null || newValue.isEmpty)
          true
        else
          item.toLowerCase.contains(newValue.toLowerCase)
      )
    )

  val addButton = new Button("Add") {
    onAction = (event : ActionEvent) => {
      addToWatchlist(listView.getSelectionModel.getSelectedItem)
    }
  }

  val searchAndAdd = new ToolBar{
    items = Seq(searchField, addButton)
    orientation = Horizontal
  }

  val watchlistChoiceBox = new ChoiceBox[String]{
    items = watchlistID
    value = "Watchlist"
    onAction = (event: ActionEvent) => {
      deleteID = value.value
    }
  }

  var deleteID = ""

  val deleteButton = new Button("Delete") {
    onAction = (event: ActionEvent) => {
      if deleteID.nonEmpty && watchlistID.contains(deleteID) then
        deleteFromWatchlist(deleteID)
        deleteID = ""
        watchlistChoiceBox.value = "Watchlist"
    }
  }


  val saveButton = new Button("Save") {
    onAction = (event: ActionEvent) => {
      MakeFile.save()

      val fileChooser = new FileChooser()
      fileChooser.title = "Save File"
      val selectedFile = fileChooser.showSaveDialog(LaunchDashboard.stage)
      if (selectedFile != null)
        val destFilePath = selectedFile.getAbsolutePath
        try
          val currentDirectory = Paths.get(".").toAbsolutePath.normalize.toString
          val filePath = s"$currentDirectory/src/resources/data/dashboardData/output.json"

          Files.copy(Paths.get(filePath), Paths.get(destFilePath))
        catch
          case ex: Exception => new Alert(AlertType.Information) {
            title = "Error downloading file. Please try again"
          }.showAndWait()
    }
  }


  val hideButton: Button = new Button("≡") {
      onAction = hideToolbar
    }

  val toolBarSeq = Seq(
        hideButton,
        searchAndAdd,
        listView,
        watchlistChoiceBox,
        deleteButton,
        saveButton,
      )


  def hideToolbar(event: ActionEvent) =
    if toolBar.items.length > 1 then
      toolBar.items = hideButton
    else
      toolBar.items = toolBarSeq

  val toolBar = new ToolBar {
    orientation = Vertical
    items = toolBarSeq
  }


