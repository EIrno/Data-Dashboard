package cryptoDashboard.app

import cryptoDashboard.app.LaunchDashboard.stage
import cryptoDashboard.services.{API, BuildFromFile, MakeFile}
import javafx.collections.{ObservableArray, ObservableList}
import javafx.event.{ActionEvent, EventHandler}
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Orientation
import scalafx.geometry.Orientation.{Horizontal, Vertical}
import scalafx.scene.Scene
import scalafx.scene.chart.XYChart.{Data, Series}
import scalafx.scene.chart.{CategoryAxis, LineChart, NumberAxis, XYChart}
import scalafx.scene.control.*
import scalafx.scene.layout.BorderPane
import scalafx.stage.FileChooser
import cryptoDashboard.services.{BuildFromFile, MakeFile}
import scalafx.scene.control.Alert.AlertType

import java.time
import java.time.{Instant, LocalDate, ZoneOffset}
import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.io.Source




object LaunchDashboard extends JFXApp3 {


  def start(): Unit =

    val root = new BorderPane {
      prefWidth = 1200
      prefHeight = 800
      right = cryptoDashboard.app.Dashboard.toolBar
    }


    stage = new JFXApp3.PrimaryStage {
      title = "Crypto Dashboard"
      scene = new Scene(root)
    }


    def uploadFile(): String =
      val fileChooser = new FileChooser()
      fileChooser.title = "Open Dashboard"
      val selectedFile = fileChooser.showOpenDialog(LaunchDashboard.stage)
      var content = ""

      if (selectedFile != null) then
        try
          content = Source.fromFile(selectedFile).mkString
        catch
          case ex: Exception =>
            content = ""
            new Alert(AlertType.Information) {
              title = "Unexpected error while loading the file."
              headerText = "Loading the last saved dashboard"
            }.showAndWait()
      content


    val alert = new Alert(Alert.AlertType.Confirmation) {
      title = "File Upload?"
      contentText = "Do you want to upload a new file?"
      buttonTypes = Seq(ButtonType.OK, ButtonType.No)
    }


    val result = alert.showAndWait() match
      case Some(ButtonType.OK) => uploadFile()
      case Some(ButtonType.No) => ""
      case _ => ""


    val rootSplitPane = BuildFromFile.build(result)

    root.center = rootSplitPane.ui
    MakeFile.root = Option(rootSplitPane)
}

