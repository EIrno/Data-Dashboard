package cryptoDashboard.UI

import javafx.event.{ActionEvent, EventHandler}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Orientation
import scalafx.geometry.Orientation.{Horizontal, Vertical}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.*
import scalafx.scene.layout.Background.*
import scalafx.scene.layout.{Background, BorderPane}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer




class Component(var cType: String, var splitOrientation: Orientation):

  var children: ArrayBuffer[Component] = ArrayBuffer()
  var parent: Component = this
  var componentId: String = randomString(3)


  var content = cType match
    case "graph" => Graph()
    case "root" => Graph()
    case "pie" => Pie()
    case "card" => Card()
    case "bar" => Bar()



  def randomString(length: Int) =
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (i <- 1 to length) {
      sb.append(r.nextPrintableChar)
    }
    sb.toString



  def addChildren(newComponent: Component) =

    // case when splitting horizontaly on root level
    if (this.cType != "root" && this.parent.cType == "root" && newComponent.splitOrientation == Horizontal) then

      newComponent.parent = this.parent  // making the new element parent the root

      // inserting it to ui_items and children
      this.parent.ui.items.insert(this.parent.ui.items.indexOf(this.ui)+1, newComponent.ui)
      this.parent.children.insert(this.parent.children.indexOf(this)+1, newComponent)

    else if this.cType != "root" && (this.parent.cType != "root" || newComponent.splitOrientation == Vertical)  then // case when splitting anything else

      var placeholder = Component("graph", newComponent.splitOrientation) // placeholder
      placeholder.ui.items.remove(0) // making it clear of ui elements


      placeholder.componentId = placeholder.componentId + "placeholder"


      var temp_parent = this.parent // temp variable for this element parent because its going to change in the end
      placeholder.parent = temp_parent // making placeholder parent the current element parent
      var index_temp = this.parent.ui.items.indexOf(this.ui) // temp of index because repetition. also for insertion


      //removing this element from parent ui_items list and children list and adding placeholder to those places
      this.parent.ui.items.remove(index_temp)
      this.parent.ui.items.insert(index_temp, placeholder.ui)
      this.parent.children.remove(index_temp)
      this.parent.children.insert(index_temp, placeholder)

      this.ui.orientation = newComponent.splitOrientation


      // adding the existing element and the new element to the placeholder ui_items and children
      placeholder.ui.items.addOne(this.ui)
      placeholder.ui.items.addOne(newComponent.ui)
      placeholder.children.addOne(this)
      placeholder.children.addOne(newComponent)


      // making the placeholder items parent the placeholder
      newComponent.parent = placeholder
      this.parent = placeholder

    else // else is for the adding to the root from Main
      newComponent.parent = this.parent
      this.parent.ui.items.addOne(newComponent.ui)
      this.parent.children.addOne(newComponent)




  def delete =
    
    

    if this.parent.cType != "root" then // every other case
      var index_temp_placeholder_parent = this.parent.parent.ui.items.indexOf(this.parent.ui) // root elements ui_list index of the placeholder
      var index_temp_placeholder_parent_c = this.parent.parent.children.indexOf(this.parent) // root elements list index of the placeholder
      var index_temp_this = this.parent.ui.items.indexOf(this.ui) // placeholder children ui_list index
      var index_temp_this_c = this.parent.children.indexOf(this) //placeholder children list index


      this.parent.ui.items.remove(index_temp_this) // remove from placeholder ui_items this
      this.parent.parent.ui.items.remove(index_temp_placeholder_parent) // remove from root ui_items the placeholder
      this.parent.parent.children.remove(index_temp_placeholder_parent_c)
      this.parent.children.remove(index_temp_this_c) // remove from placeholder children this element
      this.parent.children.last.parent = this.parent.parent // placeholder remainingn item parent init root
      this.parent.parent.ui.items.insert(index_temp_placeholder_parent, this.parent.ui.items.last) // insert root ui_items remaingni ui_item
      this.parent.parent.children.insert(index_temp_placeholder_parent_c, this.parent.children.last)

      
    else // case when elements parent is root
      if this.parent.children.length > 1 then
        var index_temp_this = this.parent.ui.items.indexOf(this.ui)
        var index_temp_this_c = this.parent.children.indexOf(this)
        this.parent.ui.items.remove(index_temp_this)
        this.parent.children.remove(index_temp_this_c)
      else
        new Alert(AlertType.Information) {
            title = "Can't delete all the components"
          }.showAndWait()



  def changeContent(newContent: String): Unit =
    newContent match
      case "pie" =>
        content = Pie()
        this.cType = "pie"

      case "graph" =>
        content = Graph()
        this.cType = "graph"

      case "card" =>
        content = Card()
        this.cType = "card"

      case "bar" =>
        content = Bar()
        this.cType = "bar"

    content match
      case p: Pie => borderPane.center.value = p.ui
      case g: Graph => borderPane.center.value = g.ui
      case c: Card => borderPane.center.value = c.ui
      case b: Bar => borderPane.center.value = b.ui




  val contentChoice = new ChoiceBox[String]{
    items = ObservableBuffer("Graph", "Pie", "Bar", "Card")
    value = cType
    onAction = (event: ActionEvent) => {
      value.value match
        case "Pie" => changeContent("pie")
        case "Graph" => changeContent("graph")
        case "Card" =>  changeContent("card")
        case "Bar" =>  changeContent("bar")
    }
  }



  def newButton(name: String) =
    var button = new Button(name)
    button.onAction = (event: ActionEvent) => {
      name match
        case "x" => delete
        case "|" => addChildren(Component("graph", Horizontal))
        case "—" => addChildren(Component("graph", Vertical))
    }
    button


  private var toolBar =
    new ToolBar {
      orientation = Orientation.Horizontal
      items = Seq(
        newButton("x"),
        newButton("|"),
        newButton("—"),
        contentChoice
      )
    }

  var borderPane =
    new BorderPane {
      prefWidth = 1200
      prefHeight = 800
      top = toolBar
      if cType != "root" then
        // center = content.ui
        content match
          case p: Pie => center = p.ui
          case g: Graph => center = g.ui
          case c: Card => center = c.ui
          case b: Bar => center = b.ui
    }

  var rootSplitPane =
    new SplitPane {
        prefWidth = 160
        prefHeight = 200
        BorderPane.setAlignment(this, javafx.geometry.Pos.CENTER)
        orientation.value = splitOrientation
    }

  var nodeSplitPane =
    new SplitPane {
        prefWidth = 160
        prefHeight = 200
        BorderPane.setAlignment(this, javafx.geometry.Pos.CENTER)
        orientation.value = splitOrientation
        items.addOne(borderPane)
    }


  var ui: SplitPane = cType match
    case "graph" => nodeSplitPane
    case "root" => rootSplitPane
    case "pie" => nodeSplitPane
    case _ => nodeSplitPane


