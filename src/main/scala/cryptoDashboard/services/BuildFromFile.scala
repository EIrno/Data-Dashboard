package cryptoDashboard.services

import cryptoDashboard.UI.{Bar, Card, Component, Graph, Pie}
import cryptoDashboard.app.Dashboard
import cryptoDashboard.app.Dashboard.watchlist
import play.api.libs.json.{JsObject, JsValue, Json}
import scalafx.geometry.Orientation.{Horizontal, Vertical}
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType





/**
 * object is used to build the layout of the dashboard.
 * it is  called at launch
 * 
 */

object BuildFromFile:

    def build(json: String = ""): Component =
        try
            def readSaveFile(path: String): JsValue =
                val source = scala.io.Source.fromFile(path)
                val jsonString = try source.mkString finally source.close()
                // Parse JSON
                val json = Json.parse(jsonString)
                json


            val dataJson: JsValue =
                if json.isEmpty then
                    readSaveFile("src/resources/data/dashboardData/output.json")
                else
                    Json.parse(json)


            val structureData = (dataJson \ "components")
            val rootComponentId = structureData.as[Map[String, JsObject]].collectFirst {
                case (componentId, obj) if (obj \ "cType").asOpt[String].contains("root") =>
                    componentId
            }


            val watchlistData = (dataJson \ "watchlist").as[Seq[String]]
            watchlistData.foreach(id => Dashboard.addToWatchlist(id, true))

            val componentsList = scala.collection.mutable.Map[String, Component]() // List for all component

            def getOrientationData(comp: JsObject): scalafx.geometry.Orientation =
                val direction: String = (comp \ "orientation").as[String] // get direction
                if direction == "HORIZONTAL" then
                    Horizontal
                else
                    Vertical

            // cycle through the components(json) where ComponentId is String and component is a jSObject
            structureData.as[Map[String, JsObject]].foreach { case (componentId, component) =>

                val orientationData = getOrientationData(component)
                val cTypeData: String = (component \ "cType").as[String]
                val contentData: Seq[String] = (component \ "contentData").as[Seq[String]]
                var visibleData: Map[String, String] = (component \ "dataVisible").as[Map[String, String]]
                val colorData: Map[String, String] = (component \ "dataColor").as[Map[String, String]]

                componentsList(componentId) = Component(cTypeData, orientationData) // make a component
                componentsList(componentId).content match
                    case p: Pie =>
                    case g: Graph =>
                        val filteredContentData = contentData.filter(p => Dashboard.watchlist.contains(p))

                        filteredContentData.foreach(coin =>
                          if colorData.contains(coin) then
                             g.addSeries(coin, colorData(coin))
                          else
                             g.addSeries(coin)
                        )

                        g.changeInterval((component \ "contentInterval").as[String])

                    case c: Card =>
                        val filteredContentData = contentData.filter(p => watchlist.contains(p))
                        if filteredContentData.nonEmpty then
                            c.selectedCrypto = contentData.head
                            visibleData.filter((key, value) => value != "").keys.foreach(key => c.add(key))
                    case b: Bar =>

                        val filteredContentData = contentData.filter(p => watchlist.contains(p))

                        filteredContentData.foreach(coin =>
                          if colorData.contains(coin) then
                             b.addSeries(coin, colorData(coin))
                          else
                             b.addSeries(coin)
                        )
            }



             def addChildrenOnBuild(newComponent: Component, currentElem: Component) =
                if (currentElem.cType != "root") then
                    currentElem.ui.items.addOne(newComponent.ui)
                    currentElem.children.addOne(newComponent)

                else // else is for the adding to the root from Main
                    newComponent.parent = currentElem.parent
                    currentElem.ui.items.addOne(newComponent.ui)
                    currentElem.children.addOne(newComponent)


            def buildingStructure(root: String): Boolean =
                var currentElem = componentsList(root)
                var currentElemJson = (structureData \ root)
                val tempParent: String = (currentElemJson \ "parent").as[String]
                currentElem.parent = componentsList(tempParent)
                val children = (currentElemJson \ "children").as[Seq[String]]
                if children.isEmpty then
                    return true

                children.foreach(child => addChildrenOnBuild(componentsList(child), currentElem))

                if currentElem.cType != "root" then
                    currentElem.ui.items.remove(0)
                children.foreach(child => buildingStructure(child))
                true

            buildingStructure(rootComponentId.get)
            componentsList(rootComponentId.get)

        catch
            case _ => new Alert(AlertType.Information) {
                  title = "Unexpected error while loading the file."
                  headerText = "Loading the last saved dashboard"
                }.showAndWait()
                this.build()
