package cryptoDashboard.services

import cryptoDashboard.UI.{Bar, Card, Component, Graph, Pie}
import cryptoDashboard.app.Dashboard
import play.api.libs.json.Json
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scalafx.Includes.*
import play.api.libs.json.*


/**
 * object for making the json that represents the layout
 */

object MakeFile:

    case class JsonObjectData(parent: String, cType: String, splitOrientation: String, children: Seq[String], contentData: Seq[String], contentInterval: String, dataVisible: Map[String, String], dataColor: scala.collection.mutable.Map[String,String])

    var root: Option[Component] = None

    var jsonData: Map[String, JsonObjectData] = Map()

    def makeObject(current: Component, contentData: Seq[String], contentInterval: String, dataVisible: Map[String, String], dataColor : scala.collection.mutable.Map[String,String]) =
        val tempChildrenId: Seq[String] = current.children.map(child => child.componentId).toSeq
        jsonData += (current.componentId -> JsonObjectData(current.parent.componentId, current.cType, current.splitOrientation.toString, tempChildrenId, contentData, contentInterval, dataVisible, dataColor))


    def buildVisibleData(data: Map[String, String]): Map[String, String] =
        val tempMap: Map[String, String] = Map(
            "price" -> "",
            "marketCap" -> "",
            "volume24h" -> "",
            "yearEnding" -> "",
            "yearToDate" -> "",
            "average30" -> "",
            "range30" -> "",
            "range365" -> "")
        if data.nonEmpty then
            val dataTemp = data.map((key, value) => key -> "true")
            val mergeMap = tempMap ++ dataTemp
            mergeMap
        else
            tempMap

    def traverseToRoot(): Unit =
        val componentList: ArrayBuffer[Component] = ArrayBuffer()

        var currentElem = root.get

        while currentElem.cType != "root" do
            currentElem = currentElem.parent

        def travelRecursive(current: Component): Boolean =
            // val tempChildrenId: Seq[String] = current.children.map(child => child.componentId).toSeq
            current.content match
                case p: Pie => makeObject(current, Seq(), "", buildVisibleData(Map()), scala.collection.mutable.Map[String,String]())
                case g: Graph => makeObject(current, g.lineChart.getData.map(_.getName).toSeq, g.interval, buildVisibleData(Map()), g.colors)
                case c: Card => makeObject(current, Seq(c.selectedCrypto), "", buildVisibleData(c.dataVisible), scala.collection.mutable.Map[String,String]())
                case b: Bar => makeObject(current, b.barChart.getData.map(_.getName).toSeq, "", buildVisibleData(Map()), b.colors)
            if current.children.nonEmpty then
                for i <- current.children do
                    travelRecursive(i)

            true
        travelRecursive(currentElem)


    def save() =
        traverseToRoot()
        val jsond = Json.toJson(jsonData.view.mapValues ( component =>
                val tempJson = Json.toJson(component.dataVisible)
                val colorTempJson = Json.toJson(component.dataColor)
                Json.obj(
                    "parent" -> component.parent,
                    "children" -> component.children,
                    "cType" -> component.cType,
                    "orientation" -> component.splitOrientation,
                    "contentData" -> component.contentData,
                    "contentInterval" -> component.contentInterval,
                    "dataVisible" -> tempJson,
                    "dataColor" -> colorTempJson
                )
            )
        )

        val watchlistDataJson = Json.toJson(Dashboard.watchlist.keys)

        val finalJson = Json.obj(
            "components" -> jsond,
            "watchlist" -> watchlistDataJson
        )


        val file = new java.io.File("src/resources/data/dashboardData/output.json")
        val bw = new java.io.BufferedWriter(new java.io.FileWriter(file))
        bw.write(Json.prettyPrint(finalJson))
        bw.close()