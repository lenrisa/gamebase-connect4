// DO NOT MODIFY FOR BASIC SUBMISSION
// scalastyle:off

package engine

import engine.graphics.Color.Black
import engine.graphics.{Color, Point, Rectangle, Triangle}
import processing.core.{PApplet, PConstants}

class GameBase   extends PApplet {

  // inner class: can call current time of outer class
  class UpdateTimer(val framesPerSecond: Float) {

    val frameDuration: Float = 1000 / framesPerSecond // ms
    var nextFrame: Float = Float.MaxValue

    def init(): Unit = nextFrame = currentTime() + frameDuration
    def timeForNextFrame(): Boolean = currentTime() >= nextFrame
    def advanceFrame(): Unit = nextFrame = nextFrame + frameDuration

  }

  // ===Processing Wrappers & Abstractions===

  /** An alias for the obscurely named function millis()
    *
    * @return Current time in milliseconds since stating the program.
    */
  def currentTime(): Int = millis()

  def drawTextCentered(string: String, size: Float, center: Point, withShadow: Boolean = false): Unit = {
    val (x, y) = (center.x, center.y-(size/2))
    textAlign(PConstants.CENTER, PConstants.CENTER)
    textSize(size)
    drawText(string, Point(x, y), withShadow)
  }

  def drawText(string: String, pos: Point, withShadow: Boolean = true): Unit = {
    if (withShadow) drawTextShadow(string, pos)
    text(string, pos.x, pos.y)
  }

  /** Quick hack for legibility on different backgrounds */
  def drawTextShadow(string: String, pos: Point, color: Color = Black, thickness: Float = 1): Unit = {
    pushStyle()
    setFillColor(color)
    List((1,0),(-1,0),(0,1),(0,-1)).foreach(t => {
      text(string, pos.x+(t._1*thickness), pos.y+t._2*thickness)
    })
    popStyle()
  }

  def drawLine(p1 : Point, p2 : Point) : Unit =
    line(p1.x,p1.y, p2.x,p2.y ) 

  def drawRectangle(r: Rectangle): Unit =
    rect(r.left, r.top, r.width, r.height)

  def drawRoundedRectangle(r: Rectangle): Unit =
    rect(r.left, r.top, r.width, r.height, 30)

  def drawEllipse(r: Rectangle): Unit =
    ellipse(r.center.x, r.center.y, r.width, r.height)

  def setFillColor(c: Color): Unit =
    fill(c.red, c.green, c.blue, c.alpha)
}
