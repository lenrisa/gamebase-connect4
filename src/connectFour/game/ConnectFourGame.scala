package connectFour.game

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import connectFour.logic.GameLogic
import engine.graphics.Color._
import processing.event.{KeyEvent, MouseEvent}

import java.awt.event
import java.awt.event.KeyEvent._
import processing.core.{PApplet, PConstants, PImage}



class ConnectFourGame extends GameBase {
  var menuBackground: PImage = createImage(0, 0, 0)
  var emptyCell : PImage = createImage(0, 0, 0)
  var purplePiece : PImage = createImage(0, 0, 0)
  var yellowPiece : PImage = createImage(0, 0, 0)
  var yellowGhost : PImage = createImage(0, 0, 0)
  var purpleGhost : PImage = createImage(0, 0, 0)
  var gameLogic = new GameLogic(GameLogic.DefaultDims, GameLogic.initialBoard, GameLogic.randomGen, singleplayer = false,
                                isGameRunning = false, isDifficultyScreen = false, difficulty = 1)
  val widthInPixels: Int = 900
  val heightInPixels: Int = 900
  val screenArea : Rectangle = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)
  val singleplayerButton : Rectangle = Rectangle(Point(100,450), (widthInPixels.toFloat / 3), heightInPixels.toFloat / 6)
  val multiplayerButton : Rectangle = Rectangle(Point(500,450), (widthInPixels.toFloat / 3), heightInPixels.toFloat / 6)
  val easyButton : Rectangle = Rectangle(Point(50,400), 200, heightInPixels.toFloat / 6)
  val mediumButton : Rectangle = Rectangle(Point(350,400), 200, heightInPixels.toFloat / 6)
  val hardButton : Rectangle = Rectangle(Point(650,400), 200, heightInPixels.toFloat / 6)

  // this function is wrongly named draw by processing (is called on each update next to drawing)
  override def draw(): Unit = {
    if (!gameLogic.isGameRunning & !gameLogic.isDifficultyScreen) drawMenu()
    else if (!gameLogic.isGameRunning & gameLogic.isDifficultyScreen) drawDifficultyScreen()
    else {
      drawGrid()
      if (gameLogic.gameOver) {
        if (gameLogic.getWinner == 0) drawYellowWonScreen()
        else if (gameLogic.getWinner == 1) drawPurpleWonScreen()
        else drawTieScreen()
      }
    }
  }

  def drawMenu() : Unit = {
    setFillColor(White)
    drawRectangle(screenArea)
    image(menuBackground, 0, 0)
    drawSingleplayerButton()
    drawMultiplayerButton()
  }

  def drawSingleplayerButton(): Unit = {
    setFillColor(Purple)
    drawRoundedRectangle(singleplayerButton)
    setFillColor(Color.White)
    drawTextCentered("SINGLEPLAYER", 30, Point(singleplayerButton.center.x, singleplayerButton.center.y + 8))
  }

  def drawMultiplayerButton(): Unit = {
    setFillColor(Purple)
    drawRoundedRectangle(multiplayerButton)
    setFillColor(Color.White)
    drawTextCentered("MULTIPLAYER", 30, Point(multiplayerButton.center.x, multiplayerButton.center.y + 8))
  }

  def drawDifficultyScreen(): Unit = {
    setFillColor(White)
    drawRectangle(screenArea)
    image(menuBackground, 0, 0)
    drawEasyButton()
    drawMediumButton()
    drawHardButton()
  }

  def drawEasyButton() : Unit = {
    setFillColor(Purple)
    drawRoundedRectangle(easyButton)
    setFillColor(Color.White)
    drawTextCentered("EASY", 20, Point(easyButton.center.x, easyButton.center.y + 8))
  }

  def drawMediumButton(): Unit = {
    setFillColor(Purple)
    drawRoundedRectangle(mediumButton)
    setFillColor(Color.White)
    drawTextCentered("MEDIUM", 20, Point(mediumButton.center.x, mediumButton.center.y + 8))
  }

  def drawHardButton(): Unit = {
    setFillColor(Purple)
    drawRoundedRectangle(hardButton)
    setFillColor(Color.White)
    drawTextCentered("HARD", 20, Point(hardButton.center.x, hardButton.center.y + 8))
  }

  def drawYellowWonScreen(): Unit = {
    setFillColor(White)
    drawTextCentered("YELLOW WON!", 60, screenArea.center)
  }

  def drawPurpleWonScreen(): Unit = {
    setFillColor(White)
    drawTextCentered("PURPLE WON!", 60, screenArea.center)
  }

  def drawTieScreen(): Unit = {
    setFillColor(White)
    drawTextCentered("IT'S A TIE!", 60, screenArea.center)
  }

  def drawGrid(): Unit = {

    val widthInCells = GameLogic.DefaultDims.width
    val heightInCells = GameLogic.DefaultDims.height
    val widthPerCell = (screenArea.width / widthInCells) min ConnectFourGame.MaxCellWidth.toFloat
    val heightPerCell = (screenArea.height / heightInCells) min ConnectFourGame.MaxCellHeigth.toFloat
    val cellEdge = widthPerCell min heightPerCell
    val usedScreenWidthPixels = cellEdge * widthInCells
    val usedScreenHeightPixels = cellEdge * heightInCells
    val left = screenArea.left + (screenArea.width - usedScreenWidthPixels) / 2
    val top = screenArea.top + (screenArea.height - usedScreenHeightPixels) / 2
    val actualScreen = Rectangle(Point(left,top), usedScreenWidthPixels, usedScreenHeightPixels)
    val gameBoardScreen = Rectangle(Point(left - cellEdge/4,top + cellEdge), usedScreenWidthPixels + (cellEdge / 2), usedScreenHeightPixels - cellEdge)
    val screenBottom = Rectangle(Point(0,screenArea.height - (cellEdge/2)), screenArea.width, cellEdge/2)

    def getCell(colIndex: Int, rowIndex: Int): Rectangle = {
      val leftUp = Point(actualScreen.left + colIndex * cellEdge,
        actualScreen.top + rowIndex * cellEdge)
      Rectangle(leftUp, cellEdge, cellEdge)
    }

    def drawCell(area: Rectangle, cell: CellType): Unit = {
      val newArea = area.grow((0.95f))
      cell match {
        case TopRow() =>
          setFillColor(Color.White)
          drawEllipse(area.grow(0.95f))
        case PlayerAGhost() =>
          image(yellowGhost, newArea.left, newArea.top)
        case PlayerBGhost() =>
          image(purpleGhost, newArea.left, newArea.top)
        case PlayerA() =>
          image(yellowPiece, newArea.left, newArea.top)
        case PlayerB() =>
          image(purplePiece, newArea.left, newArea.top)
        case Empty() =>
          image(emptyCell, newArea.left, newArea.top)
      }
    }

    setFillColor(Cyan)
    drawRectangle(screenArea)

    setFillColor(NavyBlue)
    drawRoundedRectangle(gameBoardScreen)

    setFillColor(Gray)
    drawRectangle(screenBottom)

    setFillColor(Color.White)
    drawTextCentered("Use arrows to move the piece. Press space to drop the piece.\n Press 'q' to QUIT. Press 'r' to RESTART",
      15, Point(screenBottom.center.x, screenBottom.center.y + 5))

    for (y <- 0 until GameLogic.DefaultDims.width;
         x <- 0 until GameLogic.DefaultDims.height) {
      drawCell(getCell(x, y), gameLogic.getCellType(x, y))
    }

  }

  /** Method that calls handlers for different key press events.
   * You may add extra functionality for other keys here.
   * See [[event.KeyEvent]] for all defined keycodes.
   *
   * @param event The key press event to handle
   */
  override def keyPressed(event: KeyEvent): Unit = {

    event.getKeyCode match {
      case VK_LEFT => gameLogic.moveLeft()
      case VK_RIGHT => gameLogic.moveRight()
      case VK_SPACE => gameLogic.drop()
      case VK_Q => exit()
      case VK_R => gameLogic = new GameLogic(GameLogic.DefaultDims, GameLogic.initialBoard,
        GameLogic.randomGen, false, isGameRunning = false, isDifficultyScreen = false, difficulty = 1)
      case _        => ()
    }

  }

  def isMouseInArea(area: Rectangle): Boolean = {
    mouseX >= area.leftUp.x && mouseY >= area.leftUp.y && mouseX <= area.leftUp.x + area.width && mouseY <= area.leftUp.y + area.height
  }

  override def mouseClicked(event: MouseEvent): Unit = {
    if (!gameLogic.isGameRunning) {
      if(!gameLogic.isDifficultyScreen) {
        if(isMouseInArea(singleplayerButton))
          gameLogic = new GameLogic(GameLogic.DefaultDims, GameLogic.initialBoard, GameLogic.randomGen,
            true, isGameRunning = false, isDifficultyScreen = true, difficulty = 1)
        else if (isMouseInArea(multiplayerButton))
          gameLogic = new GameLogic(GameLogic.DefaultDims, GameLogic.initialBoard, GameLogic.randomGen,
            false, isGameRunning = true, isDifficultyScreen = false, difficulty = 1)
      }
      else { //difficulty screen
        if (isMouseInArea(easyButton))
          gameLogic = new GameLogic(GameLogic.DefaultDims, GameLogic.initialBoard, GameLogic.randomGen,
            true, isGameRunning = true, isDifficultyScreen = false, difficulty = 1)
        else if (isMouseInArea(mediumButton))
          gameLogic = new GameLogic(GameLogic.DefaultDims, GameLogic.initialBoard, GameLogic.randomGen,
            true, isGameRunning = true, isDifficultyScreen = false, difficulty = 2)
        else if (isMouseInArea(hardButton))
          gameLogic = new GameLogic(GameLogic.DefaultDims, GameLogic.initialBoard, GameLogic.randomGen,
            true, isGameRunning = true, isDifficultyScreen = false, difficulty = 4)
      }
    }
  }


  override def settings(): Unit = {
    pixelDensity(displayDensity())
    // If line below gives errors try size(widthInPixels, heightInPixels, PConstants.P2D)
    size(widthInPixels, heightInPixels)
    keyRepeatEnabled = true
  }

  override def setup(): Unit = {
    text("", 0, 0)
    menuBackground = loadImage("sprites/background.png", "png")
    emptyCell = loadImage("sprites/empty.png", "png")
    purplePiece = loadImage("sprites/purplePiece.png", "png")
    yellowPiece = loadImage("sprites/yellowPiece.png", "png")
    purpleGhost = loadImage("sprites/purpleGhost.png", "png")
    yellowGhost = loadImage("sprites/yellowGhost.png", "png")

    //backgroundImage.resize(widthInPixels, heightInPixels)
  }

}


object ConnectFourGame {

  val MaxCellWidth : Int = 100
  val MaxCellHeigth : Int = MaxCellWidth

  def main(args: Array[String]): Unit = {
    // This is needed for Processing, using the name
    // of the class in a string is not very beautiful...
    PApplet.main("connectFour.game.ConnectFourGame")
  }

}