package connectFour.logic

import connectFour.game.{CellType, Empty, PlayerA, PlayerB, TopRow}
import engine.random.{RandomGenerator, ScalaRandomGen}
import scala.util.control.Breaks._
import scala.collection.mutable

case class GameState(
                      gridDims : Dimensions,
                      currentBoard : Seq[Seq[CellType]],
                      randomGen: RandomGenerator,
                      singleplayer : Boolean,
                      isDifficultyScreen : Boolean = false,
                      difficulty : Int,
                      isGameRunning : Boolean = false,
                      player : Point = Point(0,0),
                      playerGhost : Point = Point(0,6),
                      playerColor : CellType = PlayerA(),
                      isGameOver : Boolean = false,
                      winner : Int = -1) {

  def cellTypeAt(p: Point): CellType = {
    if (p == player) this.playerColor
    else if (p == playerGhost) this.playerColor.ghost
    else if (p.y == 0) TopRow()
    else this.boardMap(this.currentBoard).apply(p)
  }

  def moveRight() : GameState = {
    if (this.player.x == gridDims.width - 1)
      this.copy(player = Point(0,0), playerGhost = getFinalPlace(this.currentBoard,Point(0,0)))
    else {
      val newPlayer = this.player + Point(1,0)
      this.copy(player = newPlayer, playerGhost = getFinalPlace(this.currentBoard,newPlayer))
    }
  }

  def moveLeft(): GameState = {
    if (this.player.x == 0)
      this.copy(player = Point(gridDims.width - 1, 0),
        playerGhost = getFinalPlace(this.currentBoard, Point(gridDims.width - 1, 0)))
    else {
      val newPlayer = this.player + Point(-1, 0)
      this.copy(player = newPlayer, playerGhost = getFinalPlace(this.currentBoard, newPlayer))
    }
  }

  def boardMap(board : Seq[Seq[CellType]]): Map[Point, CellType] = {
    (for ((row, rowIndex) <- board.zipWithIndex;
          (cell, cellIndex) <- row.zipWithIndex)
    yield (Point(cellIndex, rowIndex), cell)
      ).toMap
  }

  def moveDown() : GameState = {
    this.copy(player = this.getFinalPlace(this.currentBoard, this.player))
  }

  def getFinalPlace(board : Seq[Seq[CellType]], player : Point) : Point = {
    val column = this.boardBottom(board).filter(p => p.x == player.x)
    var finalPlace = column.head
    column.foreach(p => if (p.y < finalPlace.y) finalPlace = p)
    finalPlace
  }

  def boardBottom(board : Seq[Seq[CellType]]): List[Point] = {
    val bottomLine = this.boardMap(board).filter(p => p._1.y == gridDims.height - 1).keys.toList
    this.currentTakenSpots(board).map(p => p + Point(0, -1)) ++ bottomLine
  }

  def currentTakenSpots(board : Seq[Seq[CellType]]): List[Point] = {
    this.boardMap(board).filter(x => !x._2.isEmpty).keys.toList
  }

  def addPieceToBoard() : GameState = {
    val p = player
    this.copy(currentBoard = this.currentBoard.updated(p.y, this.currentBoard(p.y).updated(p.x, this.playerColor)))
  }

  def checkGameOver(board: Seq[Seq[CellType]]): Boolean = {
    this.checkWinner(board, PlayerA()) | this.checkWinner(board, PlayerB()) | this.fullBoard(board)
  }

  def updateBoardAndSpawnPiece(): GameState = {
    if(this.checkGameOver(this.currentBoard)) {
      val newWinner = getWinner()
      this.copy(isGameOver = true, winner = newWinner)
    }
    else {
      val newPlayer = Point(0, 0)
      val newGhost = getFinalPlace(this.currentBoard, newPlayer)
      val newPlayerColor = this.playerColor.opposite
      this.copy(player = newPlayer, playerColor = newPlayerColor, playerGhost = newGhost)
    }
  }

  def getWinner() : Int = {
    var currWinner = -1
    if (this.checkWinner(this.currentBoard, this.playerColor)) {
      if (this.playerColor == PlayerA()) currWinner = 0
      if (this.playerColor == PlayerB()) currWinner = 1
    }
    currWinner
  }

  def fullBoard(board : Seq[Seq[CellType]]): Boolean = {
    if (this.currentTakenSpots(board).length == gridDims.width * (gridDims.height - 1)) true
    else false
  }

  def isColumnFull(column : Int, board : Seq[Seq[CellType]]) : Boolean = {
    if(board(1)(column) != Empty()) true
    else false
  }

  def checkWinner(board : Seq[Seq[CellType]], player : CellType) : Boolean = {
    var won = false

    //horizontal wins
    for (x <- 0 until gridDims.width - 3) {
      for (y <- 1 until gridDims.height) {
        if(board(y)(x) == player & board(y)(x+1) == player
          & board(y)(x+2) == player & board(y)(x+3) == player) won = true
      }
    }

    //vertical wins
    for (x <- 0 until gridDims.width) {
      for (y <- 1 until gridDims.height - 3) {
        if (board(y)(x) == player & board(y + 1)(x) == player
          & board(y + 2)(x) == player & board(y + 3)(x) == player) won = true
      }
    }

    // diagonal \ wins
    for(x <- 0 until gridDims.width - 3) {
      for (y <- 1 until gridDims.height - 3) {
        if(board(y)(x) == player & board(y+1)(x+1) == player
          & board(y+2)(x+2) == player & board(y+3)(x+3) == player) won = true
      }
    }

    //diagonal / wins
    for (x <- 0 until gridDims.width - 3) {
      for (y <- 3 until gridDims.height) {
        if (board(y)(x) == player & board(y - 1)(x + 1) == player
          & board(y - 2)(x + 2) == player & board(y - 3)(x + 3) == player) won = true
      }
    }

    won
  }


  def calculateMoveScore(move : Seq[CellType], player : CellType) : Int = {
    var score = 0
    val pieceCount = move.count(p => p == player)
    val emptyCount = move.count(p => p == Empty())

    if (pieceCount == 4) score += 100 //winning move
    else if (pieceCount == 3 & emptyCount == 1) score += 5 //3 in a row
    else if (pieceCount == 2 & emptyCount == 2) score += 2 //2 in a row

    if (move.count(p => p == player.opposite) == 3 & emptyCount == 1) score -= 4 //opponent 3 in a row move

    score
  }

  def getMoveScore(board: Seq[Seq[CellType]], player : CellType): Int = {
    var score = 0

    //Middle column
    val centerColumn = board.map{_(3)}
    val centerPieceCount = centerColumn.count(p => p == player)
    score += centerPieceCount * 3


    //Horizontal scoring
    for (r <- 0 until gridDims.width){
      val row = board(r)
      for (c <- 0 until gridDims.height - 3){
        val moveOutcome = row.slice(c, c + 4)
        score += calculateMoveScore(moveOutcome, player)
      }
    }

    // Vertical scoring
    for (c <- 0 until gridDims.height){
      val column = board.map{_(c)}
      for (r <- 0 until gridDims.width - 3) {
        val moveOutcome = column.slice(r, r + 4)
        score += calculateMoveScore(moveOutcome, player)
      }
    }

    //Diagonal \ scoring
    for (r <- 0 until gridDims.width - 3) {
      for (c <- 0 until gridDims.height - 3) {
        var moveOutcome : Seq[CellType] = Seq()
        for (i <- 0 until 4) moveOutcome = moveOutcome :+ board(c + i)(r + i)
        score += calculateMoveScore(moveOutcome, player)
      }
    }

    //Diagonal / scoring
    for (r <- 0 until gridDims.width - 3) {
      for (c <- 0 until gridDims.height - 3) {
        var moveOutcome: Seq[CellType] = Seq()
        for (i <- 0 until 4) moveOutcome = moveOutcome :+ board(c + i)(r + 3 - i)
        score += calculateMoveScore(moveOutcome, player)
      }
    }
    score
  }

  def getAllowedMoves(board : Seq[Seq[CellType]]) : Seq[Int] = {
    var allowedMoves : Seq[Int] = Seq()
    for (col <- 0 until gridDims.height) {
      if(!isColumnFull(col, board)) allowedMoves = allowedMoves :+ col
    }
    allowedMoves
  }

  def minMax(board : Seq[Seq[CellType]], difficulty : Int, AITurn : Boolean) : (Int,Int) = {
    val allowedMoves = getAllowedMoves(board)
    if (this.checkGameOver(board) | difficulty == 0) {
        if (checkWinner(board, PlayerB())) return (-1, 10000)
        else if (checkWinner(board, PlayerA())) return (-1, -10000)
        else if (fullBoard(board)) return (-1, 0)
        else return (-1, getMoveScore(board, PlayerB()))
      }
    if (AITurn) {
      var bestScore = -10000000
      var bestColumn = allowedMoves(randomGen.randomInt(allowedMoves.length))
      for (col <- allowedMoves) {
          val finalPlace = getFinalPlace(board, Point(col, 0))
          val tempBoard = board.updated(finalPlace.y, board(finalPlace.y).updated(finalPlace.x, PlayerB()))
          val newScore = minMax(tempBoard, difficulty - 1, false)._2
          if (newScore > bestScore) {
            bestScore = newScore
            bestColumn = col
          }
        }
      (bestColumn, bestScore)
    } else {
      var bestScore = 10000000
      var bestColumn = allowedMoves(randomGen.randomInt(allowedMoves.length))
      for (col <- allowedMoves) {
          val finalPlace = getFinalPlace(board, Point(col, 0))
          val tempBoard = board.updated(finalPlace.y, board(finalPlace.y).updated(finalPlace.x, PlayerA()))
          val newScore = minMax(tempBoard, difficulty - 1, true)._2
          if (newScore < bestScore) {
            bestScore = newScore
            bestColumn = col
          }
        }
      (bestColumn, bestScore)
    }
  }

  def computerMove() : GameState = {
    val minMaxTuple = minMax(this.currentBoard, this.difficulty, true)
    val column = minMaxTuple._1
    val p = getFinalPlace(this.currentBoard, Point(column,0))
    val tempBoard = this.currentBoard.updated(p.y, this.currentBoard(p.y).updated(p.x, this.playerColor))
    this.copy(player = p, currentBoard = tempBoard)
  }

}

class GameLogic(val gridDims: Dimensions,
                val initialBoard: Seq[Seq[CellType]],
                val randomGen: RandomGenerator,
                val singleplayer: Boolean,
                val isDifficultyScreen : Boolean,
                val difficulty : Int,
                val isGameRunning: Boolean) {

  var currGameState: GameState = GameState(gridDims, initialBoard, randomGen, singleplayer, isDifficultyScreen, difficulty)

  def gameOver: Boolean = currGameState.isGameOver

  def getWinner: Int = currGameState.winner

  def getCellType(x: Int, y: Int): CellType = {
    currGameState.cellTypeAt(Point(x, y))
  }

  def moveLeft(): Unit = {
    if (!currGameState.isGameOver) currGameState = currGameState.moveLeft()
  }

  def moveRight(): Unit = {
    if (!currGameState.isGameOver) currGameState = currGameState.moveRight()
  }

  def drop(): Unit = {
    if (!currGameState.isColumnFull(currGameState.player.x, currGameState.currentBoard)
      & !currGameState.isGameOver) {
      currGameState = currGameState.moveDown()
      currGameState = currGameState.addPieceToBoard()
      currGameState = currGameState.updateBoardAndSpawnPiece()

      if (currGameState.singleplayer & !currGameState.isGameOver) {
        currGameState = currGameState.computerMove()
        currGameState = currGameState.updateBoardAndSpawnPiece()
      }
    }
  }
}

object GameLogic{
  val MaxWidth = 50
  val MaxHeight = 50

  val DefaultWidth: Int = 7
  val DefaultHeight: Int = 7
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def makeEmptyBoard(gridDims: Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty())
    Seq.fill(gridDims.height)(emptyLine)
  }

  val initialBoard = makeEmptyBoard(DefaultDims)

  val randomGen = new ScalaRandomGen()
}