package connectFour.logic

import connectFour.game.{CellType, Empty}

object ScoringSystem {

  def calculateMoveScore(move: Seq[CellType], player: CellType): Int = {
    var score = 0
    val pieceCount = move.count(p => p == player)
    val emptyCount = move.count(p => p == Empty())

    if (pieceCount == 4) score += 100 //winning move
    else if (pieceCount == 3 & emptyCount == 1) score += 5 //3 in a row
    else if (pieceCount == 2 & emptyCount == 2) score += 2 //2 in a row

    if (move.count(p => p == player.opposite) == 3 & emptyCount == 1) score -= 4 //opponent 3 in a row move

    score
  }

  def getMoveScore(board: Seq[Seq[CellType]], player: CellType, gridDims: Dimensions): Int = {
    var score = 0

    //Middle column
    val centerColumn = board.map {
      _ (3)
    }
    val centerPieceCount = centerColumn.count(p => p == player)
    score += centerPieceCount * 3


    //Horizontal scoring
    for (r <- 0 until gridDims.width) {
      val row = board(r)
      for (c <- 0 until gridDims.height - 3) {
        val moveOutcome = row.slice(c, c + 4)
        score += calculateMoveScore(moveOutcome, player)
      }
    }

    // Vertical scoring
    for (c <- 0 until gridDims.height) {
      val column = board.map {
        _ (c)
      }
      for (r <- 0 until gridDims.width - 3) {
        val moveOutcome = column.slice(r, r + 4)
        score += calculateMoveScore(moveOutcome, player)
      }
    }

    //Diagonal \ scoring
    for (r <- 0 until gridDims.width - 3) {
      for (c <- 0 until gridDims.height - 3) {
        var moveOutcome: Seq[CellType] = Seq()
        for (i <- 0 until 4) moveOutcome = moveOutcome :+ board(c + i)(r + i)
        score += calculateMoveScore(moveOutcome, player)
      }
    }

    score
  }
}
