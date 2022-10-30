## Game description

This is an implementation of the game "Connect Four" in Scala. 
The goal in the game is to form a line of four pieces in the same color. 
Any line - horizontal, vertical or diagonal guarantees a win. 
If the board is filled and neither of the players managed to form a line of 4 the game ends in a draw.
The player can see where their piece will fall in the board. 
Use `arrows` to move and `space` to drop the piece.
To quit the game press `q`, to restart (go back to menu) press `r`.

The game has two modes: singleplayer and multiplayer. 

In multiplayer two players can play against each other taking turns.

In singleplayer you play against an AI, there are 3 different difficulties of the game to choose from. 
There is a move-scoring system implemented in `src/connectFour/logic/ScoringSystem`. 
Each of the possible moves on the current board has a different score dependent on the following factors:
* +100 for a winning move
* +3 for each of the pieces of the same color placed in the middle column
* +5 for each 3-in-a-row 
* +2 for each 2-in-a-row 
* -4 for each opponent 3-in-a-row

This scoring system values can be changed to adjust the behaviour of the AI. 

The AI chooses the best move using on the scoring system and minMax algorithm 
with additional insight according to the game difficulty as follows:
* In easy-mode - doesn't take the opponent's moves into account
* In medium-mode - takes the next opponent's move into account
* In hard-mode - takes next 4 moves in the game into account





## How to run

In terminal type `./gradlew run` to start the game or press the play button in `ConnectFourGame` if using IntelliJ IDEA.


