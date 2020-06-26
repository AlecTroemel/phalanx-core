An implimentation of a board game that may or may not have existed during the Hellenistic period. Written in [Fennel](https://fennel-lang.org).

## Rules
__To Win the Game:__ Either remove all opponents pieces (hoplites) from the board or connect to your goal. The game ends immediately when one of these conditions are met.

__Required Pieces:__
- A 9x9 plain grid of 9 horizontal and 9 vertical lines (Go Board)
- 9 black and white stones each
- 1 black and white Goal (can be a unique piece or a 10th stone)

__Setup:__ For each player place 4 stones in the shape of a square in adjacent corners of the board 1 intersection away from the wall. These are your starting hoplites. Place 1 stone each in the opponent's corner (behind the square); This is your goal.

__Life & Death:__ 2 hoplite are connected if they share an adjacent intersection. 2 or more connected hoplites are considered a living phalanx. A hoplite is dead if they are not connected to another hoplite, or if they are pushed off the board (more below). Dead hoplites are immediately removed from the board.

__Flow of Play:__ Black takes the first turn, then players alternate back and forth. A turn consists of doing 0, 1 or 2 of the following actions (you may do the same action twice).
- __MOVE:__ move up to 2 hoplites to an adjacent intersection touching their army. You may split or merge phalanx, but hoplites may not jump between unconnected phalanx.
- __RENFORCE/ADD:__ If you have less than 9 hoplites on the board, add a new hoplite to an existing phalanx
- __PUSH:__ if your phalanx is connected to an opponent's, and the number of your hoplites is greater then the opponents in a straight line, and there is an open intersection on the other side of the line, you may slide the whole line towards the opponent's hoplites 1 intersection. Hoplites pushed off the board are dead.

## External Libraries
- [love2d](https://love2d.org/)
- [lume](https://github.com/rxi/lume)
- [lua-state-machine](https://github.com/kyleconroy/lua-state-machine)
- [inspect.lua](https://github.com/kikito/inspect.lua)
