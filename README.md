An implimentation of my board game (rules to come). Written in [Fennel](https://fennel-lang.org).

## TODO
- [x] display board
- [x] state machine
- [x] select action
- [x] add action
- [x] push action
  - [x] restrict to army
- [x] "dead rule", when a piece is alone or off the board it is dead and remove
- [x] Push
- [ ] win conditions
  - [ ] when enemy pieces are all gone
  - [ ] when connect to temple
- [ ] undo (up to the beginning of your turn? or just beginning of action?)
- [ ] AI
- [ ] Menu
- [ ] Tutorial


## External Libraries
- [love2d](https://love2d.org/)
- [lume](https://github.com/rxi/lume)
- [lua-state-machine](https://github.com/kyleconroy/lua-state-machine)
- [inspect.lua](https://github.com/kikito/inspect.lua)
