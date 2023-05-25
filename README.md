# zkat
German Card Game Skat implemented with FP in ZIO prelude and Scala 3.
Skat is like FP, if you get it some time, it's just fun.

# References
- [Skat Wiki (de)](https://de.wikipedia.org/wiki/Skat#)
- [Skat Wiki (en)](https://en.wikipedia.org/wiki/Skat_(card_game))

# Stack
- [ZIO prelude](https://zio.github.io/zio-prelude)
  - [ZPure](https://zio.github.io/zio-prelude/docs/zpure), a monad that combines Writer,Reader,State and IO without monad transformer.
      - to model the game state machine
      - to model the player IO
  - [Subtype](https://zio.github.io/zio-prelude/docs/newtypes/#subtypes) to refine types

- ZIO http
  - to implement the websocket communication 

- ZIO as runtime
