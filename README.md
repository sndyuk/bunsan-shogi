# Scala Shogi Engine
Shogi library for Scala.

### Features
- **Core Shogi Logic:** Full implementation of Shogi rules, including board representation, piece movement, drops, promotion, check, and mate detection.
- **AI Opponent:** Play against a built-in AI.
- **Kifu Parsing:** Support for reading game records in CSA and KI2 formats.
- **Player Management:** Code structure for managing human and AI players.
- **Sample GUI Application:** Includes a basic graphical interface to demonstrate library usage.

### Try sample applications

```sh
$ sbt "project sample" run
```

### Performance
Scala 2.12.6 / 2.9 GHz Intel Core i7

* 700K NPS / 1 thread
* 1,700K NPS / 2 threads
* 2,400K NPS / 4 threads

### Potential Improvements
- **Expanded Kifu Format Support:** Add support for SFEN notation for board positions and potentially other kifu formats.
- **USI Protocol:** Implement the Universal Shogi Interface (USI) for compatibility with standard Shogi GUIs.
- **AI Enhancements:**
    - Configurable AI strength and playing levels.
    - Integration of an opening book.
    - Exploration of more advanced AI techniques.
- **Game Features:**
    - Functionality to save the current game state.
    - Ability to export games to supported kifu formats.
- **Documentation:**
    - Generate and publish detailed API documentation (Scaladoc).
    - Provide more comprehensive usage examples.
- **Testing & Development:**
    - Increase test coverage across the library.
    - Update dependencies and explore modern Scala features.

### Sbt
```scala
"com.sndyuk" %% "bunsan-shogi-core" % "0.1.1"
```

### Maven
```xml
<dependency>
  <groupId>com.sndyuk</groupId>
  <artifactId>bunsan-shogi-core_2.12</artifactId>
  <version>0.1.1</version>
</dependency>
```

### License
[Apache License, Version 2.0](LICENSE)
