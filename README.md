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

### Building from Source

### Environment Setup

To build and develop this project, you'll need the following:

*   **Java Development Kit (JDK):** Version 11. The project is configured to use Temurin 11.0.17, but other JDK 11 distributions should also work.
*   **Scala:** Version 2.12.18.
*   **sbt (Simple Build Tool):** Version 1.9.9.

A convenient way to manage Java, Scala, and sbt versions is [SDKMAN!](https://sdkman.io/).
*   You can install SDKMAN! by following the instructions on their website.
*   Once SDKMAN! is installed, you can often install the required Java version with `sdk install java 11.0.17-tem`.
*   The project includes an `.sdkmanrc` file. If you have SDKMAN! installed and configured with `sdkman_auto_env=true`, navigating into the project directory should automatically select or prompt you to install the correct Java version.
*   You can install sbt via SDKMAN! (`sdk install sbt <version>`) or by following the official [sbt installation guide](https://www.scala-sbt.org/download.html). Scala is typically managed by sbt itself based on the project's `build.sbt` settings.

### Running Tests

The project includes a comprehensive suite of unit tests. To execute these tests:

1.  Open your terminal or command prompt.
2.  Navigate to the root directory of the project.
3.  Run the following command:

    ```sh
    sbt test
    ```

This command will compile the project (if necessary) and then run all tests defined in the `src/core/src/test/scala` and `src/sample/src/test/scala` directories. The output will indicate the number of tests run and whether they passed or failed.

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
