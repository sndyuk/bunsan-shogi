# Scala Shogi Engine
Shogi library for Scala.

### Features
- **Core Shogi Logic:** Full implementation of Shogi rules, including board representation, piece movement, drops, promotion, check, and mate detection.
- **AI Opponent:** Play against a built-in AI.
- **Kifu Parsing:** Support for reading game records in CSA and KI2 formats.
- **Player Management:** Code structure for managing human and AI players.
- **Sample GUI Application:** Includes a basic graphical interface to demonstrate library usage.

### Running the Web Application

The project includes a web-based interface for playing Shogi. To run the web application:

1.  Ensure you have JDK 11 and sbt installed as per the "Building from Source" section.
2.  Open your terminal or command prompt.
3.  Navigate to the root directory of the project.
4.  Run the following command to start the web server:
    ```sh
    sbt "web/runMain jp.sndyuk.shogi.web.WebServer"
    ```
5.  Once the server has started (you should see log messages indicating it's running on port 8080), open a web browser and navigate to:
    `http://localhost:8080/`

This will load the Shogi web application.

**Note on the Original Sample GUI:** The original Swing-based sample GUI application (from the `sample` module) is currently disabled in the `build.sbt` file due to refactoring work and tooling issues encountered during its update. The primary interface for this version is the web application.

### Running Sample Applications

The `sample` module contains command-line applications that demonstrate how to use the Shogi library programmatically. While the Web Application is the primary way to play a full game, these samples offer insights into specific functionalities.

#### `AIBattleSim.scala`

This application demonstrates an AI vs. AI simulation. It showcases:
*   How to configure different AI types (e.g., `AlphaBetaAI_V1`, `AlphaBetaAI_V2`).
*   How to set parameters like search depth for the AIs.
*   Detailed game progress output, including AI thinking time, Nodes Per Second (NPS), moves made, and game results (checkmate, stalemate, etc.).

To run the AI Battle Simulation:
```sh
sbt "sample/runMain jp.sndyuk.shogi.ui.AIBattleSim"
```
You can modify the AI types and their respective search depths directly within the `src/sample/src/main/scala/jp/sndyuk/shogi/ui/AIBattleSim.scala` file. Look for the AI instantiation section, where comments guide you on how to make these changes.

#### `Console.scala`

This application provides a console-based Shogi game, typically setting up a Human player against an AI player. It demonstrates:
*   How a human player can input moves using Shogi algebraic notation.
*   Basic interaction between human and AI players.

To run the Console Game:
```sh
sbt "sample/runMain jp.sndyuk.shogi.ui.Console"
```
The application will prompt you for moves. The expected input format is `(file,rank)(file,rank)[+]` (e.g., `(7,7)(7,6)` to move a piece from file 7, rank 7 to file 7, rank 6, or `(2,2)(2,1)+` for a move with promotion). Refer to the application's startup message for detailed format instructions.
You can modify the player types (e.g., Human vs. Human, AI vs. AI, or different AI for Player B) by editing the `src/sample/src/main/scala/jp/sndyuk/shogi/ui/Console.scala` file, as indicated by comments in the player instantiation section.

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
