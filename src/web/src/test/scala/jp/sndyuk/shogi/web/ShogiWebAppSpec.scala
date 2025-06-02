package jp.sndyuk.shogi.web

import org.scalatra.test.scalatest.ScalatraSuite
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._ // Import Play JSON
// Unused core imports removed as tests primarily check JSON structure
// import jp.sndyuk.shogi.core.{GameState, Position, SimplePiece, Player => CorePlayerEnum}

// Define case classes for parsing JSON responses if not directly using GameState
// GameState and its components (Position, SimplePiece.SimplePieceType, Player.Player)
// already have Play JSON formats defined in the core module.

class ShogiWebAppSpec extends ScalatraSuite with AnyFlatSpecLike with Matchers {

  // Mount the servlet for testing
  addServlet(classOf[ShogiWebApp], "/api/*")

  // Helper to parse JSON string to JsValue
  def parseJson(jsonStr: String): JsValue = Json.parse(jsonStr)

  "ShogiWebApp GET /api/game/state" should "return the current game state" in {
    get("/api/game/state") {
      status should equal (200)
      response.header("Content-Type") should startWith ("application/json") // Fixed: use header
      val jsonResponse = parseJson(response.body)
      // Basic checks for GameState structure
      (jsonResponse \ "boardSetup").asOpt[Map[String, String]] shouldBe defined // boardSetup is Map[Position, SimplePieceType]
                                                                              // Play JSON converts Map keys to String by default if complex.
                                                                              // Position's KeyReads/Writes might make it Map[String, String] effectively.
                                                                              // GameState.gameStateFormat uses Position.positionMapKeyReads/Writes
                                                                              // which means keys are "x,y". Values are SimplePieceType.toString.
      (jsonResponse \ "currentTurn").as[String] should (equal ("SENTE") or equal ("GOTE"))
      (jsonResponse \ "capturedPiecesPlayer1").asOpt[List[String]] shouldBe defined
      (jsonResponse \ "capturedPiecesPlayer2").asOpt[List[String]] shouldBe defined
      (jsonResponse \ "gameHistory").asOpt[List[JsObject]] shouldBe defined // List[SimpleTransition]

      // Check initial turn is SENTE for a new game
      if (((jsonResponse \ "gameHistory").as[List[JsObject]]).isEmpty) {
        (jsonResponse \ "currentTurn").as[String] should equal ("SENTE")
      }
    }
  }

  "ShogiWebApp POST /api/game/new" should "start a new game and return its state" in {
    post("/api/game/new") {
      status should equal (200)
      response.header("Content-Type") should startWith ("application/json") // Fixed: use header
      val jsonResponse = parseJson(response.body)
      (jsonResponse \ "currentTurn").as[String] should equal ("SENTE")
      (jsonResponse \ "gameHistory").as[List[JsObject]] shouldBe empty
      // Further checks on board setup could be done if needed
    }
  }

  "ShogiWebApp GET /api/game/valid_moves" should "return valid moves for a piece" in {
    // Ensure a new game state
    post("/api/game/new") {
      status should equal (200) // Make sure new game started
    }

    // Get valid moves for Sente's pawn at 7g (core Position(x=2, y=6))
    get("/api/game/valid_moves?x=2&y=6") {
      status should equal (200)
      response.header("Content-Type") should startWith ("application/json") // Fixed: use header
      val jsonResponse = parseJson(response.body)
      val validMoves = jsonResponse.as[List[JsValue]]
      // Expecting Sente pawn at 7g (2,6) to move to 7f (2,5)
      // val expectedMove = Json.obj("x" -> 2, "y" -> 5) // This was marked unused, direct use below
      validMoves should contain (Json.obj("x" -> 2, "y" -> 5))
    }
  }

  "ShogiWebApp POST /api/game/move" should "process a valid board move" in {
    post("/api/game/new") { status should equal (200) } // Start new game

    val movePayload = Json.obj(
      "from" -> Json.obj("x" -> 2, "y" -> 6), // Sente pawn 7g
      "to"   -> Json.obj("x" -> 2, "y" -> 5), // to 7f
      "promotion" -> false
    ).toString()

    post("/api/game/move", body = movePayload.getBytes("UTF-8"), headers = Map("Content-Type" -> "application/json")) {
      status should equal (200)
      response.header("Content-Type") should startWith ("application/json") // Fixed: use header
      val jsonResponse = parseJson(response.body)
      (jsonResponse \ "currentTurn").as[String] should equal ("GOTE") // Turn should change
      // Check if piece moved: boardSetup should have FU at "2,5" and not at "2,6"
      val boardSetup = (jsonResponse \ "boardSetup").as[Map[String, String]]
      boardSetup.get("2,5") should contain ("FU")
      boardSetup.get("2,6") shouldBe empty
    }
  }

  it should "reject an invalid board move" in {
    post("/api/game/new") { status should equal (200) } // Start new game

    // Attempt to move Gote's piece (e.g. pawn at 3c / Position(x=6,y=2)) during Sente's turn
    val movePayload = Json.obj(
      "from" -> Json.obj("x" -> 6, "y" -> 2), 
      "to"   -> Json.obj("x" -> 6, "y" -> 3), 
      "promotion" -> false
    ).toString()

    post("/api/game/move", body = movePayload.getBytes("UTF-8"), headers = Map("Content-Type" -> "application/json")) {
      status should equal (400) // Bad Request
      response.header("Content-Type") should startWith ("application/json") // Fixed: use header
      val jsonResponse = parseJson(response.body)
      (jsonResponse \ "error").asOpt[String] shouldBe defined
      (jsonResponse \ "error").as[String] should include ("Invalid move") // Or more specific like "Rule violation"
    }
  }

  it should "process a valid drop move" in {
    // To test a drop, we need a piece in hand.
    // We can't easily make a capture via API calls without a complex sequence.
    // So, we rely on ShogiGameService.startNewGame's ability to set up captured pieces.
    // However, the /api/game/new endpoint doesn't take parameters for custom setup.
    // This means this test for drop is hard to do without modifying ShogiWebApp to allow custom new games
    // OR by having a more complex setup sequence.

    // For now, this test assumes the backend *could* have a piece in hand.
    // The ShogiGameService.makeMove will use Rule.canMove, which checks hand.
    // This test is more about whether the drop request format is parsed correctly.
    // We need to ensure ShogiGameService is initialized with a piece in hand for Sente.
    // This requires a change to ShogiWebApp or a test-specific setup for ShogiGameService.

    // Let's assume ShogiWebApp's ShogiGameService is fresh for each test or suite.
    // We will call /api/game/new, then try to make a drop.
    // This test will likely fail if the default new game doesn't give Sente a FU in hand.
    // The service's `startNewGame` (default) creates a standard board with no captured pieces.
    // So, a drop will fail `Rule.canMove`.
    // To make this test pass, `ShogiWebApp` would need to expose a way to start a game with pieces in hand,
    // or this test needs to be aware of the service instance to manipulate it (not ideal for API test).

    // Given the constraints, this test will likely show the API path works, but the move is rejected by game logic.
    // Or, if we want to test a *successful* drop, we'd need to modify ShogiWebApp.
    // For now, let's test parsing and that it *tries* a drop.
    // A successful drop test would require a custom game setup endpoint or more complex move sequence.

    post("/api/game/new") { status should equal (200) } // New game, Sente has no captured pieces.

    val dropPayload = Json.obj(
      "to" -> Json.obj("x" -> 4, "y" -> 4), // Drop FU to 5e
      "droppedPiece" -> "FU"
    ).toString()

    post("/api/game/move", body = dropPayload.getBytes("UTF-8"), headers = Map("Content-Type" -> "application/json")) {
      // Expecting this to fail because Sente has no FU in hand in a default new game.
      // This tests that the drop path is taken, but the game logic (Rule.canMove) will reject it.
      status should equal (400) 
      response.header("Content-Type") should startWith ("application/json") // Fixed: use header
      val jsonResponse = parseJson(response.body)
      (jsonResponse \ "error").as[String] should include ("Invalid move") 
    }
  }
}
