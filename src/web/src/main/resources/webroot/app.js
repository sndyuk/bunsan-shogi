document.addEventListener('DOMContentLoaded', () => {
    const shogiBoardDiv = document.getElementById('shogi-board');
    const senteCapturedDiv = document.getElementById('sente-captured');
    const goteCapturedDiv = document.getElementById('gote-captured');
    const gameStatusDiv = document.getElementById('game-status');
    const newGameBtn = document.getElementById('new-game-btn');
    const battleModeSelect = document.getElementById('battle-mode');
    const suggestMoveBtn = document.getElementById('suggest-move-btn');

    let selectedPiece = null; // { x, y, pieceType, player, isPromoted } or { pieceTypeToDrop, player }
    let currentTurn = null;   // Will be 'SENTE' or 'GOTE' (string based on Player enum)
    let currentBattleMode = 'HUMAN_VS_HUMAN'; // Default or read from select
    let boardState = {};      // Map: "x_y" -> PieceInfo { pieceType, player, isPromoted }
    let validMoveHighlights = []; // Store currently highlighted cells for valid moves

    // --- Piece Representation ---
    // Maps SimplePieceType string to a display character (can be expanded)
    const pieceToDisplay = {
        'FU': '歩', 'KY': '香', 'KE': '桂', 'GI': '銀', 'KI': '金', 'KA': '角', 'HI': '飛', 'OU': '玉',
        // Promoted (if backend GameState provided this level of detail, which it doesn't for boardSetup)
        'TO': 'と', 'NY': '杏', 'NK': '圭', 'NG': '全', 'UM': '馬', 'RY': '龍'
    };
    // Player representation for display (e.g., Sente pieces are normal, Gote pieces upside down)
    // This is tricky because boardSetup from backend only gives SimplePieceType, not player or promotion.
    // We'd need to infer player based on position or have more detailed info from backend.
    // For now, this is a placeholder. We'll assume player info can be inferred or is added later.

    const promotablePieceTypes = ['FU', 'KY', 'KE', 'GI', 'KA', 'HI'];

    function isInPromotionZone(y, player) { // y is 0-8
        if (player === 'SENTE') { // Sente promotes in ranks 1-3 (y: 0, 1, 2)
            return y <= 2;
        } else if (player === 'GOTE') { // Gote promotes in ranks 7-9 (y: 6, 7, 8)
            return y >= 6;
        }
        return false;
    }

    async function fetchGameState() {
        try {
            const response = await fetch('/api/game/state');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const gameState = await response.json();

            currentTurn = gameState.currentTurn;
            boardState = gameState.boardSetup; // Expects map like {"0_0": {pieceType:"KY",...}, "0_1":{pieceType:"KE",...}}

            renderBoard(gameState.boardSetup); // boardSetup is Map<String, PieceInfo>
            renderCapturedPieces(gameState.capturedPiecesPlayer1, gameState.capturedPiecesPlayer2);
            updateGameStatus(`Turn: ${currentTurn}. History moves: ${gameState.gameHistory.length}`);
            clearHighlights();
            selectedPiece = null;
            currentBattleMode = battleModeSelect.value; // Ensure mode is current

        } catch (error) {
            console.error('Failed to fetch game state:', error);
            updateGameStatus(`Error: ${error.message}`);
        }
    }

    function renderBoard(boardSetup) {
        shogiBoardDiv.innerHTML = ''; // Clear previous board

        for (let r = 0; r < 9; r++) { // rank (y in core.Position, 0-8)
            for (let f = 0; f < 9; f++) { // file (x in core.Position, 0-8)
                const cell = document.createElement('div');
                cell.classList.add('board-cell');
                cell.dataset.x = f; // Store x (file)
                cell.dataset.y = r; // Store y (rank)

                const pieceKey = `${f}_${r}`; // Create key string "x_y" to match backend
                const pieceInfo = boardSetup[pieceKey];

                if (pieceInfo) {
                    cell.textContent = pieceToDisplay[pieceInfo.pieceType.toUpperCase()] || pieceInfo.pieceType;
                    if (pieceInfo.player === 'SENTE') {
                        cell.classList.add('sente-piece');
                    } else if (pieceInfo.player === 'GOTE') {
                        cell.classList.add('gote-piece');
                    }
                    if (pieceInfo.isPromoted) {
                        cell.classList.add('promoted-piece');
                    }
                }

                cell.addEventListener('click', () => onCellClick(f, r)); // pieceInfo will be retrieved from boardState
                shogiBoardDiv.appendChild(cell);
            }
        }
    }

    function renderCapturedPieces(senteCaptured, goteCaptured) {
        senteCapturedDiv.innerHTML = '<p>Sente\'s Captured:</p>';
        goteCapturedDiv.innerHTML = '<p>Gote\'s Captured:</p>';

        const renderList = (list, container, player) => {
            const counts = {};
            list.forEach(pieceTypeStr => counts[pieceTypeStr] = (counts[pieceTypeStr] || 0) + 1);
            for (const pieceTypeStr in counts) {
                const pieceDiv = document.createElement('div');
                pieceDiv.classList.add('captured-piece');
                pieceDiv.textContent = `${pieceToDisplay[pieceTypeStr.toUpperCase()] || pieceTypeStr} x${counts[pieceTypeStr]}`;
                pieceDiv.dataset.pieceType = pieceTypeStr;
                pieceDiv.dataset.player = player;
                pieceDiv.addEventListener('click', () => onCapturedPieceClick(pieceTypeStr, player));
                container.appendChild(pieceDiv);
            }
        };
        renderList(senteCaptured, senteCapturedDiv, 'SENTE');
        renderList(goteCaptured, goteCapturedDiv, 'GOTE');
    }

    async function onCellClick(x, y) { // pieceTypeStr removed, retrieve from boardState
        clearHighlights(); // Clear previous valid move highlights

        const pieceKey = `${x}_${y}`; // Create key string "x_y" to match backend
        const pieceInfo = boardState[pieceKey];

        if (selectedPiece) { // A piece or captured piece was already selected
            if (selectedPiece.pieceTypeToDrop) { // Trying to drop a captured piece
                console.log(`Attempting to drop ${selectedPiece.pieceTypeToDrop} at (${x},${y})`);
                makeMoveAttempt(null, { x, y }, false, selectedPiece.pieceTypeToDrop);
            } else { // Second click: trying to move selectedPiece to (x,y)
                console.log(`Attempting to move from (${selectedPiece.x},${selectedPiece.y}) to (${x},${y})`);

                let promotionChoice = false; // Default to no promotion

                // Check for promotion possibility (frontend check)
                const pieceType = selectedPiece.pieceType.toUpperCase(); // Ensure uppercase for comparison
                const player = selectedPiece.player; // Assumes selectedPiece has { ..., pieceType: 'FU', player: 'SENTE', ... }

                // Condition 1: Piece type can promote
                const canPiecePromote = promotablePieceTypes.includes(pieceType);
                // Condition 2: Original square OR target square is in promotion zone (standard Shogi rule for optional promotion)
                const isOriginalInZone = isInPromotionZone(selectedPiece.y, player);
                const isTargetInZone = isInPromotionZone(y, player); // 'y' is the target cell's y-coordinate

                if (canPiecePromote && (isOriginalInZone || isTargetInZone)) {
                    // Further check: Is the piece *already* promoted? If so, no further promotion choice.
                    if (!selectedPiece.isPromoted) {
                        // Certain pieces *must* promote if they reach a square from which they cannot move further
                        // (e.g., Pawn or Lance on the last rank, Knight on the last two ranks).
                        // The backend `Rule.canMove` will enforce this. If the frontend offers non-promotion
                        // and it's mandatory, the backend will reject.
                        // For simplicity here, we always ask if promotion is *possible* and *optional*.
                        promotionChoice = confirm(`Promote ${pieceType} to its promoted version?`);
                    }
                }

                makeMoveAttempt({ x: selectedPiece.x, y: selectedPiece.y }, { x: x, y: y }, promotionChoice, null);
            }
        } else { // First click: selecting a piece on the board
            if (pieceInfo) {
                console.log(`Selected piece ${pieceInfo.pieceType} of player ${pieceInfo.player} at (${x},${y}), Promoted: ${pieceInfo.isPromoted}`);
                selectedPiece = { x, y, pieceType: pieceInfo.pieceType, player: pieceInfo.player, isPromoted: pieceInfo.isPromoted };
                document.querySelector(`.board-cell[data-x='${x}'][data-y='${y}']`).classList.add('selected');
                fetchValidMoves({ x, y });
            }
        }
    }

    function onCapturedPieceClick(pieceType, player) {
        // TODO: Check if it's this player's turn
        clearHighlights();
        console.log(`Selected captured piece ${pieceType} for player ${player} to drop.`);
        selectedPiece = { pieceTypeToDrop: pieceType, player: player };
        // Highlight selected captured piece (optional)
        // Display valid drop locations (all empty squares)
        displayValidDropLocations();
    }

    function clearHighlights() {
        document.querySelectorAll('.board-cell.selected').forEach(c => c.classList.remove('selected'));
        validMoveHighlights.forEach(cell => cell.classList.remove('valid-move'));
        validMoveHighlights = [];
        document.querySelectorAll('.captured-piece.selected').forEach(c => c.classList.remove('selected'));
    }

    async function fetchValidMoves(fromPos) {
        try {
            const response = await fetch(`/api/game/valid_moves?x=${fromPos.x}&y=${fromPos.y}`);
            if (!response.ok) throw new Error(`HTTP error! status: ${response.status}`);
            const validMoves = await response.json(); // Expects List[Position]

            validMoves.forEach(pos => {
                const cell = document.querySelector(`.board-cell[data-x='${pos.x}'][data-y='${pos.y}']`);
                if (cell) {
                    cell.classList.add('valid-move');
                    validMoveHighlights.push(cell);
                }
            });
        } catch (error) {
            console.error('Failed to fetch valid moves:', error);
            updateGameStatus(`Error fetching valid moves: ${error.message}`);
        }
    }

    function displayValidDropLocations() {
        // For drops, all empty squares are potential targets.
        for (let r = 0; r < 9; r++) {
            for (let f = 0; f < 9; f++) {
                const pieceKey = `${f}_${r}`; // Create key string "x_y" to match backend
                if (!boardState[pieceKey]) { // If cell is empty
                    const cell = document.querySelector(`.board-cell[data-x='${f}'][data-y='${r}']`);
                    if (cell) {
                        cell.classList.add('valid-move'); // Use same highlight for now
                        validMoveHighlights.push(cell);
                    }
                }
            }
        }
    }

    async function makeMoveAttempt(fromPos, toPos, promotion, droppedPieceTypeStr) {
        let requestBody;
        let endpoint = '/api/game/move';

        if (droppedPieceTypeStr) {
            requestBody = JSON.stringify({
                to: { x: toPos.x, y: toPos.y },
                droppedPiece: droppedPieceTypeStr
            });
        } else {
            requestBody = JSON.stringify({
                from: { x: fromPos.x, y: fromPos.y },
                to: { x: toPos.x, y: toPos.y },
                promotion: promotion
            });
        }

        try {
            const response = await fetch(endpoint, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: requestBody
            });
            const result = await response.json(); // Expects GameState or error JSON
            // if (!response.ok) { // This check is moved down
            //     throw new Error(result.error || `HTTP error! status: ${response.status}`);
            // }
            // updateGameStatus(`Move successful. Turn: ${result.currentTurn}.`); // Original line
            // await fetchGameState(); // Original line, DELAY THIS

            // NEW LOGIC:
            if (response.ok) { // Check if human move was successful
                // Update currentTurn based on the response from the human's move
                // before checking if it's AI's turn.
                currentTurn = result.currentTurn;
                updateGameStatus(`Move successful. Turn: ${currentTurn}.`);

                // Now, if it's AI's turn, let AI make a move
                if (currentBattleMode === 'HUMAN_VS_AI') {
                    await requestAIMove(); // This will fetch game state internally if AI moves
                } else {
                    await fetchGameState(); // If not AI's turn, or not AI mode, just refresh
                }
            } else { // Human move failed
                throw new Error(result.error || `HTTP error! status: ${response.status}`);
            }
        } catch (error) {
            console.error('Failed to make move:', error);
            updateGameStatus(`Move failed: ${error.message}`);
        } finally {
            clearHighlights();
            selectedPiece = null;
        }
    }

    newGameBtn.addEventListener('click', async () => {
        try {
            // Update currentBattleMode from the UI selector *before* sending the request
            currentBattleMode = battleModeSelect.value;

            let requestBody = {};
            if (currentBattleMode === 'HUMAN_VS_AI') {
                requestBody.gameMode = 'hva_gote'; // Human Sente, AI Gote
                // Optionally, specify AI type and depth if desired from frontend
                // requestBody.aiType = "v2";
                // requestBody.aiSearchDepth = 3;
            } else { // HUMAN_VS_HUMAN
                requestBody.gameMode = 'hvh';
            }

            const response = await fetch('/api/game/new', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }, // Needed when sending a JSON body
                body: JSON.stringify(requestBody) // Send the gameMode
            });

            if (!response.ok) {
                const errorResult = await response.json().catch(() => ({error: "Unknown error during new game"}));
                throw new Error(errorResult.error || `HTTP error! status: ${response.status}`);
            }
            const newGameState = await response.json();

            // currentBattleMode is already updated from selector value above
            updateGameStatus(`New game started. Mode: ${currentBattleMode}. Turn: ${newGameState.currentTurn}`);
            await fetchGameState(); // Refresh state (this will also update currentTurn from gameState)
        } catch (error) {
            console.error('Failed to start new game:', error);
            updateGameStatus(`New game error: ${error.message}`);
        }
    });

    function updateGameStatus(message) {
        gameStatusDiv.innerHTML = `<p>${message}</p>`;
    }

    async function requestAIMove() {
        if (currentTurn === null) {
            console.log("Cannot request AI move, current turn is null.");
            return;
        }
        // Determine which player is AI based on currentBattleMode and currentTurn
        // For now, assuming Sente is Human and Gote is AI in HUMAN_VS_AI mode
        // This logic might need adjustment if player roles can be swapped.
        const isAIsTurn = (currentBattleMode === 'HUMAN_VS_AI' && currentTurn === 'GOTE'); // Example: Gote is AI

        if (!isAIsTurn) {
            console.log("Not AI's turn or not in AI mode.");
            return;
        }

        updateGameStatus(`AI (${currentTurn}) is thinking...`);
        try {
            // This endpoint will be created in a later step
            const response = await fetch('/api/game/ai_move', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                // body: JSON.stringify({ player: currentTurn }) // Backend can infer player from game state
            });
            const result = await response.json();
            if (!response.ok) {
                throw new Error(result.error || `HTTP error! status: ${response.status}`);
            }
            updateGameStatus(`AI (${currentTurn}) moved. Turn: ${result.currentTurn}.`);
            await fetchGameState(); // Refresh entire state after AI move
        } catch (error) {
            console.error('AI move failed:', error);
            updateGameStatus(`AI move error: ${error.message}. Turn: ${currentTurn}.`);
            // Potentially allow human to retry or handle error
        }
    }

    // Initial fetch of game state when page loads
    fetchGameState();

    battleModeSelect.addEventListener('change', (event) => {
        currentBattleMode = event.target.value;
        console.log(`Battle mode changed to: ${currentBattleMode}`);
        // Optionally, reset the game or update status message
        updateGameStatus(`Battle mode set to ${currentBattleMode}. Turn: ${currentTurn}.`);
        // If changing mode mid-game, consider if an AI move should be triggered immediately
        // if it's now AI's turn. For simplicity, this might only take full effect on new game or next turn.
    });

    suggestMoveBtn.addEventListener('click', async () => {
        if (currentTurn === null) {
            updateGameStatus("Game not active or turn is unclear. Cannot suggest a move.");
            console.log("Cannot suggest move, current turn is null.");
            return;
        }

        // Prevent suggesting moves if it's AI's turn in HUMAN_VS_AI mode,
        // as the AI will move automatically.
        // This check assumes Sente is Human and Gote is AI in HUMAN_VS_AI.
        // Adjust if player roles can be different.
        const isAIsActualTurn = (currentBattleMode === 'HUMAN_VS_AI' && currentTurn === 'GOTE');
        if (isAIsActualTurn) {
            updateGameStatus("AI is about to move. Suggestion not available now.");
            return;
        }

        updateGameStatus(`Requesting suggested move for ${currentTurn}...`);
        try {
            // This endpoint will be created in a later step.
            // It should return a move object like { from: {x,y}, to: {x,y}, promotion: boolean, pieceType: "FU" }
            // The backend should determine whose turn it is from the game state.
            const response = await fetch('/api/game/suggest_move', { // MODIFIED LINE
                method: 'GET' // MODIFIED LINE
                // Query parameters can be added here if needed: e.g. /api/game/suggest_move?aiType=v2
            });

            if (!response.ok) {
                const errorResult = await response.json().catch(() => ({error: "Unknown error suggesting move"}));
                throw new Error(errorResult.error || `HTTP error! status: ${response.status}`);
            }

            const suggestedMove = await response.json(); // Expects { from: {x,y}, to: {x,y}, pieceType: "FU", promotion: boolean } or similar

            if (suggestedMove && suggestedMove.from && suggestedMove.to) {
                updateGameStatus(`Suggested move for ${currentTurn}: ${suggestedMove.pieceType} from (${suggestedMove.from.x},${suggestedMove.from.y}) to (${suggestedMove.to.x},${suggestedMove.to.y})${suggestedMove.promotion ? ' (promote)' : ''}.`);
                console.log('Suggested move:', suggestedMove);

                // Highlight the suggested move
                clearHighlights(); // Clear any previous selections or highlights

                // Highlight 'from' cell
                const fromCell = document.querySelector(`.board-cell[data-x='${suggestedMove.from.x}'][data-y='${suggestedMove.from.y}']`);
                if (fromCell) {
                    fromCell.classList.add('selected'); // Use 'selected' style for 'from'
                    validMoveHighlights.push(fromCell); // Add to list to be cleared later
                }

                // Highlight 'to' cell
                const toCell = document.querySelector(`.board-cell[data-x='${suggestedMove.to.x}'][data-y='${suggestedMove.to.y}']`);
                if (toCell) {
                    toCell.classList.add('valid-move'); // Use 'valid-move' style for 'to'
                    validMoveHighlights.push(toCell);
                }

                // Store the suggestion if you want the user to click to execute it,
                // or just display it. For now, it's just a display.
                // If the user clicks another piece, these highlights will clear.
            } else {
                updateGameStatus(`No suggestion available or invalid suggestion format for ${currentTurn}.`);
                console.log('Received invalid suggestion:', suggestedMove);
            }

        } catch (error) {
            console.error('Failed to get suggested move:', error);
            updateGameStatus(`Error suggesting move: ${error.message}`);
        }
    });
});
