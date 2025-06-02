document.addEventListener('DOMContentLoaded', () => {
    const shogiBoardDiv = document.getElementById('shogi-board');
    const senteCapturedDiv = document.getElementById('sente-captured');
    const goteCapturedDiv = document.getElementById('gote-captured');
    const gameStatusDiv = document.getElementById('game-status');
    const newGameBtn = document.getElementById('new-game-btn');

    let selectedPiece = null; // { x, y, pieceType, player (inferred/known) } or { pieceTypeToDrop, player }
    let currentTurn = null;   // Will be 'SENTE' or 'GOTE' (string based on Player enum)
    let boardState = {};      // Map: "x,y" -> "FU" (SimplePieceType string)
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

    async function fetchGameState() {
        try {
            const response = await fetch('/api/game/state');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const gameState = await response.json();
            
            currentTurn = gameState.currentTurn;
            boardState = gameState.boardSetup; // Expects map like {"0,0": "KY", "0,1":"KE", ...} where x,y are 0-indexed
            
            renderBoard(gameState.boardSetup); // boardSetup is Map<Position, SimplePieceType>
            renderCapturedPieces(gameState.capturedPiecesPlayer1, gameState.capturedPiecesPlayer2);
            updateGameStatus(`Turn: ${currentTurn}. History moves: ${gameState.gameHistory.length}`);
            clearHighlights();
            selectedPiece = null;

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

                const pieceKey = `${f},${r}`; // Create key string "x,y"
                const pieceTypeStr = boardSetup[pieceKey]; // e.g., "FU"

                if (pieceTypeStr) {
                    cell.textContent = pieceToDisplay[pieceTypeStr.toUpperCase()] || pieceTypeStr;
                    // TODO: Add player indication (e.g., class for styling Sente/Gote pieces)
                    // This requires player info per piece, not just SimplePieceType.
                    // For now, a placeholder: if (r < 5) cell.classList.add('gote-piece-display');
                }

                cell.addEventListener('click', () => onCellClick(f, r, pieceTypeStr));
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

    async function onCellClick(x, y, pieceTypeStr) {
        clearHighlights(); // Clear previous valid move highlights

        if (selectedPiece) { // A piece or captured piece was already selected
            if (selectedPiece.pieceTypeToDrop) { // Trying to drop a captured piece
                console.log(`Attempting to drop ${selectedPiece.pieceTypeToDrop} at (${x},${y})`);
                makeMoveAttempt(null, { x, y }, false, selectedPiece.pieceTypeToDrop);
            } else { // Second click: trying to move selectedPiece to (x,y)
                console.log(`Attempting to move from (${selectedPiece.x},${selectedPiece.y}) to (${x},${y})`);
                // TODO: Implement promotion logic if applicable
                const promotion = false; // Hardcoded for now
                makeMoveAttempt({ x: selectedPiece.x, y: selectedPiece.y }, { x, y }, promotion, null);
            }
        } else { // First click: selecting a piece on the board
            if (pieceTypeStr) {
                console.log(`Selected piece ${pieceTypeStr} at (${x},${y})`);
                selectedPiece = { x, y, pieceType: pieceTypeStr /* TODO: player? */ };
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
                const pieceKey = `${f},${r}`;
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
            if (!response.ok) {
                throw new Error(result.error || `HTTP error! status: ${response.status}`);
            }
            updateGameStatus(`Move successful. Turn: ${result.currentTurn}.`);
            await fetchGameState(); // Refresh entire state
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
            const response = await fetch('/api/game/new', { method: 'POST' });
            if (!response.ok) {
                const errorResult = await response.json().catch(() => ({error: "Unknown error during new game"}));
                throw new Error(errorResult.error || `HTTP error! status: ${response.status}`);
            }
            const newGameState = await response.json();
            updateGameStatus('New game started. Turn: ' + newGameState.currentTurn);
            await fetchGameState(); // Refresh state
        } catch (error) {
            console.error('Failed to start new game:', error);
            updateGameStatus(`New game error: ${error.message}`);
        }
    });

    function updateGameStatus(message) {
        gameStatusDiv.innerHTML = `<p>${message}</p>`;
    }

    // Initial fetch of game state when page loads
    fetchGameState();
});
