var init = function() {
    var board,
        game = new Chess();

    // do not pick up pieces if the game is over
    // only pick up pieces for White
    var onDragStart = function(source, piece, position, orientation) {
        if (game.in_checkmate() === true || game.in_draw() === true ||
            piece.search(/^b/) !== -1) {
            return false;
        }
    };

    var makeComputerMove = function() {
        var possibleMoves = game.moves();

        // game over
        if (possibleMoves.length === 0) return;

        var FEN = game.fen();
        FEN = FEN.replace(/ /g, "_");
        FEN = FEN.replace(/\//g, "~");
        var pollingURL = '/chess/' + uuid + "/" + FEN;
        (function pollServerForMove() {
            $.getJSON(pollingURL, function(data, textStatus, jqXHR) {
                if (data.bestMove) {
                    // insert the dash that chess.js wants
                    var parsedMove;
                    if (data.bestMove.length == 4) {
                        parsedMove = {
                            from: data.bestMove.slice(0, 2),
                            to: data.bestMove.slice(2, 4)
                        }
                    } else {
                        parsedMove = {
                            from: data.bestMove.slice(0, 2),
                            to: data.bestMove.slice(2, 4),
                            promotion: data.bestMove.slice(4,5)
                        }
                    }
                    game.move(parsedMove);
                    board.position(game.fen());
                } else {
                    setTimeout(pollServerForMove, 1000);
                }

            });
        }());
    };

    var onDrop = function(source, target) {
        // see if the move is legal
        var move = game.move({
            from: source,
            to: target,
            promotion: 'q' // NOTE: always promote to a queen for example simplicity
        });

        // illegal move
        if (move === null) return 'snapback';

        // consult server for computer's move
        makeComputerMove();
    };

    // update the board position after the piece snap
    // for castling, en passant, pawn promotion
    var onSnapEnd = function() {
        board.position(game.fen());
    };

    var cfg = {
        draggable: true,
        position: 'start',
        onDragStart: onDragStart,
        onDrop: onDrop,
        onSnapEnd: onSnapEnd
    };

    board = new ChessBoard('board', cfg);
}; // end init()
$(document).ready(init);
