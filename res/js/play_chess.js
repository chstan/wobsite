var init = function() {
    var showing = false;

    $("#opponent-button").click(function() {
        if (showing) {
            $("#opponent-div").hide();
            $(this).text('See your opponent!');
            showing = false;
        } else {
            $("#opponent-div").show();
            $(this).text('Too intimidating!');
            showing = true;
        }
    });

    var gameState = "ongoing";
    var board,
        game = new Chess();

    // do not pick up pieces if the game is over
    // only pick up pieces for White
    var onDragStart = function(source, piece, position, orientation) {
        if (game.in_checkmate() === true || game.in_draw() === true ||
            game.in_stalemate() === true ||
            game.in_threefold_repetition() === true ||
            piece.search(/^b/) !== -1) {
            return false;
        }
    };

    var makeComputerMove = function() {
        // check for game over
        if (game.moves().length === 0 || game.in_draw() === true ||
            game.in_threefold_repetition() === true) {
            if (gameState === "ongoing") {
                gameState = "finished";
                result = "";
                if (game.in_checkmate() === true) {
                    // server lost
                    result = "loss";
                } else {
                    result = "tie";
                }
                var resultURL = '/chess/result/' + uuid + '/' + result;
                (function notifyServerOfResult() {
                    $.getJSON(resultURL, function(data, textStatus, jqXHR){});
                })();
            }
            return;
        }

        var FEN = game.fen();
        var pollingURL = '/chess/poll';
        (function pollServerForMove() {
            $.getJSON(pollingURL,
                      {'uuid': uuid,
                       'FEN': FEN},
                      function(data, textStatus, jqXHR) {
                          if (data.bestMove) {
                              // insert the dash that chess.js wants
                              var parsedMove;
                              if (data.bestMove.length === 4) {
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
                              if (game.moves().length === 0 || game.in_draw() === true ||
                                  game.in_threefold_repetition() === true) {
                                  if (gameState === "ongoing") {
                                      gameState = "finished";
                                      result = "";
                                      if (game.in_checkmate() === true) {
                                          // server won
                                          result = "win";
                                      } else {
                                          result = "tie";
                                      }
                                      var resultURL = '/chess/result/' + uuid + '/' + result;
                                      (function notifyServerOfResult() {
                                          $.getJSON(resultURL, function(data, textStatus, jqXHR){});
                                      })();
                                  }
                              }
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
