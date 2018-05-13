(function () {
  "use strict";

  var message = document.getElementById("message");
  // var state_string = document.getElementById("state_string"); // only used for debugging

  var play_again = function() {
    window.location.reload(true);
  };

  var prolog_string2list = function(prolog_string) {
    prolog_string = prolog_string.replace(/(\w+)(\()/g, "$2$1,").replace(/(\w+)/g, "\"$1\"");
    prolog_string = prolog_string.replace(/\(/g, "[").replace(/\)/g, "]");
    return JSON.parse(prolog_string);
  };

  // only used for testing, may be unnecessary
  var list2prolog_string = function(list) {
    var prolog_string = JSON.stringify(list).replace(/"/g, "").slice(1,-1);
    prolog_string = prolog_string.replace(/\[(\w+)\,/g, "$1(").replace(/\]/g, ")");
    return "[" + prolog_string + "]";  // maybe drop brackets since this would be used to return move
  };

  function Cell(board, row, column, colour) {
    // count from 0, not 1
    this.board = board;
    this.row = row;
    this.column = column;
    this.colour = colour;
    this.x = column * this.board.SQUARE_LENGTH;
    this.y = row * this.board.SQUARE_LENGTH;
    this.l = this.board.SQUARE_LENGTH;
  }

  Cell.prototype.draw = function(colour) {
    this.board.context.fillStyle = colour;
    this.board.context.fillRect(this.x, this.y, this.l, this.l);
  };

  function Piece(board, row, column, type, colour) {
    this.board = board;
    this.row = row;
    this.column = column;
    this.type = type;
    this.colour = colour;
    this.x_centre = (column * this.board.SQUARE_LENGTH) + (0.5 * this.board.SQUARE_LENGTH);
    this.y_centre = (row * this.board.SQUARE_LENGTH) + (0.5 * this.board.SQUARE_LENGTH);
  }

  Piece.prototype.draw = function(colour) {
    var halfsize = 0.4 * this.board.SQUARE_LENGTH;
    this.board.context.strokeStyle = colour;
    this.board.context.lineWidth = this.board.BAR_WIDTH;
    if (this.type === "x") {
      this.board.context.moveTo(this.x_centre - halfsize, this.y_centre - halfsize);
      this.board.context.lineTo(this.x_centre + halfsize, this.y_centre + halfsize);
      this.board.context.moveTo(this.x_centre + halfsize, this.y_centre - halfsize);
      this.board.context.lineTo(this.x_centre - halfsize, this.y_centre + halfsize);
      this.board.context.stroke();
    }
    if (this.type === "o") {
      this.board.context.beginPath();
      this.board.context.arc(this.x_centre, this.y_centre, halfsize, 0, Math.PI * 2, true);
      this.board.context.closePath();
      this.board.context.stroke();
    }
  };

  function Board(game) {
    this.DIM = 3;
    this.game = game;
    this.canvas = document.getElementById("board");
    this.context = this.canvas.getContext("2d");
    this.canvas.onclick = this.mark.bind(this); // breaks without bind(this)
    this.canvas.onmousemove = this.get_highlight.bind(this);
    this.SQUARE_LENGTH = Math.floor(this.canvas.width / this.DIM);
    this.BAR_WIDTH = 5;
    this.highlight = [];
    this.set_cells(); // reference as this.cells
    this.set_pieces(); // reference as this.pieces
    this.set_clickables(); // this.clickables
  }

  Board.prototype.set_cells = function() {
    var cells = [];
    var board = this;
    for (var row = 0; row < this.DIM; row++) {
      for (var column = 0; column < this.DIM; column++) {
        // draw a checker board
        if (((row % 2 === 0) && (column % 2 === 0)) || ((row % 2 === 1) && (column % 2 === 1))) { 
          cells.push(new Cell(board, row, column, "ghostwhite"));
        } else {
          cells.push(new Cell(board, row, column, "silver"));
        }
      }
    }
    this.cells = cells;
  };

  Board.prototype.set_pieces = function() {
    var pieces = [];
    var board = this;
    var state_list = prolog_string2list(this.game.state);
    state_list.forEach(function(term) {
      if ((term[0] === "cell") && (term[3] !== "b")) {
        pieces.push(new Piece(board, term[1] - 1, term[2] - 1, term[3], "black"));
      }
    });
    this.pieces = pieces;
  };
  
  Board.prototype.set_clickables = function() { // wrong, should use legals
    var board = this;
    this.clickables = "";
    var state_list = prolog_string2list(this.game.state);
    state_list.forEach(function(term) {
      if ((term[0] === "cell") && (term[3] === "b")) {
        board.clickables = board.clickables + "(" + String(term[1] - 1) + "," + String(term[2] - 1) + ")";
      }
    });
  };

  Board.prototype.is_clickable = function(row, column) {
    var row_col_pair = "(" + String(row) + "," + String(column) + ")";
    return (this.clickables.indexOf(row_col_pair) !== -1);
  };

  Board.prototype.mark = function (event) {
    if ((this.game.active_players.includes(this.game.human_player)) && (this.game.life_stage === "underway")) {
      // need to check this is a legal move
      var rect = this.canvas.getBoundingClientRect();
      var row = Math.floor((event.clientY - rect.top) / this.SQUARE_LENGTH);
      var column = Math.floor((event.clientX - rect.left) / this.SQUARE_LENGTH);
      if (this.is_clickable(row, column)) {
        var move = "does(" + this.game.human_player + ",mark(" + String(row + 1) + "," + String(column + 1) + "))";
        if (this.game.moves_list === "") {
          this.game.moves_list = move;
        } else {
          this.game.moves_list = this.game.moves_list + "," + move;
        }
        var send_str = "state=" + encodeURIComponent(this.game.state) +
          "&aiplayer=" + encodeURIComponent(this.game.ai_player) +
          "&moves=" + encodeURIComponent("[" + this.game.moves_list + "]");
        this.game.server_call(send_str);
        // stop player moving again this turn
        this.game.active_players.splice(this.game.active_players.indexOf(this.game.human_player), 1);
        // clear highlights
        this.highlight = [];
        // redraw board with mark
        if (this.game.human_player === "white") {
          this.pieces.push(new Piece(this, row, column, "x", "black"));
        } else {
          this.pieces.push(new Piece(this, row, column, "o", "black"));
        }
        this.draw();
      }
    } // else "not your turn"
  };
  
  Board.prototype.get_highlight = function (event) {
    if ((this.game.active_players.includes(this.game.human_player)) && (this.game.life_stage === "underway")) {
      var rect = this.canvas.getBoundingClientRect();
      var row = Math.floor((event.clientY - rect.top) / this.SQUARE_LENGTH);
      var column = Math.floor((event.clientX - rect.left) / this.SQUARE_LENGTH);
      // need to check if cell in legals moves
      if (this.is_clickable(row, column)) {
        this.highlight = [row, column];
        // this.draw();
      } else {
        this.highlight = [];
      }
    } else {
      this.highlight = [];
    }
    this.draw();
  };

  Board.prototype.draw = function () {
    if (this.game.life_stage === "start") {
      var that = this.game;
      message.innerHTML = 'Would you like to play <input id="white" type="button" value="X">' + 
        'or <input id="black" type="button" value="O">?';
      document.getElementById("white").onclick = function () {that.set_player("white");};
      document.getElementById("black").onclick = function () {that.set_player("black");};
    }
    this.cells.forEach(function(cell) {
      cell.draw(cell.colour);
    });
    if (this.highlight !== []) {
      var board = this;
      this.cells.forEach(function(cell) {
        if ((cell.row === board.highlight[0]) && (cell.column === board.highlight[1])) {
          cell.draw("yellow");
        }
      });      
    }
    this.pieces.forEach(function(piece) {
      piece.draw(piece.colour);
    });
  };

  function Game() {
    this.init = "[control(white),cell(1,1,b),cell(1,2,b),cell(1,3,b),cell(2,1,b),cell(2,2,b),cell(2,3,b),cell(3,1,b),cell(3,2,b),cell(3,3,b)]";
    this.legals = "[does(white,mark(1,1)),does(white,mark(1,2)),does(white,mark(1,3))," + 
      "does(white,mark(2,1)),does(white,mark(2,2)),does(white,mark(2,3)),does(white,mark(3,1)),does(white,mark(3,2)),does(white,mark(3,3))]";
    this.state = this.init; // use init to reset to new game
    this.moves_list = "";
    this.roles = ["white", "black"];
    this.set_active_players(); // reference as this.active_players
    this.white_reward = 50;
    this.black_reward = 50;
    this.life_stage = "start"; // make this "start", "underway", "over"
    this.board = new Board(this);
    this.board.draw();
  }

  Game.prototype.set_player = function (player) {
    if (player === "white") {
      this.human_player = "white";
      this.ai_player = "black";
      message.textContent = "You to play";
    } else {
      this.human_player = "black";
      this.ai_player = "white";
      // get server to return opening move
      var send_str = "state=" + encodeURIComponent(this.state) +
          "&aiplayer=" + encodeURIComponent(this.ai_player) +
          "&moves=" + encodeURIComponent("[noop]"); // bug needs fixing
      this.server_call(send_str);
    }
    this.life_stage = "underway";
  };

  Game.prototype.set_active_players = function() {
    var state_list = prolog_string2list(this.state);
    var active_players = [];
    state_list.forEach(function(term) {
      if (term[0] === "control") {
        active_players.push(term[1]);
      }
    });
    this.active_players = active_players;
  };

  Game.prototype.make_move = function (response) {
    response = response.split("&");
    this.state = response[0].split("=")[1];
    this.legals = response[1].split("=")[1];
    this.white_reward = response[2].split("=")[1];
    this.black_reward = response[3].split("=")[1];
    if (response[4].split("=")[1] === "true") {
      this.life_stage = "over";
    } else {
      this.life_stage = "underway";
      message.textContent = "You to play";
    }
    var move = response[5].split("=")[1];
    if (move !== "noop") {
      if (this.moves_list === "") {
        this.moves_list = move;
      } else {
        this.moves_list = this.moves_list + "," + move;
      }
    }
    this.set_active_players();
    this.board.set_pieces();
    this.board.set_clickables();
    this.board.draw();
    if (this.life_stage === "over") {
      message.innerHTML = "Game Over! X " + this.white_reward + ", Y " + this.black_reward + 
        ' <input id="game_over" type="button" value="Play Again?">';
      document.getElementById("game_over").onclick = function () {play_again();};
    }
  };

  Game.prototype.server_response = function () {
    if (this.httpRequest.readyState === XMLHttpRequest.DONE) {
      if (this.httpRequest.status === 200) {
        this.make_move(this.httpRequest.responseText);      
      } else {
        alert("There was a problem with the request.");
      }
    }
  };

  Game.prototype.server_call = function (send_str) {
    message.textContent = "Computer is thinking...";
    this.httpRequest = new XMLHttpRequest();
    if (!this.httpRequest) {
      alert("Giving up :( Cannot create an XMLHTTP instance");
      return false;
    }
    this.httpRequest.onreadystatechange = this.server_response.bind(this);
    this.httpRequest.open("POST", "/move");
    this.httpRequest.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
    this.httpRequest.send(send_str);
  };

var game = new Game();

}());

