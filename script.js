(function () {
    "use strict";
    var movetxt = document.querySelector("#move");
    var statetxt = document.querySelector("#state");
    var rewardtxt = document.querySelector("#reward");
    var terminaltxt = document.querySelector("#terminal");

    function Cell(board, column, row, colour) {
        this.board = board;
        this.x = this.board.COLUMNS.indexOf(column) * this.board.SQUARE_LENGTH;
        this.y = this.board.ROWS.indexOf(row) * this.board.SQUARE_LENGTH;
        this.l = this.board.SQUARE_LENGTH;
        this.colour = colour;
        // attaching mouse events to this doesn't work
    }

    Cell.prototype.draw = function (colour) {
        this.board.context.fillStyle = colour;
        this.board.context.fillRect(this.x, this.y, this.l, this.l);
    };

    function Piece(board, column, row, counter) {
        this.board = board;
        this.column = column;
        this.row = row;
        this.counter = counter;
        if (counter === "wp") {
            this.colour = "firebrick";
        }
        if (counter === "wk") {
            this.colour = "tomato";
        }
        if (counter === "bp") {
            this.colour = "darkslategray";
        }
        if (counter === "bk") {
            this.colour = "black";
        }
        this.x = (this.board.COLUMNS.indexOf(column) * this.board.SQUARE_LENGTH) + (this.board.SQUARE_LENGTH / 2);
        this.y = (this.board.ROWS.indexOf(row) * this.board.SQUARE_LENGTH) + (this.board.SQUARE_LENGTH / 2);
        this.radius = 0.4 * this.board.SQUARE_LENGTH;
    }

    Piece.prototype.draw = function () {
        this.board.context.fillStyle = this.colour;
        this.board.context.strokeStyle = this.colour;
        this.board.context.setLineDash([]);
        this.board.context.lineWidth = 1;
        this.board.context.beginPath();
        this.board.context.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
        this.board.context.closePath();
        this.board.context.fill();
        this.board.context.stroke();
    };

    Piece.prototype.oldpos_draw = function () {
        this.board.context.strokeStyle = this.colour;
        this.board.context.setLineDash([6, 3]);
        this.board.context.lineWidth = 3;
        this.board.context.beginPath();
        this.board.context.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
        this.board.context.closePath();
        this.board.context.stroke();
    };

    function Board(game) {
        var black_square = "silver";
        var white_square = "ghostwhite";
        this.game = game;
        this.canvas = document.getElementById("board");
        this.context = this.canvas.getContext("2d");
        this.canvas.onclick = this.select.bind(this); // breaks without bind(this)
        this.canvas.onmousemove = this.highlight.bind(this);
        this.SQUARE_LENGTH = Math.floor(this.canvas.width / 8);
        this.COLUMNS = ["a", "b", "c", "d", "e", "f", "g", "h"];
        this.ROWS = ["8", "7", "6", "5", "4", "3", "2", "1"];
        this.grid = {};
        this.grid.a8 = new Cell(this, "a", "8", white_square);
        this.grid.a7 = new Cell(this, "a", "7", black_square);
        this.grid.a6 = new Cell(this, "a", "6", white_square);
        this.grid.a5 = new Cell(this, "a", "5", black_square);
        this.grid.a4 = new Cell(this, "a", "4", white_square);
        this.grid.a3 = new Cell(this, "a", "3", black_square);
        this.grid.a2 = new Cell(this, "a", "2", white_square);
        this.grid.a1 = new Cell(this, "a", "1", black_square);
        this.grid.b8 = new Cell(this, "b", "8", black_square);
        this.grid.b7 = new Cell(this, "b", "7", white_square);
        this.grid.b6 = new Cell(this, "b", "6", black_square);
        this.grid.b5 = new Cell(this, "b", "5", white_square);
        this.grid.b4 = new Cell(this, "b", "4", black_square);
        this.grid.b3 = new Cell(this, "b", "3", white_square);
        this.grid.b2 = new Cell(this, "b", "2", black_square);
        this.grid.b1 = new Cell(this, "b", "1", white_square);
        this.grid.c8 = new Cell(this, "c", "8", white_square);
        this.grid.c7 = new Cell(this, "c", "7", black_square);
        this.grid.c6 = new Cell(this, "c", "6", white_square);
        this.grid.c5 = new Cell(this, "c", "5", black_square);
        this.grid.c4 = new Cell(this, "c", "4", white_square);
        this.grid.c3 = new Cell(this, "c", "3", black_square);
        this.grid.c2 = new Cell(this, "c", "2", white_square);
        this.grid.c1 = new Cell(this, "c", "1", black_square);
        this.grid.d8 = new Cell(this, "d", "8", black_square);
        this.grid.d7 = new Cell(this, "d", "7", white_square);
        this.grid.d6 = new Cell(this, "d", "6", black_square);
        this.grid.d5 = new Cell(this, "d", "5", white_square);
        this.grid.d4 = new Cell(this, "d", "4", black_square);
        this.grid.d3 = new Cell(this, "d", "3", white_square);
        this.grid.d2 = new Cell(this, "d", "2", black_square);
        this.grid.d1 = new Cell(this, "d", "1", white_square);
        this.grid.e8 = new Cell(this, "e", "8", white_square);
        this.grid.e7 = new Cell(this, "e", "7", black_square);
        this.grid.e6 = new Cell(this, "e", "6", white_square);
        this.grid.e5 = new Cell(this, "e", "5", black_square);
        this.grid.e4 = new Cell(this, "e", "4", white_square);
        this.grid.e3 = new Cell(this, "e", "3", black_square);
        this.grid.e2 = new Cell(this, "e", "2", white_square);
        this.grid.e1 = new Cell(this, "e", "1", black_square);
        this.grid.f8 = new Cell(this, "f", "8", black_square);
        this.grid.f7 = new Cell(this, "f", "7", white_square);
        this.grid.f6 = new Cell(this, "f", "6", black_square);
        this.grid.f5 = new Cell(this, "f", "5", white_square);
        this.grid.f4 = new Cell(this, "f", "4", black_square);
        this.grid.f3 = new Cell(this, "f", "3", white_square);
        this.grid.f2 = new Cell(this, "f", "2", black_square);
        this.grid.f1 = new Cell(this, "f", "1", white_square);
        this.grid.g8 = new Cell(this, "g", "8", white_square);
        this.grid.g7 = new Cell(this, "g", "7", black_square);
        this.grid.g6 = new Cell(this, "g", "6", white_square);
        this.grid.g5 = new Cell(this, "g", "5", black_square);
        this.grid.g4 = new Cell(this, "g", "4", white_square);
        this.grid.g3 = new Cell(this, "g", "3", black_square);
        this.grid.g2 = new Cell(this, "g", "2", white_square);
        this.grid.g1 = new Cell(this, "g", "1", black_square);
        this.grid.h8 = new Cell(this, "h", "8", black_square);
        this.grid.h7 = new Cell(this, "h", "7", white_square);
        this.grid.h6 = new Cell(this, "h", "6", black_square);
        this.grid.h5 = new Cell(this, "h", "5", white_square);
        this.grid.h4 = new Cell(this, "h", "4", black_square);
        this.grid.h3 = new Cell(this, "h", "3", white_square);
        this.grid.h2 = new Cell(this, "h", "2", black_square);
        this.grid.h1 = new Cell(this, "h", "1", white_square);
        this.moveto = [];
    }

    Board.prototype.update_state = function () {
        var idx;
        var from_colrow;
        var to_colrow;
        var state_array = this.game.state.replace("[", "").replace("]", "");
        state_array = state_array.replace(/\),/g, ") ");
        state_array = state_array.split(" ");
        state_array = state_array.map(function (cell) {
            return cell.replace(/\(/g, ",").replace(/\)/g, "").split(",");
        });
        this.pieces = [];
        for (idx = 0; idx < state_array.length; idx += 1) {
            if (state_array[idx][0] === "step") {
              this.game.turn = state_array[idx][1];
            }
            if ((state_array[idx][0] === "cell") && (state_array[idx][3] !== "b")) {
                this.pieces.unshift(new Piece(this, state_array[idx][1], state_array[idx][2], state_array[idx][3]));
            }
        }
        this.game.legals_array = this.game.legals.replace("[", "").replace("]", "");
        this.game.legals_array = this.game.legals_array.replace(/\),/g, ") ");
        this.game.legals_array = this.game.legals_array.split(" ");
        this.game.legals_array = this.game.legals_array.map(function (does) {
            return does.replace(/\(/g, ",").replace(/\)/g, "").split(",");
        });
        this.legals = {};
        for (idx = 0; idx < this.game.legals_array.length; idx += 1) {
            from_colrow = this.game.legals_array[idx][4] + this.game.legals_array[idx][5];
            to_colrow = this.game.legals_array[idx][this.game.legals_array[idx].length - 2] +
                    this.game.legals_array[idx][this.game.legals_array[idx].length - 1];
            if (this.legals.hasOwnProperty(from_colrow)) {
                this.legals[from_colrow].unshift(to_colrow);
            } else {
                this.legals[from_colrow] = [to_colrow];
            }
        }
        statetxt.textContent = "You to move";
        movetxt.textContent = this.game.turn;
        this.draw();
    };

    Board.prototype.set_oldpos = function (from_colrow, to_colrow) {
        var idx;
        for (idx = 0; idx < this.pieces.length; idx += 1) {
            if (from_colrow === this.pieces[idx].column + this.pieces[idx].row) {
                this.oldpos = this.pieces[idx];
                this.pieces[idx] = new Piece(this, to_colrow[0], to_colrow[1], this.pieces[idx].counter);
                break;
            }
        }
    };

    Board.prototype.makeMove = function (from_colrow, to_colrow) {
        var idx;
        var move;
        var moves_list = this.game.legals.replace("[", "").replace("]", "");
        moves_list = moves_list.replace(/\),/g, ") ");
        moves_list = moves_list.split(" ");
        for (idx = 0; idx < this.game.legals_array.length; idx += 1) {
            if (
                (from_colrow === this.game.legals_array[idx][4] + this.game.legals_array[idx][5]) &&
                (to_colrow === this.game.legals_array[idx][this.game.legals_array[idx].length - 2] + 
                    this.game.legals_array[idx][this.game.legals_array[idx].length - 1])
            ) {
                move = moves_list[idx];
                break;
            }
        }
        this.set_oldpos(from_colrow, to_colrow);
        this.game.requestNext(move);
    };

    Board.prototype.xy2grid = function (x, y) {
        var rect = this.canvas.getBoundingClientRect();
        var column = this.COLUMNS[Math.floor((x - rect.left) / this.SQUARE_LENGTH)];
        var row = this.ROWS[Math.floor((y - rect.top) / this.SQUARE_LENGTH)];
        return column + row;
    };

    Board.prototype.highlight = function (event) {
        var colrow = this.xy2grid(event.clientX, event.clientY);
        this.highlights = [];
        if (this.legals.hasOwnProperty(colrow)) {
            this.highlights = this.legals[colrow];
            this.highlights[this.legals[colrow].length] = colrow;
        }
        this.draw();
    };

    Board.prototype.select = function (event) {
        var colrow = this.xy2grid(event.clientX, event.clientY);
        if ((this.legals.hasOwnProperty(colrow)) || (this.moveto.includes(colrow))) {
            if (this.selected === undefined) {
                this.selected = colrow;
                this.moveto = this.legals[colrow];
            } else {
                if (this.selected === colrow) {
                    delete this.selected;
                    this.moveto = [];
                }
                if (this.moveto.includes(colrow)) {
                    this.makeMove(this.selected, colrow);
                }
            }
            this.draw();
        }
    };

    Board.prototype.draw = function () {
        var col;
        var row;
        var cell;
        for (col = 0; col < this.COLUMNS.length; col += 1) {
            for (row = 0; row < this.ROWS.length; row += 1) {
                cell = this.grid[this.COLUMNS[col] + this.ROWS[row]];
                cell.draw(cell.colour);
            }
        }
        if (this.selected === undefined) {
            for (col = 0; col < this.highlights.length; col += 1) {
                cell = this.grid[this.highlights[col]];
                cell.draw("yellow");
            }
        } else {
            for (col = 0; col < this.moveto.length; col += 1) {
                cell = this.grid[this.moveto[col]];
                cell.draw("yellow");
            }
            this.grid[this.selected].draw("greenyellow");
        }
        this.pieces.forEach(function (piece) {
            piece.draw();
        });
        if (this.oldpos !== undefined) {
            this.oldpos.oldpos_draw();
        }
    };

    function Game() {
        this.aiplayer = "black";
        this.player = "red";
        this.state = "[control(red),step(1),piece_count(black,12),piece_count(red,12)," +
                "cell(a,1,b),cell(a,2,wp),cell(a,3,b),cell(a,4,b),cell(a,5,b),cell(a,6,bp),cell(a,7,b),cell(a,8,bp)," +
                "cell(b,1,wp),cell(b,2,b),cell(b,3,wp),cell(b,4,b),cell(b,5,b),cell(b,6,b),cell(b,7,bp),cell(b,8,b)," +
                "cell(c,1,b),cell(c,2,wp),cell(c,3,b),cell(c,4,b),cell(c,5,b),cell(c,6,bp),cell(c,7,b),cell(c,8,bp)," +
                "cell(d,1,wp),cell(d,2,b),cell(d,3,wp),cell(d,4,b),cell(d,5,b),cell(d,6,b),cell(d,7,bp),cell(d,8,b)," +
                "cell(e,1,b),cell(e,2,wp),cell(e,3,b),cell(e,4,b),cell(e,5,b),cell(e,6,bp),cell(e,7,b),cell(e,8,bp)," +
                "cell(f,1,wp),cell(f,2,b),cell(f,3,wp),cell(f,4,b),cell(f,5,b),cell(f,6,b),cell(f,7,bp),cell(f,8,b)," +
                "cell(g,1,b),cell(g,2,wp),cell(g,3,b),cell(g,4,b),cell(g,5,b),cell(g,6,bp),cell(g,7,b),cell(g,8,bp)," +
                "cell(h,1,wp),cell(h,2,b),cell(h,3,wp),cell(h,4,b),cell(h,5,b),cell(h,6,b),cell(h,7,bp),cell(h,8,b)]";
        this.legals = "[does(red,move(wp,b,3,a,4)),does(red,move(wp,b,3,c,4)),does(red,move(wp,d,3,c,4)),does(red,move(wp,d,3,e,4))," +
                "does(red,move(wp,f,3,e,4)),does(red,move(wp,f,3,g,4)),does(red,move(wp,h,3,g,4))]";
        this.board = new Board(this);
        this.board.update_state();
    }

    Game.prototype.updateBoard = function () {
        var response_array;
        var move;
        var from_colrow;
        var to_colrow;
        var reward;
        var terminal;
        if (this.httpRequest.readyState === XMLHttpRequest.DONE) {
            if (this.httpRequest.status === 200) {
                response_array = this.httpRequest.responseText.split("&");
                move = response_array[0].split("=")[1];
                move = move.replace(/\(/g, ",").replace(/\)/g, "").split(",");
                from_colrow = move[4] + move[5];
                to_colrow = move[move.length - 2] + move[move.length - 1];
                this.state = response_array[1].split("=")[1];
                this.legals = response_array[2].split("=")[1];
                this.aiplayer = response_array[3].split("=")[1];
                reward = response_array[4].split("=")[1];
                terminal = response_array[5].split("=")[1];
                // debugging start
                rewardtxt.textContent = reward;
                terminaltxt.textContent = terminal;
                // debugging end
                this.board.set_oldpos(from_colrow, to_colrow);
                this.board.update_state();
            } else {
                alert("There was a problem with the request.");
            }
        }
    };

    Game.prototype.requestNext = function (move) {
        this.httpRequest = new XMLHttpRequest();
        if (!this.httpRequest) {
            alert("Giving up :( Cannot create an XMLHTTP instance");
            return false;
        }
        var send_str = "move=" + encodeURIComponent(move) +
                "&state=" + encodeURIComponent(this.state) +
                "&aiplayer=" + encodeURIComponent(this.aiplayer);
        this.httpRequest.onreadystatechange = this.updateBoard.bind(this);
        this.httpRequest.open("POST", "/move");
        this.httpRequest.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
        this.httpRequest.send(send_str);
        delete this.board.selected;
        delete this.board.legals;
        this.board.highlights = [];
        this.board.draw();
        movetxt.textContent = String(Number(this.turn) + 1);
        statetxt.textContent = "Computer is thinking";
    };

    var game = new Game();
}());


