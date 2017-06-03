(function () {
    "use strict";
    function Board() {
        this.legals = ["does(red,move(wp,b,3,a,4))",
                       "does(red,move(wp,b,3,c,4))",
                       "does(red,move(wp,d,3,c,4))",
                       "does(red,move(wp,d,3,e,4))",
                       "does(red,move(wp,f,3,e,4))",
                       "does(red,move(wp,f,3,g,4))",
                       "does(red,move(wp,h,3,g,4))"];
        this.state = ["control(red)", "step(1)", "piece_count(black,12)", "piece_count(red,12)", 
          "cell(a,1,b)", "cell(a,2,wp)", "cell(a,3,b)", "cell(a,4,b)", "cell(a,5,b)", "cell(a,6,bp)", "cell(a,7,b)", "cell(a,8,bp)",
          "cell(b,1,wp)", "cell(b,2,b)","cell(b,3,wp)","cell(b,4,b)","cell(b,5,b)","cell(b,6,b)","cell(b,7,bp)","cell(b,8,b)",
          "cell(c,1,b)","cell(c,2,wp)","cell(c,3,b)","cell(c,4,b)","cell(c,5,b)","cell(c,6,bp)","cell(c,7,b)","cell(c,8,bp)",
          "cell(d,1,wp)","cell(d,2,b)","cell(d,3,wp)","cell(d,4,b)","cell(d,5,b)","cell(d,6,b)","cell(d,7,bp)","cell(d,8,b)",
          "cell(e,1,b)","cell(e,2,wp)","cell(e,3,b)","cell(e,4,b)","cell(e,5,b)","cell(e,6,bp)","cell(e,7,b)","cell(e,8,bp)",
          "cell(f,1,wp)","cell(f,2,b)","cell(f,3,wp)","cell(f,4,b)","cell(f,5,b)","cell(f,6,b)","cell(f,7,bp)","cell(f,8,b)",
          "cell(g,1,b)","cell(g,2,wp)","cell(g,3,b)","cell(g,4,b)","cell(g,5,b)","cell(g,6,bp)","cell(g,7,b)","cell(g,8,bp)",
          "cell(h,1,wp)","cell(h,2,b)","cell(h,3,wp)","cell(h,4,b)","cell(h,5,b)","cell(h,6,b)","cell(h,7,bp)","cell(h,8,b)"];
        this.ai_player = "black";
        this.player = "red";
        this.cells = this.state.map(function(cell) {return cell.replace(/\(/g,",").replace(/\)/g, "").split(",");});
        this.cells = this.cells.filter(function(term) {return term[0] === "cell";});
        this.cells = this.cells.map(function(cell) {return cell.slice(1);});
        this.moves = this.legals.map(function(move) {return move.replace(/\(/g,",").replace(/\)/g, "").split(",");});
        // this.moves = this.moves.map(function(move) {return move.slice(4);});
        this.canvas = document.getElementById("board");
        this.context = this.canvas.getContext("2d");
        this.canvas.onclick = this.selectMove.bind(this);
        this.SQUARE_LENGTH = Math.floor(this.canvas.width / 8);
        this.COLUMNS = ["a", "b", "c", "d", "e", "f", "g", "h"];
        this.ROWS = ["8", "7", "6", "5", "4", "3", "2", "1"];
        this.selected = null;
        this.possible_moves = [];
        this.httpRequest = undefined;
        // this.jump_start = null;    
    }

    Board.prototype.draw = function () {
        var black_square = "silver";
        var white_square = "ghostwhite";
        var selected_square = "greenyellow";
        var possible_move = "yellow";
        var square_colour = white_square;
        var square = function (col, row, square_colour) {
            var x = this.COLUMNS.indexOf(col);
            var y = this.ROWS.indexOf(row);
            var l = this.SQUARE_LENGTH;
            this.context.fillStyle = square_colour;
            this.context.fillRect(x * l, y * l, l, l);
        };
        var piece = function (col, row, counter) {
            var colour;
            if (counter === "wp") {
                colour = "firebrick";
            }
            if (counter === "wk") {
                colour = "tomato";
            }
            if (counter === "bp") {
                colour = "darkslategray";
            }
            if (counter === "bk") {
                colour = "black";
            }
            var x = (this.COLUMNS.indexOf(col) * this.SQUARE_LENGTH) + (this.SQUARE_LENGTH / 2);
            var y = (this.ROWS.indexOf(row) * this.SQUARE_LENGTH) + (this.SQUARE_LENGTH / 2);
            var halfsize = 0.4 * this.SQUARE_LENGTH;
            this.context.fillStyle = colour;
            this.context.strokeStyle = colour;
            this.context.beginPath();
            this.context.arc(x, y, halfsize, 0, Math.PI * 2, true);
            this.context.closePath();
            this.context.fill();
            this.context.stroke();
        };
        this.ROWS.forEach(function (row) {
            this.COLUMNS.forEach(function (col) {
                square.bind(this)(col, row, square_colour);
                if (square_colour === black_square) {
                    square_colour = white_square;
                } else {
                    square_colour = black_square;
                }
            }, this);
            if (square_colour === black_square) {
                square_colour = white_square;
            } else {
                square_colour = black_square;
            }
        }, this);
        if (this.selected !== null) {
            square.bind(this)(this.selected[0], this.selected[1], selected_square);
        }
        this.possible_moves.forEach(function (cell) {
            square.bind(this)(cell[cell.length - 2], cell[cell.length - 1], possible_move);
        }, this);
        this.cells.forEach(function (cell) {
            if (cell[2] !== "b") {
                piece.bind(this)(cell[0], cell[1], cell[2]);
            }
        }, this);
    };

    Board.prototype.selectMove = function (event) {
        // cases
        // does(red,doublejump(wp,e,4,c,6,e,8)
        var idx;
        var lenmove;
        var rect = this.canvas.getBoundingClientRect();
        var column = this.COLUMNS[Math.floor((event.clientX - rect.left) / this.SQUARE_LENGTH)];
        var row = this.ROWS[Math.floor((event.clientY - rect.top) / this.SQUARE_LENGTH)];
        if (this.possible_moves.length == 0) {
            this.possible_moves = this.moves.filter(function(move) {return move[4] === column && move[5] === row;});
            if (this.possible_moves.length > 0) {
                if (this.possible_moves[0][4] === column && this.possible_moves[0][5] === row) {
                    this.selected = [column, row];
                    this.draw();
                }
            }
        } else {
            for (idx = 0 ; idx < this.possible_moves.length ; idx++) {
                lenmove = this.possible_moves[idx].length - 1;
                if (this.possible_moves[idx][lenmove - 1] == column && this.possible_moves[idx][lenmove] == row) {
                    var move = this.possible_moves[idx][0] + "(" +
                               this.possible_moves[idx][1] + "," +
                               this.possible_moves[idx][2] + "(" + 
                               this.possible_moves[idx].slice(3).join(",") + "))" ;
                    var state = "[" + this.state.join(",") + "]";
                    this.httpRequest = new XMLHttpRequest();
                    if (!this.httpRequest) {
                        alert("Giving up :( Cannot create an XMLHTTP instance");
                        return false;
                    }
                    var send_str = "move=" + encodeURIComponent(move) + 
                                   "&state=" + encodeURIComponent(state) + 
                                   "&aiplayer=" + encodeURIComponent(this.ai_player);
                    this.httpRequest.onreadystatechange = this.makeMoves.bind(this);
                    this.httpRequest.open("POST", "/move");
                    this.httpRequest.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
                    this.httpRequest.send(send_str);
                    this.selected = null;
                    this.possible_moves = [];
                    this.draw();
                    break;
                }
            }
        }
    };

    Board.prototype.makeMoves = function () {
        var terms2lst = function (termstr) {
            termstr = termstr.replace("[", "").replace("]", "");
            termstr = termstr.replace(/\)\,/g, ") ");
            return termstr.split(" ");
        };
        // Step 2 – Handling the server response
        if (this.httpRequest.readyState === XMLHttpRequest.DONE) {
            // Everything is good, the response was received.
            if (this.httpRequest.status === 200) {
                var response_array = this.httpRequest.responseText.split("&");
                this.state = terms2lst(response_array[0].slice(6));
                this.legals = terms2lst(response_array[1].slice(7));
                this.cells = this.state.map(function(cell) {return cell.replace(/\(/g,",").replace(/\)/g, "").split(",");});
                this.cells = this.cells.filter(function(term) {return term[0] === "cell";});
                this.cells = this.cells.map(function(cell) {return cell.slice(1);});
                this.moves = this.legals.map(function(move) {return move.replace(/\(/g,",").replace(/\)/g, "").split(",");});
                var flash = document.querySelector("#flash");
                flash.textContent = this.legals;
                this.draw();
            } else {
                alert("There was a problem with the request.");
            }
        //} else {
        // Not ready yet
        }
    };


/*
var makeRequest = function (event) {
    move = event.currentTarget.textContent;
    state = "[" + state.toString() + "]";
    aiplayer = "black"
    // https://developer.mozilla.org/en-US/docs/AJAX/Getting_Started
    // Step 1 – How to make an HTTP request
    httpRequest = new XMLHttpRequest();
    if (!httpRequest) {
      alert("Giving up :( Cannot create an XMLHTTP instance");
      return false;
    }
    httpRequest.onreadystatechange = makeMoves;
    httpRequest.open("POST", "/move");
    httpRequest.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
    httpRequest.send("move=" + move + "&state=" + state + "&aiplayer=" + aiplayer);
};
*/

var board = new Board();
board.draw();


//var legalsList = document.querySelector("#legals");
//legalsList.innerHTML = "";

// var cells = prolog2js(state).filter(function(term) {return term[0] === "cell";});

// document.getElementById("state").textContent = cells;



}());

