# checkers-webapp

A web checkers player using swi-prolog as the server and javascript as the client.

The documentation so far is limited to a post I did at [https://groups.google.com/forum/?nomobile=true#!topic/swi-prolog/hSCLma-X97A](https://groups.google.com/forum/?nomobile=true#!topic/swi-prolog/hSCLma-X97A). Please comment there if you have any suggestions.

I've started a project which I hope will grow into a nice example of how to use swi-prolog as a webapp framework to play strategy games (something I think prolog is better suited for than other programming languages).

I've progressed far enough to put something out there (in the spirit of release early, release often).

The code is at [https://github.com/roblaing/checkers-webapp](https://github.com/roblaing/checkers-webapp) (it only consists of four files: server.pl, checkers.pl, script.js and index.html).

I use a linux machine and start the server at port 3000 with

> ./server.pl &

I assume it would also work on Windows.

To get a look at how it works, I've deployed it at [http://www.frontiersoftware.co.za/](http://www.frontiersoftware.co.za)

The "rules" code for checkers has been converted from [http://games.ggp.org/base/games/checkers/checkers.kif](http://games.ggp.org/base/games/checkers/checkers.kif) which is written in a lisp-style prolog developed by the general game playing (GGP) community. There are hundreds of games, including chess, written by them aimed "robotic warfare" in the sense of competing AI players rather than human vs web server. My ambition is to create a general framework to be able to put many of these games on the web for humans to play.

So far, I've spent most of my time battling to figure out how to get the javascript frontent and swi-prolog backend to communicate, so there is no proper AI yet, just a random legal player. Also the frontend still has some way to go. The human player at time of writing is hardwired to red and you can't change your mind once you have selected a piece - all to be fixed soon.

I'm very much a novice (all the code is based on various tutorials), so any helpful advice from experts will be much appreciated.

Robert

