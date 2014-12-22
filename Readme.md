GML
===

GML is a Conway's Game of Life writen in Erlang programming language.
It is running in a boundless space, so be careful - it can eat your Universe.
Author: Mikhail Kurkov <mkurkov@gmail.com>

Usage
-----

After cloning repository run simple `make` command - it will start Erlang shell with application started.
There are several commands that you can run from shell to control game process:

* gml:gen(100,200,50) - generate random game field with 100 points in width, 200 in height and 50 random placed live cells
* gml:run(N) - run game for N steps
* gml:run() - run game indefinitely
* gml:pause() - stop game if it is running
* gml:view(0,0,100,50) - print part of game field to console, params are X,Y,W,H
* gml:load("somefile.gml") - load game state from file (see description of format below)
* gml:save(0,0,100,50,"somefile.gml") - make a snapshot of game field, params - X,Y,W,H,FileName
* gml:watch(0,0,100,40) - run current game and print view to console

Coordinates started from top left then down and right.
In `priv` directory there are several examples.

Example usage:

    > gml:load("priv/oscillator.gml").
    ok
    > gml:view(0,0,5,5).
    .....
    ###..
    .....
    .....
    .....
    ok
    > gml:run(1).
    ok
    > gml:view(0,0,5,5).
    .#...
    .#...
    .#...
    .....
    .....
    ok
    > gml:save(0,0,5,5,"dump.gml").
    ok

Gosper gun war watching:

    > gml:load("priv/gosper_gun_war.gml").
    ok
    > gml:watch(0,0,100,40)
    ...
    ...
    Watching X:0 Y:0 W:100 H:40. Generation: 311.
    Press ENTER to stop watching.

    ok
    >


Input file format
-----------------

Input files have `.gml` extension and have simple structure: just put any symbol to places where alive cells should be.
Dots and spaces represent dead cells. Lines started with exclamation mark is ignored and can be used for comments.
For example:

> cat priv/glider.gml
```
 .#.
 ..#
 ###
```

> cat priv/gosper_gun.gml
```
!Name: Gosper glider gun
!Author: Bill Gosper
!The first known gun and the first known finite pattern with unbounded growth.
!www.conwaylife.com/wiki/index.php?title=Gosper_glider_gun
........................O
......................O.O
............OO......OO............OO
...........O...O....OO............OO
OO........O.....O...OO
OO........O...O.OO....O.O
..........O.....O.......O
...........O...O
............OO
```
