# README #

This project consists in a basic Multi-User Dungeon (MUD) game, developed in Racket. To run, you can simply compile and execute the mud.rkt file.


# The game#
The player character is lost inside a maze with only one locked exit gate. There are many lost items inside the maze, but only one is capable of unlocking the gate. The objective is to search for the right key, find the gate and exit.

The maze is created using procedural generation, so as the items that it contains. The complexity of the level can be changed by replacing the variables X and Y inside de maze.rkt file.

### Basic commands ###
* 'look', 'examine room' - search for lost objects in the current room
* 'map', 'show map', 'see map' - look the map to see players current location
* 'north', 'east', 'south', 'west' - move to the desired room
* 'inventory', 'bag' - look the items in your bag
* 'pick item_name', 'get item_name', 'pickup item_name' - pick the item_name if it is the current room
* 'put item_name', 'drop item_name', 'remove item_name' - drop the item_name if one is available in your inventory


### Need help to understand the source? ###

* mail me: email@robertsonlima.com
* facebook: fb.com/robertson.lima