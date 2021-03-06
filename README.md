# code-buster

A Clojure library designed to ... well, that part is up to you.

# Rules


The game is played on a map 16001 units wide and 9001 units high. 
The coordinate X=0, Y=0 is the top left pixel.

Each player controls a team of several busters. The teams start out at opposite corners of
the map, near their base. Ghosts are scattered across the map, and need to be trapped and 
brought back to a player's base. Each ghost inside the base or trapped by a buster is worth
a point for the owner. However, you may lose a point if one of your busters releases 
a ghost outside of your base.

**The map works as follows:**

* There are always 2 teams in play.
* At the start of the game, each player is given a team id. This indicates on which corner their base is positioned. The top left corner (X=0, Y=0) is for team 0. The bottom right corner ( X=16000, Y=9000) is for team 1.
* Fog makes it impossible to know the positions of ghosts and rival busters unless they are within 2200 units from one of your own busters.
* Each buster has a unique id. Each ghost has a unique id. Ghosts and busters with the same id are not correlated.

**Busters work as follows:**

* Every turn, a buster may do one of the following actions: `MOVE`, `BUST`, `RELEASE` or `STUN`
* `MOVE` followed by map coordinates will make the buster advance 800 units towards that point. The position will be rounded to the nearest integer.
* `BUST` followed by a ghost's id will cause the buster to suck that ghost into his ghost trap if he is within a range of 1760 units around the target ghost but not closer than 900 units. Trapped ghosts are no longer visible on the map.
* A buster may carry no more than 1 ghost at a time.
* `RELEASE` will cause the buster to drop the ghost he is carrying back onto the map. If this is done within 1600 units of a corner of the map on which a base is positioned, the ghost is removed from play and secures a point for the base's owner.
* `STUN` followed by a buster's id will make the buster shoot a bolt of lightning that will stun the target buster. A stunned buster cannot do anything for 10 turns. The buster's stun ability must recharge for 20 turns before it can be used again. Being stunned or attempting to stun will cause a buster to release any ghost he may be carrying.
* A buster may only stun opponents within a 1760 unit radius.

**Ghosts work as follows:**

* Ghosts remain stationary unless busters are within 2200 units of them and they are not in the process of being trapped. In this case, they will move 400 units away from the mean position of the near busters.
* If several busters attempt to trap a ghost with 0 stamina, the team with the most busters will take priority. Within that team, the closest buster will take the ghost. If both teams have an equal number of busters attempting to trap the ghost, it will not be trapped on that turn.
* A ghost being carried by a buster will escape if that buster attempts to trap a different ghost.
* Stamina does not regenerate.
* The game ends once all ghosts have been captured or after the time limit of 400 turns.

The game state of every turn is given to you as a list of entities, each with an id, position, type, state and value.

The type will be:

* 0 for a buster from team 0.
* 1 for a buster from team 1.
* -1 for a ghost.

The state will be:

* For busters:
  * 0: idle or moving buster.
  * 1: buster carrying a ghost.
  * 2: stunned buster.
  * 3: buster in the process of trapping a ghost.
* For ghosts, it is equal to the ghost's current stamina.

The value will be:

* For busters:
  * If the buster is carrying a ghost, that ghost's id.
  * If the buster is stunned the number of turns until he can move again.
* For ghosts, it is 0 unless several busters tied in trying to trap it, in which cas it is equal to the amount of busters that attempted to trap it on that turn.
 
**Victory Conditions**

Have captured more ghosts than the rival team at the end of the game.
 
**Lose Conditions**

* Your program provides unrecognized output.
* Your program times out.
* You have less ghosts than the opponent at the end of the game.
 
## Note

The program must first read within an infinite loop, read the contextual data from the standard input and provide to the standard output the desired instructions.
 	Game Input

**Initialization input**

* Line 1: an integer bustersPerPlayer for the amount of busters each team controls.
* Line 2: an integer ghostCount for the amount of ghosts available on the map.
* Line 3: an integer myTeamId for the team you are playing as.

**Input for one game turn**

* Line 1: an integer entities for the amount of entities on the map that are visible to you.
* Next entities lines: 6 space separated integers entityId, x, y, entityType, state & value. 

Represents a buster or a ghost.

**Output for one game turn**

One line for each buster: one of the following:

* `MOVE` followed by two integers x and y
* `BUST` followed by one integer ghostId
* `RELEASE`
* `STUN` followed by one integer busterId

You may append text to your instructions, it will be displayed in the viewer.

Constraints

* 2 ≤ bustersPerPlayer ≤ 5
* 8 ≤ ghostCount ≤ 28

Response time per turn ≤ 100ms

What is in store for me in the higher leagues ?
The extra rule available in higher leagues is:
Ghosts with endurance, who take several turns to successfully trap.

## Developers

    (require '[clojure.test :refer [run-tests]])
    (require 'Player-test)
    (run-tests 'Player-test)
    (Player-test/do-think-case-3)
    
    
* [Using Potential Fields in a Real-time Strategy Game Scenario](http://aigamedev.com/open/tutorials/potential-fields/)
* 

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
