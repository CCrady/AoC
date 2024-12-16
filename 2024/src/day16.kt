import java.io.File
import java.util.EnumMap

fun main() = solve("16", ::parse, ::part1)

// The parsed input file. This uses a modified coordinate system which captures only those positions in the original
// maze with odd coordinates.
private data class ReindeerMaze(
    // atlas maps each position to the set of passable directions from that position
    val atlas: Matrix<Set<Vec2.CardinalDirection>>,
    val start: Vec2, val end: Vec2
)
private data class ReindeerState(val pos: Vec2, val dir: Vec2.CardinalDirection)

private fun parse(file: File): ReindeerMaze {
    var originalStartPos: Vec2? = null
    var originalEndPos: Vec2? = null
    val passable = Matrix(file.readLines()) { pos, char ->
        when (char) {
            '.' -> true
            'S' -> { originalStartPos = pos; true }
            'E' -> { originalEndPos = pos; true }
            '#' -> false
            else -> throw IllegalArgumentException("invalid character $char in atlas")
        }
    }
    // ensure that the maze is made of connections between positions with odd coordinates
    require(passable.toSequence().all { (pos, passable) ->
        val xEven = pos.x % 2 == 0
        val yEven = pos.y % 2 == 0
        if (xEven && yEven) !passable
        else if (!xEven && !yEven) passable
        else true
    })
    // transform the coordinate system into one where just the positions with odd coordinates are represented
    val startPos = originalStartPos!! / 2
    val endPos = originalEndPos!! / 2
    val atlas = Matrix(passable.size / 2) { pos ->
        val originalPos = pos * 2 + Vec2(1, 1)
        Vec2.CardinalDirection.entries.filter { dir ->
            passable[originalPos + dir]
        }.toSet()
    }
    return ReindeerMaze(atlas, startPos, endPos)
}

// An approach that does an exhaustive search through the fastest possible paths to each state. It prioritizes moving
// straight over turning. In theory this could end up searching a path from a given location before we've found the
// fastest path to that location, meaning it would have to go back over the path from the location with the new fastest
// scores, but in practice turning costs so much more than moving that this will almost never happen. Unlike Dijkstra's,
// this algorithm doesn't require a heap with a reducePriority operation, which unfortunately does not exist in the Java
// or Kotlin standard libraries.
private fun part1(input: ReindeerMaze): Int {
    val (atlas, startPos, endPos) = input
    val startState = ReindeerState(startPos, Vec2.CardinalDirection.EAST)
    // we want to iterate through the frontier in order of when we added each element, so that we pop states with fewer
    // turns first. linkedSetOf guarantees that this will be the case
    val frontier = linkedSetOf(startState)
    val bestScores = Matrix(atlas.size) { pos ->
        EnumMap(Vec2.CardinalDirection.entries.associateWith { dir ->
            if (ReindeerState(pos, dir) == startState) 0
            else Int.MAX_VALUE
        })
    }

    fun bestScoreAt(state: ReindeerState) = bestScores[state.pos][state.dir]!!
    // given a state and a score to reach it, add the states we'd get from turning left and right to the frontier, iff
    // the score we'd get would be better than the previous best and we could move forwards
    fun turnAndAddToFrontier(oldState: ReindeerState) {
        val newScore = bestScoreAt(oldState) + 1000
        for (newDir in listOf(oldState.dir.turnCW, oldState.dir.turnCCW)) {
            val newState = ReindeerState(oldState.pos, newDir)
            val canMove = newState.dir in atlas[newState.pos]
            if (canMove && newScore < bestScoreAt(newState)) {
                bestScores[newState.pos][newState.dir] = newScore
                frontier.add(newState)
            }
        }
    }
    turnAndAddToFrontier(startState)

    // iterate through the frontier. at each step, pop an arbitrary state from the frontier, and have the reindeer move
    // as far forwards as possible (until it hits a wall). if it could turn at any point, add the turned state to the
    // frontier.
    for (fromState in frontier.poppingIterator()) {
        val dir = fromState.dir
        var currPos = fromState.pos
        var currScore = bestScoreAt(fromState)
        // until we hit a wall...
        while (dir in atlas[currPos]) {
            // move in the given direction and update our score accordingly. we have to add 2 because each move in the
            // updated coordinate system corresponds to 2 moves in the original
            currPos += dir
            currScore += 2
            bestScores[currPos][dir] = currScore
            // update the frontier with the states we'd get from turning left and right from the current state
            turnAndAddToFrontier(ReindeerState(currPos, dir))
        }
    }
    return bestScores[endPos].minOf { (_, score) -> score }
}
