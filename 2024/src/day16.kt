import java.io.File
import java.util.EnumMap

fun main() = solve("16", ::preprocess, ::part1, ::part2)

private fun preprocess(file: File): Pair<ReindeerMaze, ScoreMap> {
    val maze = parse(file)
    val bestScores = findBestPaths(maze)
    return Pair(maze, bestScores)
}

// The parsed input file. This uses a modified coordinate system which captures only those positions in the original
// maze with odd coordinates. It maps each position to the set of passable directions from that position.
private typealias ReindeerMaze = Matrix<Set<Vec2.CardinalDirection>>
private data class ReindeerState(val pos: Vec2, val dir: Vec2.CardinalDirection) {
    // the position in the original coordinate system
    val originalPos: Vec2
        get() = pos * 2 + Vec2(1, 1)
}

private fun parse(file: File): ReindeerMaze {
    val lines = file.readLines()
    val height = lines.size
    val width = lines.first().length
    val passable = Matrix(file.readLines()) { pos, char ->
        when (char) {
            '.' -> true
            'S' -> { require(pos == Vec2(1, height - 2)); true }
            'E' -> { require(pos == Vec2(width - 2, 1));  true }
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
    return Matrix(passable.size / 2) { pos ->
        val originalPos = pos * 2 + Vec2(1, 1)
        Vec2.CardinalDirection.entries.filter { dir ->
            passable[originalPos + dir]
        }.toSet()
    }
}

private typealias ScoreMap = Matrix<EnumMap<Vec2.CardinalDirection, Int>>
private operator fun ScoreMap.get(state: ReindeerState): Int = this[state.pos][state.dir]!!
private operator fun ScoreMap.set(state: ReindeerState, score: Int) {
    this[state.pos][state.dir] = score
}

// An approach that does an exhaustive search through the fastest possible paths to each state. It prioritizes moving
// straight over turning. In theory this could end up searching a path from a given location before we've found the
// fastest path to that location, meaning it would have to go back over the path from the location with the new fastest
// scores, but in practice turning costs so much more than moving that this will almost never happen. Unlike Dijkstra's,
// this algorithm doesn't require a heap with a reducePriority operation, which unfortunately does not exist in the Java
// or Kotlin standard libraries.
private fun findBestPaths(maze: ReindeerMaze): ScoreMap {
    val startState = ReindeerState(
        Vec2(0, maze.height - 1),
        Vec2.CardinalDirection.EAST
    )
    // we want to iterate through the frontier in order of when we added each element, so that we pop states with fewer
    // turns first. linkedSetOf guarantees that this will be the case
    val frontier = linkedSetOf(startState)
    val bestScores: ScoreMap = Matrix(maze.size) { pos ->
        EnumMap(Vec2.CardinalDirection.entries.associateWith { dir ->
            if (ReindeerState(pos, dir) == startState) 0
            else Int.MAX_VALUE
        })
    }

    // given a state and a score to reach it, add the states we'd get from turning left and right to the frontier, iff
    // the score we'd get would be better than the previous best and we could move forwards
    fun turnAndAddToFrontier(oldState: ReindeerState) {
        val newScore = bestScores[oldState] + 1000
        for (newDir in listOf(oldState.dir.turnCW, oldState.dir.turnCCW)) {
            val newState = ReindeerState(oldState.pos, newDir)
            val canMove = newState.dir in maze[newState.pos]
            if (canMove && newScore < bestScores[newState]) {
                bestScores[newState] = newScore
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
        var currScore = bestScores[fromState]
        // until we hit a wall or a section we've already been over...
        while (dir in maze[currPos]) {
            // move in the given direction and update our score accordingly. we have to add 2 because each move in the
            // updated coordinate system corresponds to 2 moves in the original
            currPos += dir
            currScore += 2
            val currState = ReindeerState(currPos, dir)
            val oldBestScore = bestScores[currState]
            // if we've already found a better or equivalent path to this state, skip it
            if (currScore >= oldBestScore) break
            bestScores[currState] = currScore
            // update the frontier with the states we'd get from turning left and right from the current state
            turnAndAddToFrontier(currState)
        }
    }
    return bestScores
}

private fun part1(input: Pair<ReindeerMaze, ScoreMap>): Int {
    val (_, bestScores) = input
    val endPoses = bestScores[bestScores.width - 1, 0]
    return endPoses.minOf { (_, x) -> x }
}

// Beginning at the end position, work our way backwards and record the positions that are part of any shortest path.
// Return the number of positions recorded.
// This is quite a long and unwieldy function, mostly because we have to correct for the coordinate-system
// transformation ignoring all the tiles that connect two odd-coordinate tiles. It's probably possible to do something
// clever with the frontier to make the implementation more elegant and easier to follow, but that can wait for another
// time.
// TODO: refactor this function
private fun part2(input: Pair<ReindeerMaze, ScoreMap>): Int {
    val (maze, bestScores) = input
    val endPos = Vec2(bestScores.width - 1, 0)
    val endState = Vec2.CardinalDirection.entries.map { dir ->
        ReindeerState(endPos, dir)
    }.minBy { state ->
        bestScores[state]
    }

    var frontier = setOf(endState)
    val visitedOriginalPoses = mutableSetOf<Vec2>()
    while (frontier.isNotEmpty()) {
        visitedOriginalPoses.addAll(frontier.map { state -> state.originalPos })
        // flatmap each state in the frontier to the set of states that can reach it in a single action and that have
        // lower scores, i.e. the set of states that come immediately before the given state on any shortest path
        frontier = frontier.flatSetMap { currState ->
            val currScore = bestScores[currState]
            // turnedCWOrNull, turnedCCWOrNull, and movedBackOrNull are all candidates for previous states. they're null
            // if the state they represent cannot reach currState in a single action or if the state they represent
            // doesn't have a lower score than currState.
            val turnedCWOrNull = ReindeerState(currState.pos, currState.dir.turnCW).let { state ->
                if (bestScores[state] < currScore) {
                    state
                } else null
            }
            val turnedCCWOrNull = ReindeerState(currState.pos, currState.dir.turnCCW).let { state ->
                if (bestScores[state] < currScore) {
                    state
                } else null
            }
            val movedBackOrNull = ReindeerState(currState.pos - currState.dir, currState.dir).let { state ->
                if (currState.dir.reverse in maze[currState.pos] && bestScores[state] < currScore) {
                    state
                } else null
            }
            // if we can move backwards, make sure to add the position between the current and moved-back position,
            // since that's not counted in the frontier
            if (movedBackOrNull != null) {
                val intermediatePos = currState.originalPos - currState.dir
                visitedOriginalPoses.add(intermediatePos)
            }

            listOfNotNull(
                turnedCWOrNull, turnedCCWOrNull, movedBackOrNull
            )
        }
    }

    return visitedOriginalPoses.size
}

// Testing function to print the current state of the frontier and visited sets from the above function's
// implementation.
private fun printVisitedAndFrontier(size: Vec2, visitedOriginalPoses: Set<Vec2>, frontier: Set<ReindeerState>) {
    val originalSize = size * 2 + Vec2(1, 1)
    val frontierOriginalStates = frontier.associate { state ->
        state.originalPos to state.dir
    }
    println(Matrix(originalSize) { originalPos ->
        if (originalPos.x in listOf(0, originalSize.x - 1) || originalPos.y in listOf(0, originalSize.y - 1)) {
            '#'
        } else if (originalPos in frontierOriginalStates) {
            frontierOriginalStates.getValue(originalPos).asChar()
        } else if (originalPos in visitedOriginalPoses) {
            'O'
        } else if (originalPos % Vec2(2, 2) == Vec2(1, 1)) {
            '.'
        } else ' '
    })
}
