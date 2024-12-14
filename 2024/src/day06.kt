import java.io.File

fun main() = solve("06", ::parse, ::part1, ::part2v2)

private data class GuardStart(val passable: Matrix<Boolean>, val startPos: Vec2)

private fun parse(file: File): GuardStart {
    var guardPos: Vec2? = null
    val passable = Matrix(file.readLines()) { pos, char ->
        when (char) {
            '.' -> true
            '#' -> false
            '^' -> { guardPos = pos; true }
            else -> throw IllegalArgumentException("Invalid character in input matrix")
        }
    }
    requireNotNull(guardPos)
    return GuardStart(passable, guardPos!!)
}

private fun part1(input: GuardStart): Int {
    val passable = input.passable
    val visited = MutableMatrix(passable.size) { false }
    var guardDir = Vec2.CardinalDirection.NORTH
    var guardPos = input.startPos
    while (passable.inBounds(guardPos + guardDir)) {
        visited[guardPos] = true
        while (!passable[guardPos + guardDir]) {
            guardDir = guardDir.turnCW
        }
        guardPos += guardDir.vector
    }
    visited[guardPos] = true
    return visited.count { b -> b }
}

// A naive approach which does not save any work between different obstacle positions.
private fun part2v1(input: GuardStart): Int {
    val passableMutable = MutableMatrix(input.passable.size) { pos -> input.passable[pos] }
    val visitedScratch = VisitedMap(input.passable.size)
    return extraObstacleLocations(input).count { pos ->
        // temporarily set the current location to impassable
        passableMutable[pos] = false
        val doesLoop = isLoop(passableMutable, input.startPos, visitedScratch)
        // set the current location back to passable
        passableMutable[pos] = true
        doesLoop
    }
}

@JvmInline
private value class VisitedMap(val underlying: Matrix<BooleanArray>) {
    operator fun get(pos: Vec2, dir: Vec2.CardinalDirection): Boolean = underlying[pos][dir.ordinal]
    fun visit(pos: Vec2, dir: Vec2.CardinalDirection) {
        underlying[pos][dir.ordinal] = true
    }

    constructor(size: Vec2) : this(
        Matrix(size) { BooleanArray(Vec2.CardinalDirection.entries.size) { false } }
    )

    fun reset() {
        for ((_, arr) in underlying) {
            arr.fill(false)
        }
    }
}

private fun isLoop(passable: Matrix<Boolean>, startPos: Vec2, visitedScratch: VisitedMap): Boolean {
    visitedScratch.reset()
    var guardDir = Vec2.CardinalDirection.NORTH
    var guardPos = startPos
    while (passable.inBounds(guardPos)) {
        // if we've visited the current location-direction before, then we've found a loop
        if (visitedScratch[guardPos, guardDir]) return true
        // otherwise mark that we've visited this location-direction and move on
        visitedScratch.visit(guardPos, guardDir)
        val inFrontOfGuard = guardPos + guardDir
        if (passable.inBounds(inFrontOfGuard) && !passable[inFrontOfGuard]) {
            guardDir = guardDir.turnCW
        } else {
            guardPos += guardDir
        }
    }
    return false
}

private fun extraObstacleLocations(input: GuardStart): Sequence<Vec2> {
    val (passable, startPos) = input
    return passable.toSequence().filter { (pos, isPassable) ->
        isPassable && pos != startPos
    }.map { (pos, _) -> pos }
}

// A much faster approach which pre-calculates where the guard will end up from most positions.
private fun part2v2(input: GuardStart): Int {
    val startState = GuardState(input.startPos, Vec2.CardinalDirection.NORTH)
    val shortcutMap = makeShortcutMap(input.passable, startState)
    return extraObstacleLocations(input).count counter@{ extraObstacle ->
        val reachedStates = mutableSetOf<GuardState>()
        var currState = startState
        while (!currState.isOutOfBounds()) {
            val nextState = bumpTurnFast(input.passable, shortcutMap, extraObstacle, currState)
            if (nextState in reachedStates) return@counter true // we've found a loop
            reachedStates.add(nextState)
            currState = nextState
        }
        return@counter false // after iterating enough times we got out of bounds
    }
}

private data class GuardState(val pos: Vec2, val dir: Vec2.CardinalDirection) {
    fun isOutOfBounds(): Boolean = pos.x < 0
}
private typealias ShortcutMap = Map<GuardState, GuardState>

private fun makeShortcutMap(passable: Matrix<Boolean>, startState: GuardState): ShortcutMap {
    val obstacles = passable.toSequence().filter { (_, isPassable) -> !isPassable }.map { (pos, _) -> pos }
    // the states a guard can be in after bumping into one of the obstacles and turning
    val bumpTurnedStates = obstacles.flatMap { pos ->
        Vec2.CardinalDirection.entries.map { dir ->
            GuardState(pos - dir, dir.turnCW)
        }
    }.filter { (pos, _) ->
        passable.inBounds(pos)
    }
    // make sure to include the starting state in the map!
    val fromStates = sequenceOf(startState) + bumpTurnedStates
    return fromStates.associateWith { guardState -> bumpTurn(passable, guardState) }
}

// Move forwards from the given location-direction until we bump into something, and then turn right. If we go out of
// bounds, then return an out-of-bounds state instead.
private fun bumpTurn(passable: Matrix<Boolean>, fromState: GuardState): GuardState {
    val guardDir = fromState.dir
    var guardPos = fromState.pos
    while (passable.inBounds(guardPos)) {
        if (!passable[guardPos]) {
            guardPos -= guardDir
            return GuardState(guardPos, guardDir.turnCW)
        }
        guardPos += guardDir
    }
    return GuardState(Vec2(-1, -1), guardDir.turnCW)
}

// Given the shortcut map, the extra obstacle we've added, and the guard's current state, find the guard's state after
// they've bumped into something and turned.
private fun bumpTurnFast(
    passable: Matrix<Boolean>, shortcutMap: ShortcutMap, extraObstacle: Vec2, currState: GuardState
): GuardState {
    // if the current state isn't in the shortcut map, that means the guard just bumped into the extra obstacle and we
    // need to calculate the next obstructed state the slow way. we don't need to check whether they'll bump into the
    // extra obstacle here because they just did, so they're no longer facing it.
    if (currState !in shortcutMap) return bumpTurn(passable, currState)
    // otherwise we have a prediction
    val predictedState = shortcutMap.getValue(currState)
    val vecToExtra = extraObstacle - currState.pos
    val facingExtra = vecToExtra.inSameMooreDirection(currState.dir.vector)
    if (!facingExtra) return predictedState
    // otherwise the guard is currently facing the extra obstacle, and therefore stands a chance of bumping into it
    val distanceToExtra = vecToExtra.mag
    val distanceToPredicted =
        if (predictedState.isOutOfBounds())
            Int.MAX_VALUE
        else
            (predictedState.pos - currState.pos).mag + 1
    if (distanceToPredicted < distanceToExtra) return predictedState
    // otherwise the guard will bump into the extra obstacle before they bump into the predicted one
    val newPos = extraObstacle - currState.dir
    return GuardState(newPos, predictedState.dir)
}