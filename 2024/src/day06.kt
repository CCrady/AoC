import java.io.File
import kotlin.math.sign

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

// A naive approach which tests each possible obstacle location independently.
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

// FIXME: gives the wrong answer

private data class GuardState(val pos: Vec2, val dir: Vec2.CardinalDirection) {
    fun isOutOfBounds(): Boolean = pos.x < 0
    fun turn(): GuardState = GuardState(this.pos, this.dir.turnCW)

    companion object {
        val outOfBounds = GuardState(Vec2(-1, -1), Vec2.CardinalDirection.NORTH)
    }
}
private typealias ShortcutMap = Map<GuardState, GuardState>

private fun part2v2(input: GuardStart): Int {
    val startState = GuardState(input.startPos, Vec2.CardinalDirection.NORTH)
    val shortcutMap = makeShortcutMap(input.passable, startState)
    return extraObstacleLocations(input).count counter@{ extraObstacle ->
        val reachedStates = mutableSetOf<GuardState>()
        var currentState = startState
        do {
            val nextState = nextBump(input.passable, shortcutMap, extraObstacle, currentState)
            if (nextState in reachedStates) return@counter true // we've found a loop
            reachedStates.add(nextState)
            currentState = nextState
        } while (!currentState.isOutOfBounds())
        return@counter false // after iterating enough times we got out of bounds
    }
}

private fun makeShortcutMap(passable: Matrix<Boolean>, startState: GuardState): ShortcutMap {
    val obstacles = passable.toSequence().filter { (_, isPassable) -> !isPassable }.map { (pos, _) -> pos }
    val obstructedStates = obstacles.flatMap { pos ->
        Vec2.CardinalDirection.entries.map { dir -> GuardState(pos + dir, dir.reverse) }
    }.filter { (pos, _) ->
        passable.inBounds(pos)
    }
    val nextStates = obstructedStates.map { guardState ->
        findObstacle(passable, guardState.turn())
    }
    // don't forget to include the first obstruction we face from the start state!
    val obstructionFromStartState = findObstacle(passable, startState)
    val associations = (obstructedStates zip nextStates) + (startState to obstructionFromStartState)
    return associations.toMap()
}

// Move forwards from the given location-direction. Return the first obstructed position we get into, or outOfBounds if
// there are no obstacles in our way.
private fun findObstacle(passable: Matrix<Boolean>, fromState: GuardState): GuardState {
    val guardDir = fromState.dir
    var guardPos = fromState.pos
    while (passable.inBounds(guardPos)) {
        if (!passable[guardPos]) {
            guardPos -= guardDir
            return GuardState(guardPos, guardDir)
        }
        guardPos += guardDir
    }
    return GuardState.outOfBounds
}

// Given the shortcut map, the extra obstacle we've added, and the guard's current state, find the guard's next
// obstructed state.
private fun nextBump(
    passable: Matrix<Boolean>, shortcutMap: ShortcutMap, extraObstacle: Vec2, guardState: GuardState
): GuardState {
    // if the current state isn't in the shortcut map, that means we just bumped into the extra obstacle and we need to
    // calculate the next obstructed state the slow way
    if (guardState !in shortcutMap) {
        return findObstacle(passable, guardState.turn())
    }
    val predictedState = shortcutMap.getValue(guardState)
    val vecToExtra = extraObstacle - guardState.pos
    // the predicted state is one position before the obstacle itself
    val vecToPredicted = predictedState.pos + guardState.dir.turnCW - guardState.pos
    val willMoveInDirectionOfExtra = vecToExtra.x.sign == vecToPredicted.x.sign
                                  && vecToExtra.y.sign == vecToPredicted.y.sign
    // if we're not moving towards the extra obstacle, or the extra obstacle is closer than the predicted one, then
    // we'll hit the predicted one next
    if (!willMoveInDirectionOfExtra || vecToPredicted.mag < vecToExtra.mag) {
        return predictedState
    }
    // otherwise we'll hit the extra obstacle
    val newDir = guardState.dir.turnCW
    val newPos = extraObstacle - newDir
    return GuardState(newPos, newDir)

}