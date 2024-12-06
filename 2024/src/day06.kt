import java.io.File

fun main() = solve("06", ::parse, ::part1, ::part2)

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
            guardDir = guardDir.turnCW()
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
private fun part2(input: GuardStart): Int {
    val passableMutable = MutableMatrix(input.passable.size) { pos -> input.passable[pos] }
    val visitedScratch = VisitedMap(input.passable.size)
    return input.passable.toSequence().count counter@{ (pos, isPassable) ->
        if (!isPassable || pos == input.startPos) return@counter false
        // temporarily set the current location to impassable
        passableMutable[pos] = false
        val doesLoop = isLoop(passableMutable, input.startPos, visitedScratch)
        // set the current location back to passable
        passableMutable[pos] = true
        return@counter doesLoop
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
            guardDir = guardDir.turnCW()
        } else {
            guardPos += guardDir
        }
    }
    return false
}