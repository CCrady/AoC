import java.io.File

fun main() = solve("20", ::parse, ::part1)

private data class Input20(val startPos: Vec2, val endPos: Vec2, val passable: Matrix<Boolean>)

private fun parse(file: File): Input20 {
    val charMatrix = Matrix.fromLines(file.readLines())
    val passable = charMatrix.map { char -> char != '#' }
    val startPos = charMatrix.toSequence().single { (_, char) -> char == 'S' }.index
    val endPos   = charMatrix.toSequence().single { (_, char) -> char == 'E' }.index
    return Input20(startPos, endPos, passable)
}

private fun part1(input: Input20): Int {
    val (startPos, endPos, passable) = input
    val (trackOrder, stepAtlas) = traceTrack(startPos, endPos, passable)
    return trackOrder.asSequence().flatMap { posBeforeCheat ->
        // for each track position, try cheating in each direction
        val stepsBeforeCheat = stepAtlas[posBeforeCheat]
        Vec2.CardinalDirection.entries.map { dir ->
            // find the number of steps we'd save by cheating in this direction, or some negative number if we'd end up
            // inside a wall (since stepsAfterCheat will be -1)
            val posAfterCheat = posBeforeCheat + dir + dir
            val stepsAfterCheat = stepAtlas.getValue(posAfterCheat)
            stepsAfterCheat - stepsBeforeCheat - 2
        }
    }.count { numSavedSteps ->
        numSavedSteps >= 100
    }
}

// Trace through the track. Returns a list of track positions in order from start to end, and a matrix mapping track
// positions to the picosecond on which they're reached without cheating. The matrix contains -1 for unreachable track
// positions.
private fun traceTrack(startPos: Vec2, endPos: Vec2, passable: Matrix<Boolean>): Pair<List<Vec2>, Matrix<Int>> {
    val trackOrder = ArrayDeque<Vec2>()
    val stepAtlas = MutableMatrix<Int>(passable.size) { -1 }
    var currPos = startPos
    for (step in countUp()) {
        trackOrder.addLast(currPos)
        stepAtlas[currPos] = step
        val nextPos = Vec2.CardinalDirection.entries.map { dir ->
            currPos + dir
        }.singleOrNull { nextPos ->
            passable[nextPos] && stepAtlas[nextPos] == -1
        }
        if (nextPos == null) break
        currPos = nextPos
    }
    require(currPos == endPos)
    return Pair(trackOrder, stepAtlas.withDefault { -1 })
}
