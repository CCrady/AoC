import java.io.File

fun main() = solve("20", { file -> traceTrack(parse(file)) }, ::part1, ::part2)


private fun parse(file: File): Triple<Vec2, Vec2, Matrix<Boolean>> {
    val charMatrix = Matrix.fromLines(file.readLines())
    val passable = charMatrix.map { char -> char != '#' }
    val startPos = charMatrix.toSequence().single { (_, char) -> char == 'S' }.index
    val endPos   = charMatrix.toSequence().single { (_, char) -> char == 'E' }.index
    return Triple(startPos, endPos, passable)
}

// Trace through the track. Returns a list of track positions in order from start to end, and a matrix mapping track
// positions to the picosecond on which they're reached without cheating. The matrix contains -1 for unreachable track
// positions.
private fun traceTrack(input: Triple<Vec2, Vec2, Matrix<Boolean>>): Pair<List<Vec2>, Matrix<Int>> {
    val (startPos, endPos, passable) = input
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


private fun part1(input: Pair<List<Vec2>, Matrix<Int>>): Int {
    val (trackOrder, stepAtlas) = input
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

private fun part2(input: Pair<List<Vec2>, Matrix<Int>>): Int {
    val (trackOrder, stepAtlas) = input
    val cheatOffsets = (2..20).associateWith(::vectorsWithMagnitude)
    return trackOrder.asSequence().flatMap { posBeforeCheat ->
        // for each track position, try cheating to all possible destinations within 20 steps
        val stepsBeforeCheat = stepAtlas[posBeforeCheat]
        cheatOffsets.flatMap { (cheatSteps, cheatVectors) ->
            cheatVectors.map { cheatVector ->
                // find the number of steps we'd save by cheating to posAfterCheat, or some negative number if we'd end
                // up inside a wall (since stepsAfterCheat will be -1)
                val posAfterCheat = posBeforeCheat + cheatVector
                val stepsAfterCheat = stepAtlas.getValue(posAfterCheat)
                stepsAfterCheat - stepsBeforeCheat - cheatSteps
            }
        }
    }.count { numSavedSteps ->
        numSavedSteps >= 100
    }
}

private fun vectorsWithMagnitude(mag: Int): List<Vec2> {
    val vectorsInSoutheast = (0..<mag).map { i -> Vec2(mag - i, i) }
    return listOf(
        { it }, Vec2::turnCW, Vec2::reverse, Vec2::turnCCW
    ).flatMap { turn ->
        vectorsInSoutheast.map(turn)
    }
}
