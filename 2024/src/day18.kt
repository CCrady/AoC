import java.io.File

fun main() = solve("18", ::parse, ::part1, ::part2)

// In order to make the size of the memory space and the number of bytes to include for part 1 available to the code,
// add a line at the start of the input file of the form <width>x<height> first <n>, e.g. "71x71 first 1024". Note that
// the width and height are one more than the x and y coordinates of the end position.

private data class Input18(val bounds: Vec2, val firstN: Int, val bytePoses: List<Vec2>)

private fun parse(file: File): Input18 {
    val lines = file.readLines()
    val (width, height, firstN) = """(\d+)x(\d+) first (\d+)""".toRegex().matchToInts(lines.first())
    val bytePoses = lines.drop(1).map { line ->
        val (xPos, yPos) = """(\d+),(\d+)""".toRegex().matchToInts(line)
        Vec2(xPos, yPos)
    }
    return Input18(Vec2(width, height), firstN, bytePoses)
}

private fun part1(input18: Input18): Int {
    val consideredBytePoses = input18.bytePoses.take(input18.firstN).toSet()
    val passable = Matrix(input18.bounds) { pos -> pos !in consideredBytePoses }
    val startPos = Vec2(0, 0)
    val endPos = input18.bounds - Vec2(1, 1)

    val visited = MutableMatrix(input18.bounds) { false }
    var frontier = setOf(startPos)
    for (iteration in countUp()) {
        for (pos in frontier) visited[pos] = true
        if (visited[endPos]) return iteration
        frontier = frontier.flatSetMap { currPos ->
            Vec2.CardinalDirection.entries.map { dir ->
                currPos + dir
            }.filter { nextPos ->
                visited.inBounds(nextPos) && !visited[nextPos] && passable[nextPos]
            }
        }
    }
    throw IllegalStateException("Is the countUp() iterator broken?")
}

// This approach keeps a path between the start and end saved. If a byte falls that doesn't block that path, we don't
// need to recalculate the paths, because we already have one that works.
private fun part2(input18: Input18): Vec2? {
    // maps each position to an adjacent position that comes before it in some fastest path to it from the start, or
    // null if the position doesn't have a position that comes before it. null can mean that the position is occupied by
    // a byte and thus impassable, or that it's the starting position and therefore there's no earlier position.
    val dirToStart = MutableMatrix(input18.bounds) { pos ->
        when {
            pos == Vec2.ZERO -> null
            pos.x == 0       -> Vec2.CardinalDirection.NORTH
            else             -> Vec2.CardinalDirection.WEST
        }
    }

    for (byte in input18.bytePoses) {
        dirToStart[byte] = null
        if (!isPathClear(dirToStart)) {
            val doesAPathExist = setPaths(dirToStart)
            if (!doesAPathExist) return byte
        }
    }
    // we got through the entire list without the path being blocked
    return null
}

// Whether the path we find by following the directions backwards from the end leads us all the way back to the start.
// This requires said path to not lead into an infinite loop.
private fun isPathClear(dirToStart: Matrix<Vec2.CardinalDirection?>): Boolean {
    val startPos = Vec2.ZERO
    val endPos = dirToStart.size - Vec2(1, 1)

    var currPos = endPos
    while (dirToStart[currPos] != null) currPos += dirToStart[currPos]!!
    return currPos == startPos
}

// Repopulate dirToStart based on the current obstacles. Returns whether there exists a path from the start to the end.
// This algorithm short-circuits once we reach the end position, so it may leave some positions that are reachable from
// the start un-repopulated, but this won't have any effect on the path we get when we trace from the end position
// backwards.
private fun setPaths(dirToStart: MutableMatrix<Vec2.CardinalDirection?>): Boolean {
    val startPos = Vec2.ZERO
    val endPos = dirToStart.size - Vec2(1, 1)

    val visited = MutableMatrix(dirToStart.size) { pos -> pos == Vec2.ZERO }
    val frontier = linkedSetOf(startPos)
    for (currPos in frontier.poppingIterator()) {
        // currPos and all the positions on the path it points to have already had their dirToStarts set, so if we
        // encounter endPos we're done
        if (currPos == endPos) return true
        for (dir in Vec2.CardinalDirection.entries) {
            val nextPos = currPos + dir
            // if nextPos is passable and unvisited, mark it visited, point its direction in dirToStart back to currPos,
            // and add it to the frontier
            if (dirToStart.inBounds(nextPos) && dirToStart[nextPos] != null && !visited[nextPos]) {
                visited[nextPos] = true
                dirToStart[nextPos] = dir.reverse
                frontier.add(nextPos)
            }
        }
    }
    // if we haven't returned yet it means that we didn't find any path to endPos
    return false
}