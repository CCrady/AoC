import java.io.File

fun main() = solve("12", ::parse, ::part1, ::part2)

private fun parse(file: File): Matrix<Char> = Matrix(file.readLines()) { _, c -> c }

private fun part1(atlas: Matrix<Char>): Int {
    return regions(atlas).sumOf { region -> region.area * region.perimeter }
}

private fun part2(atlas: Matrix<Char>): Int {
    return regions(atlas).sumOf { region -> region.area * region.numSides() }
}

private data class Region(val plots: Collection<Vec2>, val edges: Collection<Edge>) {
    val area: Int
        get() = plots.size
    val perimeter: Int
        get() = edges.size
    fun numSides(): Int {
        var numSidesAcc = 0
        val uncountedEdges = edges.toMutableSet()
        // iterate through uncountedEdges, and remove all contiguous edges with the same orientation
        while (uncountedEdges.isNotEmpty()) {
            numSidesAcc++

            // pop an arbitrary edge from uncountedEdges
            val edge = uncountedEdges.first()
            uncountedEdges.remove(edge)
            // remove all contiguous edges with the same orientation in the given direction
            fun runningRemove(dir: Vec2.CardinalDirection) {
                var toRemove = edge.nudge(dir)
                while (uncountedEdges.remove(toRemove)) {
                    toRemove = toRemove.nudge(dir)
                }
            }
            runningRemove(edge.orientation.turnCW)
            runningRemove(edge.orientation.turnCCW)
        }
        return numSidesAcc
    }
}

private data class Edge(val pos: Vec2, val orientation: Vec2.CardinalDirection) {
    fun nudge(dir: Vec2.CardinalDirection): Edge = Edge(pos + dir, orientation)
}

private fun regions(atlas: Matrix<Char>): Sequence<Region> = sequence {
    val visited = MutableMatrix(atlas.size) { false }
    for ((pos, char) in atlas) {
        if (visited[pos]) continue
        yield(floodFill(pos, char, atlas, visited))
    }
}

private fun floodFill(startPos: Vec2, char: Char, atlas: Matrix<Char>, visited: MutableMatrix<Boolean>): Region {
    val plots = ArrayDeque<Vec2>()
    val edges = ArrayDeque<Edge>()
    val stack = ArrayDeque(listOf(startPos))
    visited[startPos] = true
    while (stack.isNotEmpty()) {
        val currPos = stack.removeLast()
        plots.add(currPos)
        for (dir in Vec2.CardinalDirection.entries) {
            val newPos = currPos + dir
            if (!atlas.inBounds(newPos) || atlas[newPos] != char) { // we found an edge
                edges.add(Edge(currPos, dir))
            } else if (!visited[newPos]) { // newPos has yet to be queued
                visited[newPos] = true
                stack.addLast(newPos)
            }
        }
    }
    return Region(plots, edges)
}
