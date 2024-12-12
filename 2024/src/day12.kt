import java.io.File

fun main() = solve("12", ::parse, ::part1)

private fun parse(file: File): Matrix<Char> = Matrix(file.readLines()) { _, c -> c }

private fun part1(atlas: Matrix<Char>): Int {
    return regions(atlas).sumOf { region -> region.price }
}

private data class Region(val plots: Collection<Vec2>, val perimeter: Int) {
    val area: Int
        get() = plots.size
    val price: Int
        get() = perimeter * area
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
    var perimeterAcc = 0
    val stack = ArrayDeque(listOf(startPos))
    visited[startPos] = true
    while (stack.isNotEmpty()) {
        val currPos = stack.removeLast()
        plots.add(currPos)
        for (dir in Vec2.CardinalDirection.entries) {
            val newPos = currPos + dir
            if (!atlas.inBounds(newPos) || atlas[newPos] != char) { // we found an edge
                perimeterAcc++
            } else if (!visited[newPos]) { // newPos has yet to be queued
                visited[newPos] = true
                stack.addLast(newPos)
            }
        }
    }
    return Region(plots, perimeterAcc)
}
