import java.io.File

fun main() = solve("04", ::parse, ::part1, ::part2)

private fun parse(file: File): Matrix<Char> = Matrix(file.readLines().map { line -> line.toList() })

private fun part1(input: Matrix<Char>): Int {
    val xLocations = input.toSequence().filter { (_, char) -> char == 'X' }.map { (point, _) -> point }
    return xLocations.sumOf { location ->
        Vec2.Direction.entries.count { direction ->
            xmasInDirection(input, location, direction)
        }
    }
}

private fun xmasInDirection(grid: Matrix<Char>, startLocation: Vec2, direction: Vec2.Direction): Boolean {
    var location = startLocation
    for (char in "XMAS") {
        if (!grid.inBounds(location) || char != grid.at(location)) return false
        location += direction.vector
    }
    return true
}

private fun part2(input: Matrix<Char>): Int {
    val aLocations = input.toSequence().filter { (point, char) ->
        char == 'A' && point.x >= 1 && point.y >= 1 && point.x < input.width - 1 && point.y < input.height - 1
    }.map { (point, _) -> point }
    return aLocations.count { location -> xMasCenteredAt(input, location) }
}

private fun xMasCenteredAt(grid: Matrix<Char>, startLocation: Vec2): Boolean {
    val charInDirection = { direction: Vec2.Direction -> grid.at(startLocation + direction.vector) }
    val nwChar = charInDirection(Vec2.Direction.NORTHWEST)
    val seChar = charInDirection(Vec2.Direction.SOUTHEAST)
    val neChar = charInDirection(Vec2.Direction.NORTHEAST)
    val swChar = charInDirection(Vec2.Direction.SOUTHWEST)
    return (nwChar == 'M' && seChar == 'S' || nwChar == 'S' && seChar == 'M')
        && (neChar == 'M' && swChar == 'S' || neChar == 'S' && swChar == 'M')
}