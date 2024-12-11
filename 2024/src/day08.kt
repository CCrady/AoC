import java.io.File

fun main() = solve("08", ::parse, ::part1, ::part2)

private data class Input(val atlas: Map<Char, Set<Vec2>>, val bounds: Vec2)

private fun parse(file: File): Input {
    val map = mutableMapOf<Char, MutableSet<Vec2>>().withDefault { mutableSetOf() }
    val matrix = Matrix(file.readLines()) { _, c -> c }
    for ((pos, char) in matrix) {
        if (char == '.') continue
        map[char] = map.getValue(char).also { set -> set.add(pos) }
    }
    return Input(map, matrix.size)
}

private fun part1(input: Input): Int {
    val (atlas, bounds) = input
    return atlas.values.asSequence().flatMap { locations ->
        locations.distinctPairs().map { (pos1, pos2) ->
            (pos2 - pos1) + pos2
        }
    }.filter { pos ->
        pos.inBounds(Vec2.ZERO, bounds)
    }.toSet().size
}

private fun part2(input: Input): Int {
    val (atlas, bounds) = input
    return atlas.values.asSequence().flatMap { locations ->
        locations.distinctPairs().flatMap { (pos1, pos2) ->
            val displacement = pos2 - pos1
            var currentPos = pos2
            sequence {
                while (currentPos.inBounds(Vec2.ZERO, bounds)) {
                    yield(currentPos)
                    currentPos += displacement
                }
            }
        }
    }.toSet().size
}

private fun <E> Set<E>.distinctPairs(): Sequence<Pair<E, E>> = sequence {
    for (a in this@distinctPairs) {
        for (b in this@distinctPairs) {
            if (a == b) continue
            yield(Pair(a, b))
        }
    }
}
