import java.io.File

fun main() = solve("10", ::parse, ::part1, ::part2)

private fun parse(file: File): Matrix<Int> {
    return Matrix(file.readLines()) { _, char -> char - '0' }
}

private fun part1(atlas: Matrix<Int>): Int {
    val trailheads = atlas.toSequence().filter { (_, height) ->
        height == 0
    }.map { (pos, _) ->
        pos
    }
    return trailheads.sumOf { headPos ->
        var frontier = setOf(headPos)
        for (height in 1..9) {
            frontier = nextFrontier(atlas, frontier, height)
        }
        frontier.size
    }
}

private fun nextFrontier(atlas: Matrix<Int>, currFrontier: Set<Vec2>, nextHeight: Int): Set<Vec2> {
    return currFrontier.flatMap { currPos ->
        Vec2.CardinalDirection.entries.map { dir ->
            currPos + dir
        }.filter { nextPos ->
            atlas.inBounds(nextPos) && atlas[nextPos] == nextHeight
        }
    }
}

private fun <T, R> Set<T>.flatMap(transform: (T) -> Collection<R>): Set<R> {
    return this.fold(mutableSetOf()) { acc, el ->
        acc.addAll(transform(el))
        acc
    }
}

private fun part2(atlas: Matrix<Int>): Int {
    val summits = atlas.toSequence().filter { (_, height) ->
        height == 9
    }.map { (pos, _) ->
        pos
    }
    // multiset of all the locations at the given height that can reach a summit; the frequency of a location is the
    // number of paths from it to a summit
    var numPathsFromLocs = summits.toMultiset()
    for (nextHeight in 8 downTo 0) {
        numPathsFromLocs = numPathsFromLocs.multiMap { currPos ->
            Vec2.CardinalDirection.entries.map { dir ->
                currPos + dir
            }.filter { nextPos ->
                atlas.getOrDefault(nextPos, -1) == nextHeight
            }
        }
    }
    return numPathsFromLocs.size
}
