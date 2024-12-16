import java.io.File
import java.math.BigInteger

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
        (1..9).fold(setOf(headPos)) { frontier, height ->
            nextFrontier(atlas, frontier, height)
        }.size
    }
}

private fun nextFrontier(atlas: Matrix<Int>, currFrontier: Set<Vec2>, nextHeight: Int): Set<Vec2> {
    return currFrontier.flatSetMap { currPos ->
        Vec2.CardinalDirection.entries.map { dir ->
            currPos + dir
        }.filter { nextPos ->
            atlas.inBounds(nextPos) && atlas[nextPos] == nextHeight
        }
    }
}

private fun part2(atlas: Matrix<Int>): BigInteger {
    val summits = atlas.toSequence().filter { (_, height) ->
        height == 9
    }.map { (pos, _) ->
        pos
    }
    val numPathsFromTrailheads = (8 downTo 0).fold(summits.toMultiset()) { numPathsFromLocs, nextHeight ->
        // numPathsFromLocs is a multiset of all the locations at the given height that can reach a summit; the
        // frequency of a location is the number of paths from it to a summit
        numPathsFromLocs.multiMap { currPos ->
            // the set of positions at the next height that are reachable from currPos
            Vec2.CardinalDirection.entries.map { dir ->
                currPos + dir
            }.filter { nextPos ->
                atlas.getOrDefault(nextPos, -1) == nextHeight
            }
        }
    }
    return numPathsFromTrailheads.size
}
