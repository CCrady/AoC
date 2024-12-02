import java.io.File
import kotlin.math.abs

fun main() {
    solve("01", ::parse, ::part1, ::part2)
}

private fun parse(file: File): Pair<List<Int>, List<Int>> {
    return file.readLines().map { line ->
        val pattern = "(\\d+) +(\\d+)".toRegex()
        val match = pattern.matchEntire(line)
        requireNotNull(match)
        val (leftString, rightString) = match.destructured
        Pair(leftString.toInt(), rightString.toInt())
    }.unzip()
}

private fun part1(lists: Pair<List<Int>, List<Int>>): Int {
    val (leftList, rightList) = lists
    return (leftList.sorted() zip rightList.sorted()).sumOf { (left, right) -> abs(left - right) }
}

private fun part2(lists: Pair<List<Int>, List<Int>>): Int {
    val (leftList, rightList) = lists
    val occurrences = numOccurrences(rightList)
    return leftList.sumOf { location -> location * occurrences.getValue(location) }
}

private fun <E> numOccurrences(list: List<E>): Map<E, Int> {
    val map = mutableMapOf<E, Int>().withDefault { 0 }
    for (location in list) {
        map[location] = map.getValue(location) + 1
    }
    return map
}
