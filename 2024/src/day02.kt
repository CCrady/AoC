import java.io.File

fun main() = solve("02", ::parse, ::part1, ::part2)

private fun parse(file: File): List<List<Int>> {
    return file.readLines().map { line ->
        line.split(' ').map { entry ->
            entry.toInt()
        }
    }
}

private fun part1(input: List<List<Int>>): Int {
    return input.count(::isSafe)
}

private fun part2(input: List<List<Int>>): Int {
    return input.count(::isSafeWithDampener)
}

private fun isSafe(report: List<Int>): Boolean {
    val shouldIncrease = report[0] < report[1]
    val acceptableRange = if (shouldIncrease) 1..3 else -3..-1
    return report.zipWithNext().all { (left, right) ->
        right - left in acceptableRange
    }
}

private fun isSafeWithDampener(report: List<Int>): Boolean {
    return removeEach(report).any(::isSafe)
}

private fun <E> removeEach(list: List<E>): Sequence<List<E>> {
    return sequence {
        for (i in list.indices) {
            val leftHalf = list.slice(0..<i)
            val rightHalf = list.slice((i+1)..<list.size)
            yield(leftHalf + rightHalf)
        }
    }
}