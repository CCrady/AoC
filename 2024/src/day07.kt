import java.io.File
import kotlin.math.pow
import kotlin.math.roundToLong

fun main() = solve("07", ::parse, ::part1, ::part2)

private data class TestCase(val target: Long, val operands: List<Long>)

private fun parse(file: File): List<TestCase> {
    return file.readLines().map { str ->
        val numberStrs = str.split(' ')
        val target = numberStrs[0].dropLast(1).toLong()
        val operands = numberStrs.drop(1).map(String::toLong)
        TestCase(target, operands)
    }
}

private fun part1(input: List<TestCase>): Long {
    return input.asSequence().filter { (target, operands) ->
        recursiveSearch1(target, operands)
    }.sumOf { (target, _) -> target }
}

private fun recursiveSearch1(target: Long, operands: List<Long>): Boolean {
    if (target <= 0) return false
    if (operands.size == 1) return target == operands.first()
    val currOperand = operands.last()
    val restOperands = operands.dropLastView()
    return recursiveSearch1(target - currOperand, restOperands)
            || recursiveSearch1(exactDivision(target, currOperand), restOperands)
}

private fun part2(input: List<TestCase>): Long {
    return input.asSequence().filter { (target, operands) ->
        recursiveSearch2(target, operands)
    }.sumOf { (target, _) -> target }
}

private fun recursiveSearch2(target: Long, operands: List<Long>): Boolean {
    if (target <= 0) return false
    if (operands.size == 1) return target == operands.first()
    val currOperand = operands.last()
    val restOperands = operands.dropLastView()
    return recursiveSearch2(target - currOperand, restOperands)
            || recursiveSearch2(exactDivision(target, currOperand), restOperands)
            || recursiveSearch2(removeSuffix(target, currOperand), restOperands)
}

private fun exactDivision(numerator: Long, denominator: Long): Long {
    if (numerator % denominator != 0L) return -1
    return numerator / denominator
}

// remove the last (base 10) digits of n if they match suffix; otherwise return -1
// this will throw if suffix <= 0
private fun removeSuffix(n: Long, suffix: Long): Long {
    // the power of 10 at which to break the number
    val digitBreak = 10.0.pow(numDigits(suffix)).roundToLong()
    if (n % digitBreak != suffix) return -1
    return n / digitBreak
}
