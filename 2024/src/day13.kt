import java.io.File
import java.math.BigInteger

fun main() = solve("13", ::parse, ::part1, ::part2)

private data class ClawMachine(val a: BigVec2, val b: BigVec2, val prize: BigVec2)

// Linear algebra tells us that unless the A and B vectors are pointing in the same direction, there's only one way to
// decompose the prize vector into a linear combination of A and B. Therefore we don't need to search through the
// combinations.

private fun parse(file: File): List<ClawMachine> {
    fun matchToBigVec2(match: MatchResult): BigVec2 {
        val (x, y) = match.destructured
        return BigVec2(x.toBigInteger(), y.toBigInteger())
    }
    return file.readLines().chunked(4) { lines ->
        val (aLine, bLine, prizeLine) = lines
        val aVec = matchToBigVec2("""Button A: X\+(\d+), Y\+(\d+)""".toRegex().matchEntire(aLine)!!)
        val bVec = matchToBigVec2("""Button B: X\+(\d+), Y\+(\d+)""".toRegex().matchEntire(bLine)!!)
        val prizeVec = matchToBigVec2("""Prize: X=(\d+), Y=(\d+)""".toRegex().matchEntire(prizeLine)!!)
        ClawMachine(aVec, bVec, prizeVec)
    }
}

// Both parts can be solved with the same technique, but part 2 requires a larger integer type, so to be DRYer we just
// do both parts with BigIntegers.
private fun part1(machines: List<ClawMachine>): BigInteger = machines.sumOf { (aVec, bVec, prizeVec) ->
    // make sure that there's only one way to get to the prize
    require(!aVec.isParallelTo(bVec))
    // solving the system of equations aTimes * aVec + bTimes * bVec = prizeVec
    val aTimes = (prizeVec.y * bVec.x - prizeVec.x * bVec.y) / (aVec.y * bVec.x - aVec.x * bVec.y)
    val bTimes = (prizeVec.x - aTimes * aVec.x) / bVec.x
    // We can only push the buttons a positive integer number of times. If we got an answer where one of the numbers is
    // negative then it won't work for our purposes. If the numbers don't satisfy the system of equations it means that
    // there was a rounding error; in the ideal case one or both of the numbers would be fractional, but fractional
    // times won't work for our purposes either.
    if (aTimes >= BigInteger.ZERO && bTimes >= BigInteger.ZERO && aVec * aTimes + bVec * bTimes == prizeVec)
        3.toBigInteger() * aTimes + bTimes
    else
        0.toBigInteger()
}

private fun part2(machines: List<ClawMachine>) = part1(machines.map { (a, b, prize) ->
    val prizeOffset = BigVec2(10000000000000.toBigInteger(), 10000000000000.toBigInteger())
    ClawMachine(a, b, prize + prizeOffset)
})
