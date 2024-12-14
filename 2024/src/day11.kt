import java.io.File
import java.math.BigInteger

fun main() = solve("11", ::parse, parts(25), parts(75))

private fun parse(file: File): List<BigInteger> {
    return file.readLines().first().split(' ').map(String::toBigInteger)
}

// The order of the pebbles doesn't matter, and two pebbles with the same number are indistinguishable, so we can use a
// multiset instead of a list. This is helpful for performance because at any given moment, most of the pebbles are
// probably going to have one of just a few different numbers.
// All 2- and 4-digit pebbles will split down to 1-digit pebbles after 1 or 2 blinks, and all 1-digit pebbles go in
// loops rather than exploding to infinity.

private fun parts(blinks: Int): (List<BigInteger>) -> BigInteger = { input ->
    var pebbles = input.toMultiset()
    repeat(blinks) {
        pebbles = pebbles.multiMap { pebble -> step(pebble) }
    }
    pebbles.size
}

private fun step(n: BigInteger): List<BigInteger> {
    if (n == BigInteger.ZERO) return listOf(BigInteger.ONE)
    val digits = numDigits(n)
    if (digits % 2 == 0) {
        val digitBreak = BigInteger.TEN.pow(digits / 2)
        return listOf(n / digitBreak, n % digitBreak)
    }
    return listOf(n * 2024.toBigInteger())
}