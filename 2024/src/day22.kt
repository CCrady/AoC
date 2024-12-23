import java.io.File

fun main() = solve("22", ::parse, ::part1, ::part2)

private fun parse(file: File): List<Int> = file.readLines().map(String::toInt)

private fun part1(input: List<Int>): Long {
    return input.sumOf { initial ->
        repeatApply(2000, initial, ::xorshift).toLong()
    }
}

private fun part2(input: List<Int>): Int {
    return input.asSequence().map(
        ::buySignalsToPrices
    ).fold(mutableMapOf<BuySignal, Int>().withDefault { 0 }) { acc, curr ->
        for ((buySignal, price) in curr) {
            acc[buySignal] = acc.getValue(buySignal) + price
        }
        acc
    }.values.max()
}

// For a given initial secret value, map each quadruple of consecutive price changes to the price the negotiator monkey
// will buy at.
private data class BuySignal(val a: Int, val b: Int, val c: Int, val d: Int)
private fun buySignalsToPrices(initial: Int): Map<BuySignal, Int> {
    val secretNumbers = generateSequence(initial, ::xorshift).take(2001)
    val prices = secretNumbers.map { n -> n % 10 }
    val priceChanges = prices.zipWithNext { a, b -> b - a }
    val firstPriceChanges = priceChanges.take(4).toList()
    val restPriceChanges = priceChanges.drop(4)
    val buySignals = restPriceChanges.runningFold(
        BuySignal(firstPriceChanges[0], firstPriceChanges[1], firstPriceChanges[2], firstPriceChanges[3])
    ) { (_, a, b, c), d ->
        BuySignal(a, b, c, d)
    }
    val buySignalsToPricesSequence = buySignals zip prices.drop(4)
    return buySignalsToPricesSequence.fold(mutableMapOf()) { acc, (buySignal, price) ->
        if (buySignal !in acc) acc[buySignal] = price
        acc
    }
}

// The number-updating algorithm is a xorshift PRNG with 24-bit words.
// https://en.wikipedia.org/wiki/Xorshift
// https://www.jstatsoft.org/article/view/v008i14/916
private const val truncateBits = (1 shl 24) - 1
private fun xorshift(n: Int): Int {
    var x = n
    x = x xor (x shl 6) and truncateBits
    x = x xor (x shr 5) // no need to re-zero the top bits
    x = x xor (x shl 11) and truncateBits
    return x
}
