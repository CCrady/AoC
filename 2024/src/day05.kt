import java.io.File

fun main() = solve("05", ::parse, ::part1, ::part2)

@JvmInline
private value class OrderRules(private val underlying: Map<Int, Set<Int>>): Comparator<Int> {
    // underlying maps a page number to the set of page numbers that must come after it

    fun isInOrder(before: Int, after: Int): Boolean = after in underlying.getValue(before)
    fun isInOrder(order: PageOrder): Boolean {
        for (i in order.indices) {
            val before = order[i]
            for (j in (i+1)..<order.size) {
                val after = order[j]
                if (!isInOrder(before, after)) return false
            }
        }
        return true
    }

    override fun compare(lhs: Int, rhs: Int): Int = if (isInOrder(lhs, rhs)) -1 else 1
}
private typealias PageOrder = List<Int>

private fun parse(file: File): Pair<OrderRules, List<PageOrder>> {
    val lines = file.readLines()
    val rulesLines = lines.takeWhile(String::isNotEmpty)
    val ordersLines = lines.takeLastWhile(String::isNotEmpty)
    return Pair(parseRules(rulesLines), parseOrders(ordersLines))
}

private fun parseRules(lines: List<String>): OrderRules {
    val map = mutableMapOf<Int, MutableSet<Int>>().withDefault { mutableSetOf() }
    for (line in lines) {
        val (before, after) = line.split("|").map(String::toInt)
        val set = map.getValue(before)
        set.add(after)
        map[before] = set
    }
    return OrderRules(map)
}

private fun parseOrders(lines: List<String>): List<PageOrder> = lines.map { line ->
    line.split(",").map(String::toInt)
}

private fun part1(input: Pair<OrderRules, List<PageOrder>>): Int {
    val (rules, orders) = input
    val validOrders = orders.filter { order -> rules.isInOrder(order) }
    return sumOfMiddles(validOrders)
}

private fun part2(input: Pair<OrderRules, List<PageOrder>>): Int {
    val (rules, orders) = input
    val invalidOrders = orders.filter { order -> !rules.isInOrder(order) }
    val fixedOrders = invalidOrders.map { order -> order.sortedWith(rules) }
    return sumOfMiddles(fixedOrders)
}

private fun sumOfMiddles(orders: List<PageOrder>): Int = orders.sumOf { order -> order[order.size / 2] }
