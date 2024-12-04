import java.io.File

fun <T> solve(day: String, parse: (File) -> T, part1: ((T) -> Int)? = null, part2: ((T) -> Int)? = null) {
    val testData = parse(File("test_input/$day.txt"))
    val inputData = parse(File("input/$day.txt"))
    if (part1 != null) {
        println("""
            part 1 test: ${ part1(testData) }
            part 1 real: ${ part1(inputData) }
        """.trimIndent())
    }
    if (part2 != null) {
        println("""
            part 2 test: ${ part2(testData) }
            part 2 real: ${ part2(inputData) }
        """.trimIndent())
    }
}

data class Vec2(val x: Int, val y: Int) {
    operator fun unaryPlus(): Vec2 = this
    operator fun unaryMinus(): Vec2 = Vec2(-x, -y)
    operator fun plus(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
    operator fun minus(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
    operator fun times(other: Int): Vec2 = Vec2(x * other, y * other)
    override fun equals(other: Any?): Boolean = other is Vec2 && x == other.x && y == other.y

    enum class Direction(val vector: Vec2) {
        EAST(Vec2(1, 0)),
        SOUTHEAST(Vec2(1, 1)),
        SOUTH(Vec2(0, 1)),
        SOUTHWEST(Vec2(-1, 1)),
        WEST(Vec2(-1, 0)),
        NORTHWEST(Vec2(-1, -1)),
        NORTH(Vec2(0, -1)),
        NORTHEAST(Vec2(1, -1))
    }
}

class Matrix<E>(private val underlying: List<List<E>>) {
    val width: Int
        get() = underlying.first().size
    val height: Int
        get() = underlying.size
    fun at(x: Int, y: Int): E = underlying[y][x]
    fun at(pos: Vec2): E = at(pos.x, pos.y)
    fun inBounds(x: Int, y: Int): Boolean = y >= 0 && x >= 0 && y < underlying.size && x < underlying[y].size
    fun inBounds(pos: Vec2): Boolean = inBounds(pos.x, pos.y)

    data class IndexedValue<T>(val index: Vec2, val value: T)

    constructor(from: Iterable<Iterable<E>>) : this(from.map { row -> row.toList() }.toList())

    fun <T> map(f: (E) -> T): Matrix<T> = Matrix(underlying.map { row -> row.map(f) })

    fun toSequence(): Sequence<IndexedValue<E>> = sequence {
        for ((y, row) in underlying.withIndex()) {
            for ((x, element) in row.withIndex()) {
                yield(IndexedValue(Vec2(x, y), element))
            }
        }
    }

    operator fun iterator(): Iterator<IndexedValue<E>> = toSequence().iterator()
}
