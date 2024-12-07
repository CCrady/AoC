import java.io.File
import kotlin.math.abs

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
    // magnitude of the vector, using the manhattan distance
    val mag: Int
        get() = abs(x) + abs(y)

    operator fun unaryPlus(): Vec2 = this
    operator fun unaryMinus(): Vec2 = Vec2(-x, -y)
    operator fun plus(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
    operator fun plus(other: MooreDirection): Vec2 = this + other.vector
    operator fun plus(other: CardinalDirection): Vec2 = this + other.vector
    operator fun minus(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
    operator fun minus(other: MooreDirection): Vec2 = this - other.vector
    operator fun minus(other: CardinalDirection): Vec2 = this - other.vector
    operator fun times(other: Int): Vec2 = Vec2(x * other, y * other)

    enum class MooreDirection(val vector: Vec2) {
        EAST(Vec2(1, 0)),
        SOUTHEAST(Vec2(1, 1)),
        SOUTH(Vec2(0, 1)),
        SOUTHWEST(Vec2(-1, 1)),
        WEST(Vec2(-1, 0)),
        NORTHWEST(Vec2(-1, -1)),
        NORTH(Vec2(0, -1)),
        NORTHEAST(Vec2(1, -1))
    }
    
    enum class CardinalDirection(val vector: Vec2) {
        EAST(Vec2(1, 0)),
        SOUTH(Vec2(0, 1)),
        WEST(Vec2(-1, 0)),
        NORTH(Vec2(0, -1));

        val turnCW: CardinalDirection
            get() = when (this) {
                EAST -> SOUTH
                SOUTH -> WEST
                WEST -> NORTH
                NORTH -> EAST
            }
        val turnCCW: CardinalDirection
            get() = when (this) {
                EAST -> NORTH
                SOUTH -> EAST
                WEST -> SOUTH
                NORTH -> WEST
            }
        val reverse: CardinalDirection
            get() = when (this) {
                EAST  -> WEST
                SOUTH -> NORTH
                WEST  -> EAST
                NORTH -> SOUTH
            }
    }
}

open class Matrix<E>(private val underlying: List<List<E>>) {
    val width: Int
        get() = underlying.first().size
    val height: Int
        get() = underlying.size
    val size: Vec2
        get() = Vec2(width, height)
    fun inBounds(x: Int, y: Int): Boolean = y >= 0 && x >= 0 && y < height && x < width
    fun inBounds(pos: Vec2): Boolean = inBounds(pos.x, pos.y)

    operator fun get(x: Int, y: Int): E = underlying[y][x]
    operator fun get(pos: Vec2): E = get(pos.x, pos.y)

    constructor(size: Vec2, init: (Vec2) -> E) : this(
        List(size.y) { y ->
            List(size.x) { x ->
                init(Vec2(x, y))
            }
        }
    )
    constructor(lines: List<String>, transform: (Vec2, Char) -> E): this(
        Vec2(lines.first().length, lines.size),
        { pos -> transform(pos, lines[pos.y][pos.x]) }
    )

    data class IndexedValue<T>(val index: Vec2, val value: T)

    fun toSequence(): Sequence<IndexedValue<E>> = sequence {
        for ((y, row) in underlying.withIndex()) {
            for ((x, element) in row.withIndex()) {
                yield(IndexedValue(Vec2(x, y), element))
            }
        }
    }
    operator fun iterator(): Iterator<IndexedValue<E>> = toSequence().iterator()

    fun count(predicate: (E) -> Boolean) = underlying.sumOf { row -> row.count(predicate) }
}

class MutableMatrix<E>(private val underlying: MutableList<MutableList<E>>): Matrix<E>(underlying) {
    operator fun set(x: Int, y: Int, value: E) {
        underlying[y][x] = value
    }
    operator fun set(pos: Vec2, value: E) = set(pos.x, pos.y, value)

    constructor(size: Vec2, init: (Vec2) -> E): this(
        MutableList(size.y) { y ->
            MutableList(size.x) { x ->
                init(Vec2(x, y))
            }
        }
    )
    constructor(lines: List<String>, transform: (Vec2, Char) -> E): this(
        Vec2(lines.first().length, lines.size),
        { pos -> transform(pos, lines[pos.y][pos.x])}
    )

    fun mapInPlace(transform: (Vec2, E) -> E): MutableMatrix<E> {
        for ((y, row) in underlying.withIndex()) {
            for (x in row.indices) {
                row[x] = transform(Vec2(x, y), row[x])
            }
        }
        return this
    }
}
