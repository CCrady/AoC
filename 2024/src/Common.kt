import java.io.File
import java.math.BigInteger
import kotlin.math.abs
import kotlin.math.log10
import kotlin.math.sign

fun <T> solve(day: String, parse: (File) -> T, part1: ((T) -> Number)? = null, part2: ((T) -> Number)? = null) {
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

fun numDigits(n: Number): Int = (log10(n.toDouble()) + 1.0).toInt()

tailrec fun gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
tailrec fun gcd(a: BigInteger, b: BigInteger): BigInteger = if (b == BigInteger.ZERO) a else gcd(b, a % b)

fun <T, R> Set<T>.flatSetMap(transform: (T) -> Collection<R>): Set<R> {
    return this.fold(mutableSetOf()) { acc, el ->
        acc.addAll(transform(el))
        acc
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

    fun inBounds(min: Vec2, max: Vec2): Boolean = x in min.x..<max.x && y in min.y..<max.y
    fun inSameMooreDirection(other: Vec2): Boolean = x.sign == other.x.sign && y.sign == other.y.sign

    companion object {
        val ZERO = Vec2(0, 0)
    }

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

// Kotlin's OO-style parametric polymorphism means we have to duplicate the Vec2 code here :(
data class BigVec2(val x: BigInteger, val y: BigInteger) {
    operator fun unaryPlus(): BigVec2 = this
    operator fun unaryMinus(): BigVec2 = BigVec2(-x, -y)
    operator fun plus(other: BigVec2): BigVec2 = BigVec2(x + other.x, y + other.y)
    operator fun minus(other: BigVec2): BigVec2 = BigVec2(x - other.x, y - other.y)
    operator fun times(other: BigInteger): BigVec2 = BigVec2(x * other, y * other)

    fun isParallelTo(other: BigVec2): Boolean {
        val commonXDivisor = gcd(this.x, other.x)
        val commonYDivisor = gcd(this.y, other.y)
        return this.x / commonXDivisor == this.y / commonYDivisor
                && other.x / commonXDivisor == other.y / commonYDivisor
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
    fun getOrDefault(x: Int, y: Int, default: E): E = if (inBounds(x, y)) this[x, y] else default
    fun getOrDefault(pos: Vec2, default: E): E = getOrDefault(pos.x, pos.y, default)

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

    fun toMutable(): MutableMatrix<E> {
        return MutableMatrix(underlying.map { row ->
            row.toMutableList()
        }.toMutableList())
    }
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

    fun mapInPlace(transform: (Vec2, E) -> E): MutableMatrix<E> {
        for ((y, row) in underlying.withIndex()) {
            for (x in row.indices) {
                row[x] = transform(Vec2(x, y), row[x])
            }
        }
        return this
    }
}

@JvmInline
value class Multiset<E>(private val underlying: Map<E, BigInteger>) {
    val size: BigInteger
        get() = underlying.values.fold(BigInteger.ZERO, BigInteger::plus)
    val entries: Set<Map.Entry<E, BigInteger>>
        get() = underlying.entries
    operator fun get(el: E) = underlying[el]
    operator fun contains(el: E) = underlying.containsKey(el)

    fun toSet(): Set<E> = underlying.keys

    constructor(): this(mapOf<E, BigInteger>().withDefault { BigInteger.ZERO })
    constructor(from: Sequence<E>): this(
        mutableMapOf<E, BigInteger>().withDefault { BigInteger.ZERO }.also { underlying ->
            for (el in from) {
                underlying[el] = underlying.getValue(el) + BigInteger.ONE
            }
        }
    )
    constructor(from: Iterable<E>): this(from.asSequence())

    // this is the flatmap of the multiset monad, but the name "flatMap" is already in use
    fun <R> multiMap(transform: (E) -> Iterable<R>): Multiset<R> {
        val newUnderlying = mutableMapOf<R, BigInteger>().withDefault { BigInteger.ZERO }
        for ((preimage, num) in underlying) {
            for (image in transform(preimage)) {
                newUnderlying[image] = newUnderlying.getValue(image) + num
            }
        }
        return Multiset(newUnderlying)
    }
}

fun <E> Sequence<E>.toMultiset(): Multiset<E> = Multiset(this)
fun <E> Iterable<E>.toMultiset(): Multiset<E> = Multiset(this)
fun <E> Set<E>.toMultiset(): Multiset<E> = Multiset(
    this.associateWith { BigInteger.ONE }.withDefault { BigInteger.ZERO }
)
