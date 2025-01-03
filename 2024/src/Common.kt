import java.io.File
import java.math.BigInteger
import kotlin.math.abs
import kotlin.math.log10
import kotlin.math.sign


fun <T> solve(day: String, parse: (File) -> T, part1: ((T) -> Any?)? = null, part2: ((T) -> Any?)? = null) {
    fun parseOrNull(file: File): T? = if (file.isFile) parse(file) else null
    val testData  = parseOrNull(File("test_input/$day.txt"))
    val inputData = parseOrNull(File("input/$day.txt"))
    if (part1 != null) {
        if (testData != null)  println("part 1 test: ${ part1(testData) }")
        if (inputData != null) println("part 1 real: ${ part1(inputData) }")
    }
    if (part2 != null) {
        if (testData != null)  println("part 2 test: ${ part2(testData) }")
        if (inputData != null) println("part 2 real: ${ part2(inputData) }")
    }
}


fun numDigits(n: Number): Int = (log10(n.toDouble()) + 1.0).toInt()

tailrec fun gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
tailrec fun gcd(a: BigInteger, b: BigInteger): BigInteger = if (b == BigInteger.ZERO) a else gcd(b, a % b)

fun safeRem(a: Int, b: Int): Int {
    val rem = a % b
    return if (rem >= 0) rem else rem + b
}

fun countUp(start: Int = 0): Iterator<Int> = object: Iterator<Int> {
    var n = start
    override fun hasNext(): Boolean = true
    override fun next(): Int = n++
}

fun <T> repeatApply(times: Int, initial: T, f: (T) -> T): T {
    var curr = initial
    repeat(times) {
        curr = f(curr)
    }
    return curr
}

fun Regex.matchToInts(input: String): List<Int> = matchEntire(input)!!.groups.drop(1).map { group ->
    group!!.value.toInt()
}

fun <E> List<E>.dropFirstView(): List<E> = subList(1, size)
fun <E> List<E>.dropLastView(): List<E> = subList(0, size - 1)

fun <E> List<E>.pairs(): Sequence<Pair<E, E>> = sequence {
    for (a in this@pairs) {
        for (b in this@pairs) {
            yield(Pair(a, b))
        }
    }
}

fun <E> List<E>.distinctUnorderedPairs(): Sequence<Pair<E, E>> = sequence {
    for ((i, a) in this@distinctUnorderedPairs.withIndex()) {
        for (j in (i+1)..<this@distinctUnorderedPairs.size) {
            val b = this@distinctUnorderedPairs[j]
            yield(Pair(a, b))
        }
    }
}

fun <T, R> Set<T>.flatSetMap(transform: (T) -> Collection<R>): Set<R> {
    return this.fold(mutableSetOf()) { acc, el ->
        acc.addAll(transform(el))
        acc
    }
}

fun <T> MutableSet<T>.poppingIterator(): Iterator<T> = object: Iterator<T> {
    override fun hasNext(): Boolean = this@poppingIterator.isNotEmpty()
    override fun next(): T {
        val popped = this@poppingIterator.first()
        this@poppingIterator.remove(popped)
        return popped
    }
}

// Wrapper for a function which performs memoization. The receiver on f is provided so that it can recurse in a memoized
// way by calling `this()`. Be cautious when wrapping functions that have side effects.
class Memoize<T, R>(private val f: Memoize<T, R>.(T) -> R): (T) -> R {
    private val memo = mutableMapOf<T, R>()
    override fun invoke(x: T): R = memo.getOrPut(x) { this.f(x) }
}


data class Vec2(val x: Int, val y: Int) {
    // magnitude of the vector, using the manhattan distance
    val mag: Int
        get() = abs(x) + abs(y)
    val sign: Vec2
        get() = Vec2(x.sign, y.sign)

    val turnCW: Vec2
        get() = Vec2(-y, x)
    val turnCCW: Vec2
        get() = Vec2(y, -x)
    val reverse: Vec2
        get() = Vec2(-x, -y)

    operator fun unaryPlus(): Vec2 = this
    operator fun unaryMinus(): Vec2 = Vec2(-x, -y)
    operator fun plus(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
    operator fun plus(other: MooreDirection): Vec2 = this + other.vector
    operator fun plus(other: CardinalDirection): Vec2 = this + other.vector
    operator fun minus(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
    operator fun minus(other: MooreDirection): Vec2 = this - other.vector
    operator fun minus(other: CardinalDirection): Vec2 = this - other.vector
    operator fun times(other: Int): Vec2 = Vec2(x * other, y * other)
    operator fun div(other: Int): Vec2 = Vec2(x / other, y / other)
    operator fun rem(other: Vec2) = Vec2(safeRem(x, other.x), safeRem(y, other.y))

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
        fun asChar(): Char = when (this) {
            EAST  -> '>'
            SOUTH -> 'v'
            WEST  -> '<'
            NORTH -> '^'
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
    open fun getValue(x: Int, y: Int): E = get(x, y)
    open fun getValue(pos: Vec2): E = get(pos)
    fun getOrDefault(x: Int, y: Int, default: E): E = if (inBounds(x, y)) this[x, y] else default
    fun getOrDefault(pos: Vec2, default: E): E = getOrDefault(pos.x, pos.y, default)

    override fun toString(): String {
        return underlying.joinToString("\n") { row -> row.joinToString("") }
    }

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
    companion object {
        fun fromLines(lines: List<String>): Matrix<Char> = Matrix(lines) { _, c -> c }
    }

    data class IndexedValue<T>(val index: Vec2, val value: T)

    fun <R> map(transform: (E) -> R): Matrix<R> {
        return Matrix(underlying.map { row ->
            row.map { el ->
                transform(el)
            }
        })
    }
    fun <R> mapIndexed(transform: (Vec2, E) -> R): Matrix<R> {
        return Matrix(underlying.mapIndexed { y, row ->
            row.mapIndexed { x, el ->
                transform(Vec2(x, y), el)
            }
        })
    }

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
        })
    }

    fun withDefault(defaultValue: (Vec2) -> E): Matrix<E> = object: Matrix<E>(underlying) {
        override fun getValue(x: Int, y: Int): E = if (inBounds(x, y)) get(x, y) else defaultValue(Vec2(x, y))
        override fun getValue(pos: Vec2): E = getValue(pos.x, pos.y)
    }
}

class MutableMatrix<E>(private val underlying: List<MutableList<E>>): Matrix<E>(underlying) {
    operator fun set(x: Int, y: Int, value: E) {
        underlying[y][x] = value
    }
    operator fun set(pos: Vec2, value: E) = set(pos.x, pos.y, value)

    constructor(size: Vec2, init: (Vec2) -> E): this(
        List(size.y) { y ->
            MutableList(size.x) { x ->
                init(Vec2(x, y))
            }
        }
    )

    fun mapInPlace(transform: (E) -> E): MutableMatrix<E> {
        for (row in underlying) {
            for (x in row.indices) {
                row[x] = transform(row[x])
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
    constructor(from: Iterable<E>): this(
        mutableMapOf<E, BigInteger>().withDefault { BigInteger.ZERO }.also { underlying ->
            for (el in from) {
                underlying[el] = underlying.getValue(el) + BigInteger.ONE
            }
        }
    )

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

fun <E> Sequence<E>.toMultiset(): Multiset<E> = Multiset(this.asIterable())
fun <E> Iterable<E>.toMultiset(): Multiset<E> = Multiset(this)
fun <E> Set<E>.toMultiset(): Multiset<E> = Multiset(
    this.associateWith { BigInteger.ONE }.withDefault { BigInteger.ZERO }
)


@JvmInline
value class UndirectedGraph<E>(private val nameToVertex: Map<E, Vertex<E>>) {
    open class Vertex<E>(val name: E, open val neighbors: Set<Vertex<E>>)

    val vertices: Collection<Vertex<E>>
        get() = nameToVertex.values
    operator fun get(name: E): Vertex<E>? = nameToVertex[name]
    operator fun iterator(): Iterator<Vertex<E>> = vertices.iterator()

    companion object {
        fun <E> fromAssociations(assocs: Iterable<Pair<E, E>>): UndirectedGraph<E> {
            class MutableVertex<E>(
                name: E, override val neighbors: MutableSet<Vertex<E>>
            ): Vertex<E>(name, neighbors)

            val nameToVertex = mutableMapOf<E, MutableVertex<E>>()
            fun getVertex(name: E) = nameToVertex.getOrPut(name) { MutableVertex(name, mutableSetOf()) }
            for ((name1, name2) in assocs) {
                val vertex1 = getVertex(name1)
                val vertex2 = getVertex(name2)
                vertex1.neighbors.add(vertex2)
                vertex2.neighbors.add(vertex1)
            }
            return UndirectedGraph(nameToVertex)
        }
    }
}
