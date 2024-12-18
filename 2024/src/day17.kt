import java.io.File

fun main() = solve("17", ::parse, ::part1, ::part2)

private data class ProgramState(val ip: Int, val regA: Long, val regB: Long, val regC: Long) {
    fun withIp(newIp: Int): ProgramState = ProgramState(newIp, regA, regB, regC)
    fun withA(newA: Long): ProgramState = ProgramState(ip, newA, regB, regC)
    fun withB(newB: Long): ProgramState = ProgramState(ip, regA, newB, regC)
    fun withC(newC: Long): ProgramState = ProgramState(ip, regA, regB, newC)
}
private typealias ProgramInstructions = List<Long>

private fun parse(file: File): Pair<ProgramState, ProgramInstructions> {
    val (regALine, regBLine, regCLine, _, instructionsLine) = file.readLines()
    val (regA) = """Register A: (\d+)""".toRegex().matchToInts(regALine)
    val (regB) = """Register B: (\d+)""".toRegex().matchToInts(regBLine)
    val (regC) = """Register C: (\d+)""".toRegex().matchToInts(regCLine)
    val instructionsString = """Program: ((?:\d,)*\d)""".toRegex().matchEntire(instructionsLine)!!.groups[1]!!.value
    val instructions = instructionsString.split(',').map(String::toLong)
    return Pair(ProgramState(0, regA.toLong(), regB.toLong(), regC.toLong()), instructions)
}


private fun part1(input: Pair<ProgramState, ProgramInstructions>): String {
    val (startState, instructions) = input
    return runProgram(startState, instructions).joinToString(",")
}

// This function can cope with all valid programs, including ones that do strange things like jumping to an odd
// instruction pointer.
private fun runProgram(startState: ProgramState, instructions: ProgramInstructions): Sequence<Long> = sequence {
    var currState = startState
    while (currState.ip < instructions.size) {
        val opcode = instructions[currState.ip]
        val operand = instructions[currState.ip + 1]
        currState = when (opcode) {
            0L -> division(ProgramState::withA, operand, currState)
            1L -> bxl(operand, currState)
            2L -> bst(operand, currState)
            3L -> jnz(operand, currState)
            4L -> bxc(operand, currState)
            5L -> {
                yield(getCombo(operand, currState) % 8)
                currState
            }
            6L -> division(ProgramState::withB, operand, currState)
            7L -> division(ProgramState::withC, operand, currState)
            else -> throw IllegalStateException("Invalid opcode $opcode")
        }
        currState = currState.withIp(currState.ip + 2)
    }
}


// Because the answers are so large (> Int.MAX_VALUE), it's infeasible to check each value of A individually. Instead,
// this approach requires the program to be structured in such a way that the initial value of A acts as a bit-string
// made of 3-bit words, and the program iterates over those words from least to most significant. See below for more
// detailed requirements and assumptions.
//
// As an example of the kind of analysis that went into this solution, take the following program:
//     bdv 0, cdv 1, bxc, out B, adv 3, jnz 0
// This is a while loop, which iterates until the value of the A register is 0. On each iteration it outputs the value
// (X ^ (X >> 1)) % 8 and sets the value of the A register to X >> 3 (where X is the value of the A register at the
// start of the iteration). Therefore, given a starting value of the A register, we can find the program's output by
// repeatedly calculating (A ^ (A >> 1)) % 8 and setting A = A >> 3, stopping once A is 0. Because we shift A to the
// right by three bits on each iteration, if we know the value of A on a given iteration there are only 8 easy-to-find
// possibilities for the value of A on the previous iteration. Also, we only need the value of A at the start of an
// iteration to know what's output on that iteration. Using this knowledge we can do a recursive tree search similar to
// the one from day 7, where we start from the final iteration and work our way backwards, cutting off branches when
// they become nonviable.
private fun part2(input: Pair<ProgramState, ProgramInstructions>): Long? {
    val (_, instructions) = input
    val instructionsChunked = instructions.chunked(2) { (opcode, operand) -> Pair(opcode, operand) }
    // the program must end with `jnz 0`, so that the entire thing is a while loop on the value of the A register
    require(instructionsChunked.last() == Pair(3L, 0L))
    // the program must not have any other `jnz` instructions, so that each iteration of the while loop is a simple
    // series of register mutations
    require(!instructionsChunked.dropLastView().any { (opcode, _) -> opcode == 3L })
    // the program must have exactly one `out` instruction, so that it outputs exactly one number on each iteration of
    // the while loop
    require(instructionsChunked.count { (opcode, _) -> opcode == 5L } == 1)
    // the program must have `adv` instructions that, taken together, divide the A register by 8 (i.e. shift it right by
    // 3 bits) on each iteration of the while loop
    require(instructionsChunked.filter { (opcode, _) -> opcode == 0L }.sumOf { (_, operand) -> operand } == 3L)
    // the program must also not keep the values of the B and C registers around between loop iterations, so that which
    // number is output on an iteration depends solely on the value of A at the start of the iteration. this is more
    // difficult to test for programmatically, so I've skipped using another require() here.

    return findQuine(instructions)
}

// Find the minimum initial value of A that produces a quine, or null if no such value exists.
private fun findQuine(instructions: ProgramInstructions): Long? {
    // Find the minimum value of A where, when the program is run with its A register initially set to this value,
    // 1. the first N numbers the program outputs will be targetOutput, and
    // 2. at the end of the Nth iteration/start of the N+1th iteration, the value of the A register will be nextA.
    // If no such value of A exists return null.
    fun minimumSatisfyingAOrNull(targetOutput: List<Long>, nextA: Long): Long? {
        // each recursive depth corresponds to one of the iterations of the while loop, and currWord is the associated
        // 3-bit word of A
        if (targetOutput.isEmpty()) return nextA

        val currTarget = targetOutput.last()
        val restTargets = targetOutput.dropLastView()
        // search the possible words for this iteration from least to greatest, so that the first starting value of A
        // we come across that satisfies the conditions is also the minimum one
        for (currWord in 0L..7L) {
            // the starting value of A for this iteration
            val currA = nextA * 8 + currWord
            // if the starting value of A during this iteration were zero, then the ending value of A would've been 0
            // during the previous iteration, and the while loop would've ended
            if (currA == 0L) continue
            // if the given value of A produces the wrong output, then we're on the wrong track
            if (iterationOutput(instructions, currA) != currTarget) continue
            val satisfyingA = minimumSatisfyingAOrNull(restTargets, currA)
            // if we've found a starting value of A that satisfies the conditions, then no need to search further
            if (satisfyingA != null) return satisfyingA
        }
        // if none of the possible words for this iteration worked, then backtrack
        return null
    }

    return minimumSatisfyingAOrNull(instructions, 0)
}

// Given a program's instructions and the initial value of the A register, find the number it outputs on the first
// iteration of the while loop. This will produce the same result as taking the first value yielded by runProgram(), but
// it has the added benefit that if our extra assumptions don't hold it will fail loudly instead of silently.
private fun iterationOutput(instructions: ProgramInstructions, initialA: Long): Long {
    var currState = ProgramState(0, initialA, 0, 0)
    for ((opcode, operand) in instructions.chunked(2)) {
        currState = when (opcode) {
            0L -> division(ProgramState::withA, operand, currState)
            1L -> bxl(operand, currState)
            2L -> bst(operand, currState)
            3L -> throw IllegalStateException("Encountered a jnz instruction before the first out instruction")
            4L -> bxc(operand, currState)
            5L -> return getCombo(operand, currState) % 8
            6L -> division(ProgramState::withB, operand, currState)
            7L -> division(ProgramState::withC, operand, currState)
            else -> throw IllegalStateException("Invalid opcode $opcode")
        }
    }
    throw IllegalStateException("No out instruction (opcode 5) in the program")
}


private fun getCombo(op: Long, state: ProgramState): Long {
    return when (op) {
        in 0L..3L -> op
        4L -> state.regA
        5L -> state.regB
        6L -> state.regC
        else -> throw IllegalArgumentException("Invalid combo operand $op")
    }
}

// Perform the division instruction (adv, bdv, or cdv) given the corresponding property mutator (withA, withB, or
// withC).
private inline fun division(
    mutator: ProgramState.(Long) -> ProgramState, op: Long, state: ProgramState
): ProgramState {
    return state.mutator(state.regA shr getCombo(op, state).toInt())
}

private fun bxl(op: Long, state: ProgramState): ProgramState {
    return state.withB(state.regB xor op)
}

private fun bxc(@Suppress("UNUSED_PARAMETER") op: Long, state: ProgramState): ProgramState {
    return bxl(state.regC, state)
}

private fun bst(op: Long, state: ProgramState): ProgramState {
    return state.withB(getCombo(op, state) % 8)
}

private fun jnz(op: Long, state: ProgramState): ProgramState {
    return if (state.regA == 0L) {
        state
    } else {
        // subtract 2 so that when we later add 2 the instruction pointer ends up at op
        state.withIp(op.toInt() - 2)
    }
}
