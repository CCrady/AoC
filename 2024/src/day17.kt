import java.io.File

// TODO: clean this up :P

fun main() = solve("17", ::parse, ::part1, ::part2)

private data class ProgramState(val ip: Long, val regA: Long, val regB: Long, val regC: Long) {
    fun withIp(newIp: Long): ProgramState = ProgramState(newIp, regA, regB, regC)
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

// My input: 2,4, 1,7, 7,5, 1,7, 0,3, 4,1, 5,5, 3,0
// => bst A, bxl 7, cdv B, bxl 7, adv 3, bxc, out B, jnz 0
// there is exactly one jump instruction, and it's at the end pointing back to the start. the program is therefore a
// simple while loop.
//
// Start:
// A a, B b, C c
// -- bst A ->
// A a, B a % 8, C c
// -- bxl 7 ->
// A a, B (a % 8) ^ 7, C c
// A a, B 7 - (a % 8), C c
// -- cdv B ->
// A a, B 7 - (a % 8), C a >> (7 - (a % 8))
// -- bxl 7 ->
// A a, B a % 8, C a >> (7 - (a % 8))
// -- adv 3 ->
// A a >> 3, B a % 8, C a >> (7 - (a % 8))
// -- bxc ->
// A a >> 3, B (a % 8) ^ (a >> (7 - (a % 8))), C a >> (7 - (a % 8))
// -- out B ->
// print ((a % 8) ^ (a >> (7 - (a % 8)))) % 8
// -- jnz 0 ->
// jump back to the start, with a = a >> 3
//
// This program will, at each step, set the values of B and C depending on the last 3 bits of A, and then replace A with
// A >> 3. So it ends up working its way through the original value of A, 3 bits at a time, starting with the least
// significant bits and going up. The program is 16 numbers long, so the starting value of A is effectively a 48-bit
// bitstring. The one hiccup is that the number that gets printed also depends on the rest of the bitstring that hasn't
// been consumed yet, so it's not as easy as just mapping 3-bit input words to 3-bit output numbers.
//
// It might be helpful to start from the end. The last number we output is 0, and on the last loop a >> 3 must be 0 in
// order for the program to terminate. So on the last loop, ((a % 8) ^ (a >> (7 - (a % 8)))) % 8 == 0, but a != 0
// because otherwise the program would've already terminated on the previous loop. Therefore a is in the range 1..7.

private fun part2(input: Pair<ProgramState, ProgramInstructions>): Long {
    return recursiveSearch(listOf(2,4,1,7,7,5,1,7,0,3,4,1,5,5,3,0))
}

private fun recursiveSearch(targetOutput: List<Long>): Long {
    // TODO: Modify this function to work with other input programs. We should still assume that the program is of the
    //       form "..., adv 3, ..., out [B/C], ..., jnz 0", i.e. it's a while loop on the value of A, the value of A
    //       gets shifted 3 bits to the right each iteration, and exactly one number is output each iteration. We should
    //       also assume that which number gets output during a given iteration depends solely on the value of A at the
    //       start of the iteration, i.e. the values of B and C at the end of an iteration get clobbered during the next
    //       iteration and don't impact the result.
    fun outputFromA(a: Long): Long {
        // SSA form of one iteration of the program
        val b1 = a % 8
        val b2 = b1 xor 7
        val c3 = a shr b2.toInt()
        val b4 = b1 // b1 xor 7 xor 7
        val b5 = b4 xor c3
        return b5 % 8
    }

    // TODO: If we just want the minimum, we can take only the first element of the sequence, because we start with the
    //       lowest possible value of currWord at each depth. This function could therefore be changed to return an Int?
    //       instead of a sequence.
    fun recursive(targetOutput: List<Long>, nextA: Long): Sequence<Long> = sequence {
        if (targetOutput.isEmpty()) {
            yield(nextA)
            return@sequence
        }
        val currTarget = targetOutput.last()
        val restTargets = targetOutput.dropLastView()
        for (currWord in 0L..7L) {
            val currA = nextA * 8 + currWord
            if (currA == 0L) continue
            if (outputFromA(currA) != currTarget) continue
            yieldAll(recursive(restTargets, currA))
        }
    }

    return recursive(targetOutput, 0).min()
}


private fun runProgram(startState: ProgramState, instructions: ProgramInstructions): Sequence<Long> = sequence {
    var currState = startState
    while (currState.ip < instructions.size) {
        val opcode = instructions[currState.ip.toInt()]
        val operand = instructions[currState.ip.toInt() + 1]
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
        state.withIp(op - 2)
    }
}
