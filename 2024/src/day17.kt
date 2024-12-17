import java.io.File

fun main() = solve("17", ::parse, ::part1)

private data class ProgramState(val ip: Int, val regA: Int, val regB: Int, val regC: Int) {
    fun withIp(newIp: Int): ProgramState = ProgramState(newIp, regA, regB, regC)
    fun withA(newA: Int): ProgramState = ProgramState(ip, newA, regB, regC)
    fun withB(newB: Int): ProgramState = ProgramState(ip, regA, newB, regC)
    fun withC(newC: Int): ProgramState = ProgramState(ip, regA, regB, newC)
}
private typealias ProgramInstructions = List<Int>

private fun parse(file: File): Pair<ProgramState, ProgramInstructions> {
    val (regALine, regBLine, regCLine, _, instructionsLine) = file.readLines()
    val (regA) = """Register A: (\d+)""".toRegex().matchToInts(regALine)
    val (regB) = """Register B: (\d+)""".toRegex().matchToInts(regBLine)
    val (regC) = """Register C: (\d+)""".toRegex().matchToInts(regCLine)
    val instructionsString = """Program: ((?:\d,)*\d)""".toRegex().matchEntire(instructionsLine)!!.groups[1]!!.value
    val instructions = instructionsString.split(',').map(String::toInt)
    return Pair(ProgramState(0, regA, regB, regC), instructions)
}

private fun part1(input: Pair<ProgramState, ProgramInstructions>): String {
    val (startState, instructions) = input
    return runProgram(startState, instructions).joinToString(",")
}

private fun runProgram(startState: ProgramState, instructions: ProgramInstructions): List<Int> {
    val output = ArrayDeque<Int>()
    var currState = startState
    while (currState.ip < instructions.size) {
        val opcode = instructions[currState.ip]
        val operand = instructions[currState.ip + 1]
        currState = when (opcode) {
            0 -> division(ProgramState::withA)
            1 -> ::bxl
            2 -> ::bst
            3 -> ::jnz
            4 -> ::bxc
            5 -> { op, state ->
                output.addLast(getCombo(op, state) % 8)
                state
            }
            6 -> division(ProgramState::withB)
            7 -> division(ProgramState::withC)
            else -> throw IllegalStateException("Invalid opcode $opcode")
        }(operand, currState)
        currState = currState.withIp(currState.ip + 2)
    }
    return output
}

private fun getCombo(op: Int, state: ProgramState): Int {
    return when (op) {
        in 0..3 -> op
        4 -> state.regA
        5 -> state.regB
        6 -> state.regC
        else -> throw IllegalArgumentException("Invalid combo operand $op")
    }
}

// Perform the division instruction (adv, bdv, or cdv) given the corresponding property mutator (withA, withB, or
// withC). Curried for better use alongside the other instruction functions.
private inline fun division(
    crossinline mutator: ProgramState.(Int) -> ProgramState
): (Int, ProgramState) -> ProgramState {
    return { op, state -> state.mutator(state.regA shr getCombo(op, state)) }
}

private fun bxl(op: Int, state: ProgramState): ProgramState {
    return state.withB(state.regB xor op)
}

private fun bxc(@Suppress("UNUSED_PARAMETER") op: Int, state: ProgramState): ProgramState {
    return bxl(state.regC, state)
}

private fun bst(op: Int, state: ProgramState): ProgramState {
    return state.withB(getCombo(op, state) % 8)
}

private fun jnz(op: Int, state: ProgramState): ProgramState {
    return if (state.regA == 0) {
        state
    } else {
        // subtract 2 so that when we later add 2 the instruction pointer ends up at op
        state.withIp(op - 2)
    }
}
