import java.io.File

fun main() = solve("24", ::parse, ::part1)

private data class LogicGate(
    val operator: (Boolean, Boolean) -> Boolean,
    val leftOperand: String,
    val rightOperand: String
)

private typealias Input24 = Pair<Map<String, Boolean>, Map<String, LogicGate>>

private fun parse(file: File): Input24 {
    val lines = file.readLines()
    val inputWireLines = lines.takeWhile(String::isNotBlank)
    val inputMap = inputWireLines.associate { line ->
        val (wire, signal) = """([xy]\d\d): ([01])""".toRegex().matchEntire(line)!!.destructured
        Pair(wire, signal == "1")
    }
    val gateLines = lines.takeLastWhile(String::isNotBlank)
    val gateMap = gateLines.associate { line ->
        val (leftOperand, operatorName, rightOperand, output) =
            """(\S{3}) (OR|AND|XOR) (\S{3}) -> (\S{3})""".toRegex().matchEntire(line)!!.destructured
        val operator = when (operatorName) {
            "OR"  -> Boolean::or
            "AND" -> Boolean::and
            "XOR" -> Boolean::xor
            else  -> throw IllegalStateException("Illegal operator name $operatorName")
        }
        val gate = LogicGate(operator, leftOperand, rightOperand)
        Pair(output, gate)
    }
    return Pair(inputMap, gateMap)
}

private fun part1(input: Input24): Long {
    val (inputWires, gates) = input
    val wireValue = Memoize { wire ->
        if (wire in inputWires) {
            inputWires.getValue(wire)
        } else {
            val (operator, leftOperand, rightOperand) = gates.getValue(wire)
            operator(this(leftOperand), this(rightOperand))
        }
    }

    return (0..46).sumOf { wireNum ->
        val wire = "z${ wireNum.toString().padStart(2, '0') }"
        if (wire in gates && wireValue(wire)) {
            1L shl wireNum
        } else 0
    }
}
