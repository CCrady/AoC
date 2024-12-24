import java.io.File
import kotlin.math.abs

private fun main() = solve("21", ::parse, ::part1)

private fun parse(file: File): List<String> = file.readLines()

// TODO: document & rename this to be more understandable

private fun part1(input: List<String>): Int {
    val doorCodeNumbers = input.map { doorCode -> doorCode.removeSuffix("A").toInt() }
    val finalDirectionalCodeLengths = input.map { doorCode ->
        ("A" + doorCode).zipWithNext { startButton, endButton ->
            val possibleResultSequences = doorButtonsToDirectionalCodes(startButton, endButton)
            possibleResultSequences.minOf { directionalCode ->
                shortestDirectionalCodeLength(2, directionalCode)
            }
        }.sum()
    }
    return (doorCodeNumbers zip finalDirectionalCodeLengths).sumOf { (codeNumber, finalLength) ->
        codeNumber * finalLength
    }
}

private fun shortestDirectionalCodeLength(iterations: Int, directionalCode: List<DirectionalButton>): Int {
    if (iterations == 0) return directionalCode.size

    return (listOf(DirectionalButton.ACTIVATE) + directionalCode).zipWithNext { startButton, endButton ->
        val possibleHigherCodes = directionalButtonsToDirectionalCodes(startButton, endButton)
        possibleHigherCodes.minOf { higherCode ->
            shortestDirectionalCodeLength(iterations - 1, higherCode)
        }
    }.sum()
}


private typealias DoorButton = Char
private enum class DirectionalButton {
    RIGHT,
    DOWN,
    LEFT,
    UP,
    ACTIVATE;

    override fun toString(): String = when (this) {
        RIGHT -> ">"
        DOWN  -> "v"
        LEFT  -> "<"
        UP    -> "^"
        ACTIVATE -> "A"
    }
}

// The buttonsToDirectionalCodes family of functions takes a starting and ending button and returns the set of shortest
// sequences of directional button inputs that will take you from the start to the end position. This set contains
// either one or two elements; it contains two when both the horizontal and vertical positions of the start and end
// buttons differ, and there's no risk of passing over the gap when moving between them.

private fun doorButtonsToDirectionalCodes(
    startButton: DoorButton, endButton: DoorButton
): Set<List<DirectionalButton>> {
    return buttonsToDirectionalCodes(startButton, endButton, ::doorButtonPos, Vec2(0, 3))
}

private fun directionalButtonsToDirectionalCodes(
    startButton: DirectionalButton, endButton: DirectionalButton
): Set<List<DirectionalButton>> {
    return buttonsToDirectionalCodes(startButton, endButton, ::directionalButtonPos, Vec2(0, 0))
}

private fun <T> buttonsToDirectionalCodes(
    startButton: T, endButton: T,
    buttonPos: (T) -> Vec2,
    gapPos: Vec2
): Set<List<DirectionalButton>> {
    val startPos = buttonPos(startButton)
    val endPos = buttonPos(endButton)
    val move = endPos - startPos
    val horizontalPresses = listOfHorizontalPresses(move)
    val verticalPresses = listOfVerticalPresses(move)
    // we need to order the horizontal and vertical movement so that we avoid the bottom-left corner
    val canMoveHorizontalFirst = horizontalPresses.isNotEmpty() && !(startPos.y == gapPos.y && endPos.x == gapPos.x)
    val canMoveVerticalFirst = verticalPresses.isNotEmpty() && !(startPos.x == gapPos.x && endPos.y == gapPos.y)
    // depending on the direction of the motion vector and whether we could hit the gap, we could have either one or two
    // possibilities
    val possibleOrders = when {
        canMoveHorizontalFirst && canMoveVerticalFirst -> setOf(
            horizontalPresses + verticalPresses,
            verticalPresses + horizontalPresses
        )
        canMoveHorizontalFirst -> setOf(horizontalPresses + verticalPresses)
        canMoveVerticalFirst   -> setOf(verticalPresses + horizontalPresses)
        else -> setOf(listOf())
    }
    return possibleOrders.map { it + DirectionalButton.ACTIVATE }.toSet()
}

private fun listOfHorizontalPresses(move: Vec2): List<DirectionalButton> {
    val direction = if (move.x > 0) DirectionalButton.RIGHT else DirectionalButton.LEFT
    return List(abs(move.x)) { direction }
}

private fun listOfVerticalPresses(move: Vec2): List<DirectionalButton> {
    val direction = if (move.y > 0) DirectionalButton.DOWN else DirectionalButton.UP
    return List(abs(move.y)) { direction }
}

private fun doorButtonPos(button: DoorButton): Vec2 = when (button) {
    in '1'..'9' -> {
        val buttonNum = button - '1'
        Vec2(buttonNum % 3, 2 - buttonNum / 3)
    }
    '0' -> Vec2(1, 3)
    'A' -> Vec2(2, 3)
    else -> throw IllegalArgumentException("Door code contained illegal character '$button'")
}

private fun directionalButtonPos(button: DirectionalButton): Vec2 = when (button) {
    DirectionalButton.UP       -> Vec2(1, 0)
    DirectionalButton.RIGHT    -> Vec2(2, 1)
    DirectionalButton.DOWN     -> Vec2(1, 1)
    DirectionalButton.LEFT     -> Vec2(0, 1)
    DirectionalButton.ACTIVATE -> Vec2(2, 0)
}
