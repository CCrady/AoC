import java.io.File
import kotlin.math.abs

private fun main() = solve("21", ::parse, parts(2), parts(25))

private fun parse(file: File): List<DoorCode> = file.readLines()

// TODO: document & rename this to be more understandable

private val directionalCodes = "0123456789A".toList().pairs().flatMap { (startButton, endButton) ->
    doorButtonsToDirectionalCodes(startButton, endButton)
}.toSet()
private val directionalCodeToDirectionalCodes = directionalCodes.associateWith { directionalCode ->
    directionalCode.mapSteps(::directionalButtonsToDirectionalCodes)
}
// This approach builds up a map of directional codes to the minimum number of manual keypresses required to input them
// one level at a time, starting from the manual input (level 0) and finishing at the robot that controls the robot that
// enters the door codes (level N).
private fun parts(numRobotsTypingOnDirectionalKeypads: Int): (List<DoorCode>) -> Long = { input ->
    val level0 = directionalCodes.associateWith { directionalCode -> directionalCode.size.toLong() }
    val levelN = repeatApply(numRobotsTypingOnDirectionalKeypads, level0) { prevLevel ->
        directionalCodeToDirectionalCodes.mapValues { (_, prevCodes) ->
            prevCodes.sumOf { options ->
                options.minOf { prevCode ->
                    prevLevel.getValue(prevCode)
                }
            }
        }
    }

    val doorCodeNumbers = input.map { doorCode -> doorCode.removeSuffix("A").toInt() }
    val finalCodeLengths = input.map { doorCode ->
        doorCode.mapSteps { startDoorButton, endDoorButton ->
            val possiblePaths = doorButtonsToDirectionalCodes(startDoorButton, endDoorButton)
            possiblePaths.minOf { possiblePath ->
                levelN.getValue(possiblePath)
            }
        }.sum()
    }

    (doorCodeNumbers zip finalCodeLengths).sumOf { (codeNumber, finalLength) ->
        codeNumber * finalLength
    }
}

private typealias DoorButton = Char
private typealias DoorCode = String
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
private typealias DirectionalCode = List<DirectionalButton>

private fun <R> DoorCode.mapSteps(transform: (DoorButton, DoorButton) -> R): List<R> {
    return ("A" + this).zipWithNext(transform)
}
private fun <R> DirectionalCode.mapSteps(transform: (DirectionalButton, DirectionalButton) -> R): List<R> {
    return (listOf(DirectionalButton.ACTIVATE) + this).zipWithNext(transform)
}

// The buttonsToDirectionalCodes family of functions takes a starting and ending button and returns the set of shortest
// sequences of directional button inputs that will take you from the start to the end position. This set contains
// either one or two elements; it contains two when both the horizontal and vertical positions of the start and end
// buttons differ, and there's no risk of passing over the gap when moving between them.

private fun doorButtonsToDirectionalCodes(
    startButton: DoorButton, endButton: DoorButton
): Set<DirectionalCode> {
    return buttonsToDirectionalCodes(startButton, endButton, ::doorButtonPos, Vec2(0, 3))
}

private fun directionalButtonsToDirectionalCodes(
    startButton: DirectionalButton, endButton: DirectionalButton
): Set<DirectionalCode> {
    return buttonsToDirectionalCodes(startButton, endButton, ::directionalButtonPos, Vec2(0, 0))
}

private fun <T> buttonsToDirectionalCodes(
    startButton: T, endButton: T,
    buttonPos: (T) -> Vec2,
    gapPos: Vec2
): Set<DirectionalCode> {
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
