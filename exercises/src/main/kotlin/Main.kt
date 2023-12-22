import exercises.*
import java.io.BufferedReader
import java.io.InputStreamReader

val exerciseLookup = hashMapOf(
    "2.1" to ::ex2_1,
    "2.1 v2" to ::ex2_1_v2,
    "2.5" to ::ex2_5,
    "2.5 v2" to ::ex2_5_v2,
    "2.15" to ::ex2_15,
    "2.15 v2" to ::ex2_15_v2,
    "2.29" to ::ex2_29,
    "2.29 v2" to ::ex2_29_v2,
    "3.let" to ::ex3_let_interpreter,
    "3.let proc" to ::ex3_let_proc_interpreter
)

fun main() {
    val input = InputStreamReader(System.`in`)
    val reader = BufferedReader(input)

    println("please insert exercise name:")
    val name = reader.readLine()
    exerciseLookup[name]?.invoke() ?: println("ex $name not found!")
}