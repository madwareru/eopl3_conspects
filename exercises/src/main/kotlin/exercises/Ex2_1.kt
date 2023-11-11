package exercises

data class NatNumberEnvironment<T>(
    val zero: () -> T,
    val isZero: (T) -> Boolean,
    val succ: (T) -> T,
    val pred: (T) -> T
) {
    val toInt: (T) -> Int get() = { if (isZero(it)) { 0 } else { 1 + toInt(pred(it)) } }
    val plus: (T, T) -> T get() = { x, y ->
        if (isZero(y)) { x } else { plus(succ(x), pred(y)) }
    }
    val minus: (T, T) -> T get() = { x, y ->
        if (isZero(y)) { x } else { minus(pred(x), pred(y)) }
    }
    val multiply: (T, T) -> T get() = { x, y ->
        when {
            isZero(x) || isZero(y) -> zero()
            else -> {
                val predY = pred(y)
                when {
                    isZero(predY) -> x
                    else -> plus(x, multiply(x, predY))
                }
            }
        }
    }
}

sealed class UnaryRepr {
    data object Zero : UnaryRepr()
    data class Succ(val tail: UnaryRepr) : UnaryRepr()
}

val unaryNatNumber = NatNumberEnvironment<UnaryRepr>(
    zero = { UnaryRepr.Zero },
    isZero = { it is UnaryRepr.Zero },
    succ = { UnaryRepr.Succ(it) },
    pred = { (it as? UnaryRepr.Succ)?.tail ?: throw NoSuchElementException("called pred on zero!") }
)

val intNatNumber = NatNumberEnvironment(
    zero = { 0 },
    isZero = { it == 0 },
    succ = { it + 1 },
    pred = { if (it > 0) { it - 1} else { throw NoSuchElementException("called pred on zero!") } }
)

val mutableListNatNumber = NatNumberEnvironment(
    zero = { mutableListOf<Boolean>() },
    isZero = { it.isEmpty() },
    succ = { val copy = it.toMutableList(); copy.add(true); copy },
    pred = {
        if (it.isNotEmpty()) {
            val copy = it.toMutableList(); copy.removeLast(); copy
        } else {
            throw NoSuchElementException("called pred on zero!")
        }
    }
)

fun ex2_1() {
    fun<T> testEnv(env: NatNumberEnvironment<T>) {
        with(env) {
            fun factorial(it: T): T {
                return if (isZero(it) || isZero(pred(it))) {
                    it
                } else {
                    multiply(it, factorial(pred(it)))
                }
            }

            val x = zero(); println("${toInt(x)} is zero: ${isZero(x)}")
            val y = succ(x); println("${toInt(y)} is zero: ${isZero(y)}")
            val z = pred(y); println("${toInt(z)} is zero: ${isZero(z)}")
            val w = plus(plus(y, y), plus(y, y)); println("${toInt(w)} is zero: ${isZero(w)}")
            val a = minus(w, y); println("${toInt(a)} is zero: ${isZero(a)}")
            val b = minus(w, a); println("${toInt(b)} is zero: ${isZero(b)}")
            val c = multiply(w, a); println("${toInt(c)} is zero: ${isZero(c)}")

            val f0 = factorial(w); println("fact(${toInt(w)}) = ${toInt(f0)}")

            val six = pred(pred(pred(pred(pred(pred(c))))))

            val f = factorial(six); println("fact(${toInt(six)}) = ${toInt(f)}")
        }
    }

    println("unary repr")
    testEnv(unaryNatNumber)

    println()

    println("mutable list repr")
    testEnv(mutableListNatNumber)

    println()

    println("int repr")
    testEnv(intNatNumber)
}