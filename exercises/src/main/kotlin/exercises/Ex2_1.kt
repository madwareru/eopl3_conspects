package exercises

data class NatNumberEnvironment<T>(
    val zero: () -> T,
    val isZero: (T) -> Boolean,
    val succ: (T) -> T,
    val pred: (T) -> T
) {
    val toInt: (T) -> Int get() = { if (isZero(it)) { 0 } else { 1 + toInt(pred(it)) } }
    val plus: (T, T) -> T get() = { x, y -> if (isZero(y)) { x } else { plus(succ(x), pred(y)) } }
    val minus: (T, T) -> T get() = { x, y -> if (isZero(y)) { x } else { minus(pred(x), pred(y)) } }
    val multiply: (T, T) -> T get() = { x, y ->
        when {
            isZero(x) || isZero(y) -> zero()
            isZero(pred(y)) -> x
            else -> plus(x, multiply(x, pred(y)))
        }
    }
}

data class UByteBigInt(val bytes: List<UByte>)

fun makeZeroBigNum() = UByteBigInt(listOf(UByte.MIN_VALUE))

fun incWithCarry(b: UByte): Pair<UByte, Boolean> =
    if (b < UByte.MAX_VALUE) {
        b.inc() to false
    } else {
        UByte.MIN_VALUE to true
    }

fun UByteBigInt.Succ(): UByteBigInt {
    val copyBytes = this.bytes.toMutableList()
    for (i in copyBytes.indices) {
        val (b, carry) = incWithCarry(copyBytes[i])
        copyBytes[i] = b

        if (!carry) {
            break
        }

        if (i == copyBytes.size-1) {
            copyBytes.add(1u)
        }
    }
    return UByteBigInt(copyBytes)
}

fun decWithCarry(b: UByte): Pair<UByte, Boolean>
    = if (b > UByte.MIN_VALUE) {
        b.dec() to false
    } else {
        UByte.MAX_VALUE to true
    }

fun UByteBigInt.IsZero(): Boolean = this.bytes.all { it == UByte.MIN_VALUE }

fun UByteBigInt.Pred(): UByteBigInt {
    if (this.IsZero()) {
        throw NoSuchElementException("called pred on zero!")
    }

    val copyBytes = this.bytes.toMutableList()
    for (i in copyBytes.indices) {
        val (b, carry) = decWithCarry(copyBytes[i])
        copyBytes[i] = b

        if (!carry) {
            break
        }
    }
    return UByteBigInt(copyBytes)
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

val listNatNumber = NatNumberEnvironment(
    zero = { listOf<Boolean>() },
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

val bigIntNatNumber = NatNumberEnvironment(
    zero = { makeZeroBigNum() },
    isZero = { it.IsZero() },
    succ = { it.Succ() },
    pred = { it.Pred() }
)

fun ex2_1() {
    fun<T> testEnv(env: () -> NatNumberEnvironment<T>): () -> Unit = {
        with(env()) {
            fun factorial(it: T): T =
                if (isZero(it) || isZero(pred(it))) {
                    it
                } else {
                    multiply(it, factorial(pred(it)))
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

    arrayOf(
        "unary repr" to testEnv { unaryNatNumber },
        "list repr" to testEnv { listNatNumber },
        "int repr" to testEnv { intNatNumber },
        "big int repr" to testEnv { bigIntNatNumber },
    ).forEach {
        val (label, testEnv) = it
        println("testing $label:")
        testEnv()
        println()
    }
}