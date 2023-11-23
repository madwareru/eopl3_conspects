package exercises

data class NatNumberScope<T>(
    val zero: () -> T,
    val isZero: (T) -> Boolean,
    val succ: (T) -> T,
    val pred: (T) -> T
) {
    val toInt: (T) -> Int get() = { isZero(it).guard { 0 }.unwrapOr { 1 + toInt(pred(it)) } }
    val plus: (T, T) -> T get() = { x, y -> isZero(y).guard { x }.unwrapOr { plus(succ(x), pred(y)) } }
    val minus: (T, T) -> T get() = { x, y -> isZero(y).guard { x }.unwrapOr { minus(pred(x), pred(y)) } }
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
    (b < UByte.MAX_VALUE)
        .guard { b.inc() to false }
        .unwrapOr { UByte.MIN_VALUE to true }

fun UByteBigInt.succImpl(): UByteBigInt {
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

fun decWithCarry(b: UByte): Pair<UByte, Boolean> =
    (b > UByte.MIN_VALUE)
        .guard { b.dec() to false }
        .unwrapOr { UByte.MAX_VALUE to true }

fun UByteBigInt.isZeroImpl(): Boolean = bytes.all { it == UByte.MIN_VALUE }

fun UByteBigInt.predImpl(): UByteBigInt =
    (!isZeroImpl())
        .guard {
            val copyBytes = bytes.toMutableList()
            for (i in copyBytes.indices) {
                val (b, carry) = decWithCarry(copyBytes[i])
                copyBytes[i] = b

                if (!carry) { break }
            }
            UByteBigInt(copyBytes)
        }.unwrapOr { throw NoSuchElementException("called pred on zero!") }

sealed class UnaryRepr {
    data object Zero : UnaryRepr()
    data class Succ(val tail: UnaryRepr) : UnaryRepr()
}

val unaryNatNumber = NatNumberScope<UnaryRepr>(
    zero = { UnaryRepr.Zero },
    isZero = { it is UnaryRepr.Zero },
    succ = { UnaryRepr.Succ(it) },
    pred = { it.tryCast<UnaryRepr.Succ>()
        .map { x -> x.tail }
        .unwrapOr { throw NoSuchElementException("called pred on zero!") }
    }
)

val intNatNumber = NatNumberScope(
    zero = { 0 },
    isZero = { it == 0 },
    succ = { it + 1 },
    pred = { if (it > 0) { it - 1} else { throw NoSuchElementException("called pred on zero!") } }
)

val listNatNumber = NatNumberScope(
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

val bigIntNatNumber = NatNumberScope(
    zero = { makeZeroBigNum() },
    isZero = { it.isZeroImpl() },
    succ = { it.succImpl() },
    pred = { it.predImpl() }
)

fun ex2_1() {
    fun<T> testScope(scope: () -> NatNumberScope<T>): () -> Unit = {
        with(scope()) {
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
        "unary repr" to testScope { unaryNatNumber },
        "list repr" to testScope { listNatNumber },
        "int repr" to testScope { intNatNumber },
        "big int repr" to testScope { bigIntNatNumber },
    ).forEach {
        val (label, testScope) = it
        println("testing $label:")
        testScope()
        println()
    }
}