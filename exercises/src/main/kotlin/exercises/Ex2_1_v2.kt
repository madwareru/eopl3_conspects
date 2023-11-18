package exercises

interface NatNumberADT<TImpl> {
    fun zero(): TImpl
    fun isZero(num: TImpl): Boolean
    fun succ(num: TImpl): TImpl
    fun pred(num: TImpl): TImpl

    fun toInt(num: TImpl): Int = if (isZero(num)) { 0 } else { 1 + toInt(pred(num)) }
    fun plus(x: TImpl, y: TImpl): TImpl = if (isZero(y)) { x } else { plus(succ(x), pred(y)) }
    fun minus(x: TImpl, y: TImpl): TImpl = if (isZero(y)) { x } else { minus(pred(x), pred(y)) }
    fun multiply(x: TImpl, y: TImpl): TImpl =
        when {
            isZero(x) || isZero(y) -> zero()
            isZero(pred(y)) -> x
            else -> plus(x, multiply(x, pred(y)))
        }
}

val bigIntNatNumberADT = object : NatNumberADT<UByteBigInt> {
    override fun zero(): UByteBigInt = makeZeroBigNum()
    override fun isZero(num: UByteBigInt): Boolean = num.isZero()
    override fun pred(num: UByteBigInt): UByteBigInt = num.pred()
    override fun succ(num: UByteBigInt): UByteBigInt = num.succ()
}

val unaryNatNumberADT = object : NatNumberADT<UnaryRepr> {
    override fun zero() = UnaryRepr.Zero
    override fun isZero(num: UnaryRepr) = num is UnaryRepr.Zero
    override fun pred(num: UnaryRepr): UnaryRepr =
        (num as? UnaryRepr.Succ)?.tail ?: throw NoSuchElementException("called pred on zero!")
    override fun succ(num: UnaryRepr): UnaryRepr = UnaryRepr.Succ(num)
}

val intNatNumberADT = object : NatNumberADT<Int> {
    override fun zero(): Int = 0
    override fun isZero(num: Int): Boolean = num == 0
    override fun pred(num: Int): Int =
        if (num > 0) { num - 1} else { throw NoSuchElementException("called pred on zero!") }
    override fun succ(num: Int): Int = num + 1
}

val listNatNumberADT = object : NatNumberADT<List<Boolean>> {
    override fun zero(): List<Boolean> = listOf()
    override fun isZero(num: List<Boolean>): Boolean = num.isEmpty()
    override fun pred(num: List<Boolean>): List<Boolean> =
        if (num.isNotEmpty()) {
            val copy = num.toMutableList()
            copy.removeLast()
            copy
        } else {
            throw NoSuchElementException("called pred on zero!")
        }
    override fun succ(num: List<Boolean>): List<Boolean> {
        val copy = num.toMutableList()
        copy.add(true)
        return copy
    }
}

fun ex2_1_v2() {
    fun<T> testScope(scope: () -> NatNumberADT<T>): () -> Unit = {
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
        "unary repr" to testScope { unaryNatNumberADT },
        "list repr" to testScope { listNatNumberADT },
        "int repr" to testScope { intNatNumberADT },
        "big int repr" to testScope { bigIntNatNumberADT },
    ).forEach {
        val (label, testScope) = it
        println("testing $label:")
        testScope()
        println()
    }
}