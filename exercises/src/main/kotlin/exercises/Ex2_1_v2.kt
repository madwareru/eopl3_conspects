package exercises

interface NatNumberADT<TImpl> {
    fun zero(): TImpl
    fun TImpl.isZero(): Boolean
    fun TImpl.succ(): TImpl
    fun TImpl.pred(): TImpl

    fun TImpl.toInt(): Int = isZero()
        .guard { 0 }
        .unwrapOr { 1 + pred().toInt() }
    operator fun TImpl.plus(other: TImpl): TImpl = other.isZero()
        .guard { this }
        .unwrapOr { succ() + other.pred() }
    operator fun TImpl.minus(other: TImpl): TImpl = other.isZero()
        .guard { this }
        .unwrapOr { pred() - other.pred() }
    operator fun TImpl.times(other: TImpl): TImpl =
        when {
            isZero() || other.isZero() -> zero()
            other.pred().isZero() -> this
            else -> this + times(other.pred())
        }
}

val bigIntNatNumberADT = object : NatNumberADT<UByteBigInt> {
    override fun zero(): UByteBigInt = makeZeroBigNum()
    override fun UByteBigInt.isZero(): Boolean = isZeroImpl()
    override fun UByteBigInt.pred(): UByteBigInt = predImpl()
    override fun UByteBigInt.succ(): UByteBigInt = succImpl()
}

val unaryNatNumberADT = object : NatNumberADT<UnaryRepr> {
    override fun zero() = UnaryRepr.Zero
    override fun UnaryRepr.isZero() = this is UnaryRepr.Zero
    override fun UnaryRepr.pred(): UnaryRepr = this
        .tryCast<UnaryRepr.Succ>()
        .map { it.tail }
        .unwrapOr { throw NoSuchElementException("called pred on zero!") }
    override fun UnaryRepr.succ(): UnaryRepr = UnaryRepr.Succ(this)
}

val intNatNumberADT = object : NatNumberADT<Int> {
    override fun zero(): Int = 0
    override fun Int.isZero(): Boolean = this == 0
    override fun Int.pred(): Int = (this > 0)
        .guard { this - 1}
        .unwrapOr { throw NoSuchElementException("called pred on zero!") }
    override fun Int.succ(): Int = this + 1
}

val listNatNumberADT = object : NatNumberADT<List<Boolean>> {
    override fun zero(): List<Boolean> = listOf()
    override fun List<Boolean>.isZero(): Boolean = isEmpty()
    override fun List<Boolean>.pred(): List<Boolean> = isNotEmpty()
        .guard {
            val copy = toMutableList()
            copy.removeLast()
            copy
        }.unwrapOr {
            throw NoSuchElementException("called pred on zero!")
        }
    
    override fun List<Boolean>.succ(): List<Boolean> {
        val copy = toMutableList()
        copy.add(true)
        return copy
    }
}

fun ex2_1_v2() {
    fun<T> testScope(scope: () -> NatNumberADT<T>): () -> Unit = {
        with(scope()) {
            fun factorial(x: T): T = if (x.isZero() || x.pred().isZero()) { x } else { x * factorial(x.pred()) }

            val x = zero(); println("${x.toInt()} is zero: ${x.isZero()}")
            val y = x.succ(); println("${y.toInt()} is zero: ${y.isZero()}")
            val z = y.pred(); println("${z.toInt()} is zero: ${z.isZero()}")
            val w = y + y + y + y; println("${w.toInt()} is zero: ${w.isZero()}")
            val a = w - y; println("${a.toInt()} is zero: ${a.isZero()}")
            val b = w - a; println("${b.toInt()} is zero: ${b.isZero()}")
            val c = w * a; println("${c.toInt()} is zero: ${c.isZero()}")

            val f0 = factorial(w); println("fact(${w.toInt()}) = ${f0.toInt()}")

            val six = (c * w).pred().pred()

            val f = factorial(six); println("fact(${six.toInt()}) = ${f.toInt()}")
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