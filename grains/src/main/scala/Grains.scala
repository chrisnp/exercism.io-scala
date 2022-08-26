object Grains {
    def square(num: Int): Option[BigInt] = {
        if (num < 1 || num > 64) {
            return None
        }
        Some (BigInt(1) << num - 1)
    }

    def total(): BigInt = {
        (BigInt(1) << 64) - 1
    }
}