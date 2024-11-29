class EmptyBufferException() extends Exception {}

class FullBufferException() extends Exception {}

class CircularBuffer(val capacity: Int) {
    private val buffer: Array[Int] = new Array(capacity)
    private var readPointer: Int = 0
    private var writePointer: Int = 0
    private var sizeInUse: Int = 0

    def write(value: Int): Unit =
        if sizeInUse == capacity then 
            throw FullBufferException()
        buffer(writePointer) = value
        writePointer += 1
        writePointer %= capacity
        sizeInUse += 1
    
    def read(): Int =
        if sizeInUse == 0 then 
            throw EmptyBufferException()
        val value = buffer(readPointer)
        readPointer += 1 
        readPointer %= capacity 
        sizeInUse -= 1
        value

    def overwrite(value: Int): Unit =
        if sizeInUse == capacity then read()
        write(value)

    def clear(): Unit = sizeInUse = 0
}