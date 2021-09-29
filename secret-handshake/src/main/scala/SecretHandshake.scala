object SecretHandshake {
    def commands(n: Int): List[String] = {
        var handshake: List[String] = List()
        if ((n & (1 << 0)) != 0) handshake :+= "wink"
        if ((n & (1 << 1)) != 0) handshake :+= "double blink"
        if ((n & (1 << 2)) != 0) handshake :+= "close your eyes"
        if ((n & (1 << 3)) != 0) handshake :+= "jump"
        if ((n & (1 << 4)) == 0) handshake 
        else handshake.reverse
    } 
}