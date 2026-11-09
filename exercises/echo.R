PORT <- 65432

# Note: serverSocket() listens on all interfaces; R has no option to bind
# only to 127.0.0.1 the way Python's s.bind((HOST, PORT)) does.
s <- serverSocket(PORT)
message(sprintf("Echo server is listening on port %d", PORT))

tryCatch({
    repeat {  # Loop over all clients
        conn <- socketAccept(s, blocking = FALSE, open = "a+b")
        message("Connection accepted")

        tryCatch({
            repeat {  # Loop over messages from the current client
                socketSelect(list(conn), timeout = NULL)  # Wait until data (or EOF) arrives
                data <- readBin(conn, "raw", 1024)        # Whatever is available, up to 1024 bytes
                if (length(data) == 0) {
                    break                                 # Ready but empty: client disconnected
                }
                msg <- sub("\n$", "", rawToChar(data))
                writeBin(charToRaw(paste0("<< ", msg, " >>\n")), conn)
            }
        }, error = function(e) {                          # Client vanished abruptly
            message("Client dropped: ", conditionMessage(e))
        }, finally = close(conn))

        message("Client disconnected. Waiting for next connection...")
    }
}, interrupt = function(e) {                              # Ctrl-C
    message("\nShutting down.")
}, finally = close(s))
