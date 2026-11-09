HOST <- "127.0.0.1"
PORT <- 65432

con <- socketConnection(host = HOST, port = PORT, blocking = TRUE, open = "r+")
message(sprintf("Connected to %s:%d. Type lines; Ctrl-D to quit.", HOST, PORT))

input <- file("stdin")
open(input)
repeat {
    line <- readLines(input, n = 1)  # Each line you type
    if (length(line) == 0) break     # Ctrl-D (end of input)
    writeLines(line, con)            # Send it to the server
    echo <- readLines(con, n = 1)    # Wait for the echo
    cat(echo, "\n", sep = "")        # Show it
}
close(con)
close(input)
