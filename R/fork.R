# --- multicore --- low-level functions ---

# selected signals
SIGHUP <- 1L
SIGINT <- 2L
SIGQUIT <- 3L
SIGKILL <- 9L
SIGTERM <- 15L
SIGALRM <- 14L
SIGSTOP <- 17L
SIGCHLD <- 20L
SIGINFO <- 29L
SIGUSR1 <- 30L
SIGUSR2 <- 31L

fork <- function() {
  r <- .Call("mc_fork", PACKAGE="multicore")
  structure(list(pid=r[1], fd=r[2:3]), class=c(ifelse(r[1]!=0L,"childProcess","masterProcess"),"process"))
}

readChildren <- function(timeout=0)
  .Call("read_children", as.double(timeout), PACKAGE="multicore")

readChild <- function(child) {
  if (inherits(child, "process")) child <- processID(child)
  if (!is.numeric(child)) stop("invalid child argument")
  .Call("read_child", as.integer(child), PACKAGE="multicore")
}

selectChildren <- function(children=NULL, timeout=0) {
  if (is.null(children)) children <- integer(0)
  if (inherits(children, "process")) children <- processID(children)
  if (is.list(children)) children <- unlist(lapply(children, function(x) if (inherits(x, "process")) x$pid else stop("children must be a list of processes or a single process")))
  if (!is.numeric(children)) stop("children must be a list of processes or a single process")
  .Call("select_children", as.double(timeout), as.integer(children))
}

rmChild <- function(child) {
  if (inherits(child, "process")) child <- processID(child)
  if (!is.numeric(child)) stop("invalid child argument")
  .Call("rm_child", as.integer(child), PACKAGE="multicore")
}

kill <- function(process, signal=SIGINT) {
  process <- processID(process)
  unlist(lapply(process, function(p) .Call("mc_kill", as.integer(p), as.integer(signal))))
}

sendMaster <- function(what) {
  if (!is.raw(what)) what <- serialize(what, NULL, FALSE)
  .Call("send_master", what, PACKAGE="multicore")
}

processID <- function(process)
  if (inherits(process, "process")) process$pid else if (is.list(process)) unlist(lapply(process, processID)) else stop("process must be of the class `process'")

sendChildStdin <- function(child, what) {
  if (inherits(child, "process") || is.list(child)) child <- processID(child)
  if (!is.numeric(child) || !length(child)) stop("child must be a valid child process")
  child <- as.integer(child)
  if (is.character(what)) what <- charToRaw(paste(what, collapse='\n'))
  if (!is.raw(what)) stop("what must be a character or raw vector")
  unlist(lapply(child, function(p) .Call("send_child_stdin", p, what)))
}

exit <- function(exit.code=0L, send=NULL) {
  if (!is.null(send)) try(sendMaster(send), silent=TRUE)
  .Call("mc_exit", as.integer(exit.code), PACKAGE="multicore")
}

children <- function() {
  p <- .Call("mc_children", PACKAGE="multicore")
  lapply(p, function(x) structure(list(pid=x), class=c("childProcess", "process")))
}
