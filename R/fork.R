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
  structure(list(pid=r[1], fd=r[2]), class=c(ifelse(r[1]!=0L,"childProcess","masterProcess"),"process"))
}

readChildren <- function(timeout=0)
  .Call("read_children", as.double(timeout), PACKAGE="multicore")

readChild <- function(child) {
  if (inherits(child, "process")) child <- child$pid
  if (!is.numeric(child)) stop("invalid child argument")
  .Call("read_child", as.integer(child), PACKAGE="multicore")
}

selectChildren <- function(children=NULL, timeout=0) {
  if (is.null(children)) children <- integer(0)
  if (inherits(children, "process")) children <- children$pid
  if (is.list(children)) children <- unlist(lapply(children, function(x) if (inherits(x, "process")) x$pid else stop("children must be a list of processes or a single process")))
  if (!is.numeric(children)) stop("children must be a list of processes or a single process")
  .Call("select_children", as.double(timeout), as.integer(children))
}

rmChild <- function(child) {
  if (inherits(child, "process")) child <- child$pid
  if (!is.numeric(child)) stop("invalid child argument")
  .Call("rm_child", as.integer(child), PACKAGE="multicore")
}

kill <- function(process, signal=SIGINT) {
  if (inherits(process, "process")) process <- process$pid
  .Call("mc_kill", as.integer(process), as.integer(signal))
}

sendMaster <- function(what) {
  if (!is.raw(what)) what <- serialize(what, NULL, FALSE)
  .Call("send_master", what, PACKAGE="multicore")
}

exit <- function(exit.code=0L, send=NULL) {
  if (!is.null(send)) try(sendMaster(send), silent=TRUE)
  .Call("mc_exit", as.integer(exit.code), PACKAGE="multicore")
}

children <- function() {
  p <- .Call("mc_children", PACKAGE="multicore")
  lapply(p, function(x) structure(list(pid=x), class=c("childProcess", "process")))
}
