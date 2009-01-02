parallel <- function(expr, mc.set.seed=FALSE) {
  f <- fork()
  env <- parent.frame()
  if (inherits(f, "masterProcess")) {
    on.exit(exit(1, structure("fatal error in wrapper code",class="try-error")))
    if (isTRUE(mc.set.seed)) set.seed(Sys.getpid())
    sendMaster(try(eval(expr, env), silent=TRUE))
    exit(0)
  }
  class(f) <- c("parallelJob", class(f))
  f
}

collect <- function(jobs, wait=TRUE, timeout=0, intermediate=FALSE) {
  if (missing(jobs)) jobs <- children()
  if (isTRUE(intermediate)) intermediate <- str
  if (!wait) {
    s <- selectChildren(jobs, timeout)
    if (is.logical(s) || !length(s)) return(NULL)
    lapply(s, function(x) { r <- readChild(x); if (is.raw(r)) unserialize(r) else NULL })
  } else {
    pids <- if (inherits(jobs, "process")) jobs$pid else if (is.list(jobs)) unlist(lapply(jobs, function(x) x$pid)) else jobs
    if (!length(pids)) pids <- children()
    if (!length(pids)) return(NULL)
    res <- list()
    fin <- rep(FALSE, length(jobs))
    while (!all(fin)) {
      s <- selectChildren(pids, 0.5)
      if (is.integer(s)) {
        for (pid in s) {
          r <- readChild(pid)
          if (is.integer(r) || is.null(r)) fin[pid==pids] <- TRUE
          if (is.raw(r)) res[[which(pid==pids)]] <- unserialize(r)
        }
        if (is.function(intermediate)) intermediate(res)
      } else if (all(is.na(match(pids, children())))) break
    }
    names(res) <- pids
    res
  }
}
