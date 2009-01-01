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

collect <- function(jobs, wait=TRUE, timeout=0) {
  if (!wait) {
    s <- selectChildren(jobs, timeout)
    if (is.logical(s) || !length(s)) return(NULL)
    lapply(s, function(x) { r <- readChild(x); if (is.raw(r)) unserialize(r) else NULL })
  } else {
    # FIXME: this is not really what we want to do - replace with proper implementation later
    collect(jobs, FALSE, 30)
  }
}
