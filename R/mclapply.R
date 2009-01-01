mclapply <- function(X, FUN, ..., mc.set.seed=TRUE, mc.cores=getOption("cores")) {
  env <- parent.frame()
  cores <- mc.cores
  if (is.null(cores)) cores <- 8
  cores <- as.integer(cores)
  if (length(X) < cores) cores <- length(X)
  if (cores < 2) return(lapply(X, FUN, ...))
  sindex <- lapply(1:cores, function(i) seq(i,length(X), by=cores))
  schedule <- lapply(1:cores, function(i) X[seq(i,length(X), by=cores)])
  ch <- list()
  res <- list()
  cp <- rep(0L, cores)
  fin <- rep(FALSE, cores)
  dr <- rep(FALSE, cores)
  inner.do <- function(core) {
    S <- schedule[[core]]
    f <- fork()
    if (inherits(f, "masterProcess")) { # child process
      on.exit(exit(1,structure("fatal error in wrapper code",class="try-error")))
      if (isTRUE(mc.set.seed)) set.seed(Sys.getpid())
      sendMaster(try(lapply(S, FUN, ...), silent=TRUE))
      exit(0)
    }
    ch[[core]] <<- f
    cp[core] <<- f$pid
    NULL
  }
  lapply(1:cores, inner.do)
  print(cp)
  while (!all(fin)) {
    a <- readChildren(1)
    if (!length(a)) break # no children -> no hope we get anything
    if (is.integer(a)) { 
      str(a)
      core <- which(cp == a)
      fin[core] <- TRUE
    } else if (is.raw(a)) {
      str(a)
      core <- which(cp == attr(a, "pid"))
      res[[core]] <- unserialize(a)
      dr[core] <- TRUE
    }
  }
#  str(res)
  ores <- list()
  for (i in 1:cores) ores[sindex[[i]]] <- res[[i]]
  ores
}

#mcapply(1:4, function(i) i+1)
