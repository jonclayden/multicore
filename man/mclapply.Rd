\name{mclapply}
\alias{mclapply}
\title{
  Parallel version of lapply
}
\description{
  \code{mclapply} is a parallelized version of \code{\link{lapply}},
  it returns a list of the same length as \code{X}, each element of
  which is the result of applying \code{FUN} to the corresponding
  element of \code{X}.
}
\usage{
mclapply(X, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = getOption("cores"))
}
\arguments{
\item{X}{a vector (atomic or list) or an expressions vector.  Other
objects (including classed objects) will be coerced by
\code{\link{as.list}}.}
\item{FUN}{the function to be applied to each element of \code{X}}
\item{...}{optional arguments to \code{FUN}}
\item{mc.set.seed}{if set to \code{TRUE} then each parallel process
first sets its seed to something different from other
processes. Otherwise all processes start with the same (namely
current) seed.}
\item{mc.silent}{if set to \code{TRUE} then all output on stdout will be
suppressed for all parallel processes spawned (stderr is not affected).}
\item{mc.cores}{The number of cores to use, i.e. how many processes
will be spawned (at most)}
}
\value{
 A list.
}
\details{
  \code{mclapply} is a parallelized version of \code{lapply}. By
  default the input vector/list \code{X} is split into as many parts
  as there are cores (currently the values are spread across the cores
  sequentially, i.e. first value to core 1, second to core 2,
  ... (core + 1)-th value to core 1 etc.) and then one process is
  spawned to each core and the results are collected.

  Due to the parallel nature of the execution random numbers are not
  sequential (in the random number sequence) as they would be in
  \code{lapply}. They are sequential for each spawned process, but not
  all jobs as a whole.

  In addition, each process is running the job inside \code{try(...,
  silent=TRUE)} so if error occur they will be stored as
  \code{try-error} objects in the list.
}
\seealso{
  \code{\link{parallel}}, \code{\link{collect}}
}
\examples{
  mclapply(1:30, rnorm)
}
\author{Simon Urbanek}
\keyword{interface}
