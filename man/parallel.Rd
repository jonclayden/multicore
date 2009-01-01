\name{parallel}
\alias{parallel}
\alias{collect}
\title{
  Evaluate an expression asynchronously in a separate process
}
\description{
  \code{parallel} starts a parallel process which evalated the given expression.

  \code{collects} results from parallel processes
}
\usage{
parallel(expr, mc.set.seed = FALSE)
collect(jobs, wait = TRUE, timeout = 0)
}
\arguments{
\item{expr}{expression to evaluate}
\item{mc.set.seed}{if set to \code{TRUE} then the random number
generator is seeded such that it is differen from any other
process. Othewise the same as in the current R session.}
\item{jobs}{list of jobs (or a single) job to collect results for}
\item{wait}{if set to \code{FALSE} it checks for any results that are
available within \code{timeout} seconds from now, otherwise it waits
for all jobs to deliver results.}
\item{timeout}{timeout (in seconds) to check for job results - applies
only if \code{wait} is \code{FALSE}.}
}
\value{
  \code{parallel} returns an object of the class \code{parallelJob}
  which is in turn a \code{childProcess}.

  \code{collect} returns any results that are available in a list
}
\details{
  \code{parallel} evaluates the \code{expr} expression in parallel to
  the current R process. Everything is shared read-only between the
  parallel process and the current process, i.e. no side-effects of
  the expression affect the main process. The result of the parallel
  execution can be collected using \code{collect} function.

  \code{collect} function collects any available results from parallel
  jobs.

  Note: If \code{expr} uses low-level \code{multicore} functions such
  as \code{\link{sendMaster}} a single job can deliver results
  multiple times and it is the responsibility of the user to interpret
  them correctly. \code{collect} will return \code{NULL} for a
  terminating job that has sent its results already after which the
  job is no longer available.
}
\seealso{
  \code{\link{mclapply}}, \code{\link{sendMaster}}
}
\examples{
  p <- parallel(1:10)
  collect(p) # will retrieve the result
  collect(p) # will signal the job as terminating
  collect(p) # there is no such job
}
\author{Simon Urbanek}
\keyword{interface}
