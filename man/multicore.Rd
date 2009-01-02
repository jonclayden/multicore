\name{multicore}
\alias{multicore}
\alias{process}
\alias{childProcess}
\alias{masterProcess}
\alias{parallelJob}
\title{
  multicore R package for parallel processing of R code
}
\description{
\emph{multicore} is an R package that provides functions for parallel
execution of R code on machines with multiple cores or CPUs. Unlike
other parallel processing methods all jobs are share the full state of
R when spawned, so no data or code needs to be initialized. The
actual spawning is very fast as well since no new R instance needs to
be started.
}
\section{Pivotal functions}{
\code{\link{mclapply}} - parallelized verson of \code{\link{lapply}}

\code{\link{parallel}} and \code{\link{collect}} - functions to
evaluate R expressions in parallel and collect the results.
}
\section{Low-level functions}{
Those function should be used only by experienced users understanding
the interaction of the master (parent) process and the child processes
(jobs) as well as the system-level mechanics involved.

See \code{\link{fork}} help page for the principles of forking
parallel processes and system-level functions, \code{\link{children}}
and \code{\link{sendMaster}} help pages for management and
communication between the parent and child processes.
}
\section{Classes}{
\emph{multicore} defines a few informal (S3) classes:

\code{process} is a list with a named entry \code{pid} containing the
process ID.

\code{childProcess} is a subclass of \code{process} representing a
child process of the current R process. A child process is a special
process that can send messages to the parent process. The list may
contain additional entries for IPC (more precisely file descriptors),
however those are considered internal.

\code{masterProcess} is a subclass of \code{process} representing a
handle that is passed to a child process by \code{\link{fork}}.

\code{parallelJob} is a subclass of \code{childProcess} representing a
child process created using the \code{\link{parallel}} function.
}
\section{Warning}{
\emph{multicore} uses the \code{fork} system call to spawn a copy of
the current process which performs the compultations in
parallel. Modern operating systems use copy-on-write approach which
makes this so appealing for parallel computation since only objects
modified during the computation will be actually copied and all other
memory is directly shared.

However, the copy shares everything including any user interface
elements. This can cause havoc since let's say one window now suddenly
belongs to two processes. Therefore \emph{multicomp} should be
preferrably used in console R and code executed in parallel may
never use GUIs or on-screen devices.
}
\seealso{
  \code{\link{parallel}}, \code{\link{mclapply}},
  \code{\link{fork}}, \code{\link{sendMaster}}, \code{\link{children}}
  and \code{\link{signals}}
}
\author{Simon Urbanek}
\keyword{interface}
