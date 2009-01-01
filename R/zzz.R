volatile <- new.env(TRUE, emptyenv())

.onLoad <- function(libname, pkgname) {
  # try to detect the number of cores
  cores <- 8 # a rather optimisitc fallback
  if (length(grep("^darwin",R.version$os))) { # on Mac OS X uses sysctl
    a <- system("/usr/sbin/sysctl -n hw.ncpu 2>/dev/null", TRUE)
    if (length(a) && nchar(a[1])) cores <- as.integer(a[1])
  } else {
    # Linux
    a <- system("grep processor /proc/cpuinfo 2>/dev/null|wc -l", TRUE)
    if (length(a) && nchar(a[1])) cores <- as.integer(a[1])
    else { # IRIX
      a <- system("hinv 2>/dev/null | grep -i processor | sed 's: .*::'", TRUE)
      if (length(a) && nchar(a[1])) cores <- as.integer(a[1])
      else { # Solaris
        a <- system("/usr/sbin/psrinfo -v|grep 'Status of processor'|wc -l", TRUE)
        if (length(a) && nchar(a[1])) cores <- as.integer(a[1])
      }
    }
  }
  if (cores < 1) cores <- 4
  volatile$detectedCores <- cores
  TRUE
}
