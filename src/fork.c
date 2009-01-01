/* multicore R package

   fork.c
   interface to system-level tools for sawning copies of the current
   process and IPC

   (C)Copyright 2008 Simon Urbanek

   see package DESCRIPTION for licensing terms */

#include <sys/types.h>
#include <unistd.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <signal.h>

#include <R.h>
#include <Rinternals.h>

typedef struct child_info {
	pid_t pid;
	int pfd;
	struct child_info *next;
} child_info_t;

static child_info_t children, *last_child = &children;

static int master_fd = -1;
static int is_master = 1;

static int rm_child_(int pid) {
	child_info_t *ci = &children, *prev = 0;
	while (ci) {
		if (ci->pid == pid) {
			if (!prev) {
				if (ci->next) {
					children.pid = ci->next->pid;
					children.pfd = ci->next->pfd;
					children.next = ci->next->next;
				} else {
					children.pid = 0;
					children.pfd = 0;
					children.next = 0;
				}
			} else {
				prev->next = ci->next;
				free(ci);
				return 1;
			}
		}
		ci = ci->next;
	}
	return 0;
}

SEXP mc_fork() {
	int pipefd[2];
	pid_t pid;
	SEXP res = allocVector(INTSXP, 2);
	int *res_i = INTEGER(res);
	if (pipe(pipefd)) error("Unable to create a pipe.");
	pid = fork();
	if (pid == -1) error("Unable to fork.");
	res_i[0] = (int) pid;
	if (pid == 0) { /* child */
		close(pipefd[0]); /* close read end */
		master_fd = res_i[1] = pipefd[1];
		is_master = 0;
#if 0
		/* remap stdin/out/err */
		{
			int pfd[2];
			pipe(pfd);
			dup2(pfd[1], STDOUT_FILENO);
			close(pfd[1]);
			// stdoutFD=pfd[0];
		}
		{
			int pfd[2];
			pipe(pfd);
			dup2(pfd[1], STDERR_FILENO);
			close(pfd[1]);
			// stderrFD=pfd[0];
		}
		{
			int pfd[2];
			pipe(pfd);
			dup2(pfd[0], STDIN_FILENO);
			close(pfd[0]);
			// stdinFD=pfd[1];
		}
#endif
	} else { /* master process */
		child_info_t *ci;
		close(pipefd[1]); /* close write end */
		res_i[1] = pipefd[0];
		/* register the new child and its pipe */
		if (children.pid == 0)
			ci = &children;
		else
			last_child->next = ci = (child_info_t*) malloc(sizeof(child_info_t));
		if (!ci) error("Memory allocation error.");
		ci->pid = pid;
		ci->pfd = pipefd[0];
		ci->next = 0;
		last_child = ci;
	}
	return res;
}

SEXP send_master(SEXP what) {
	unsigned char *b;
	unsigned int len = 0, i = 0;
	if (is_master) error("only children can send data to the master process");
	if (master_fd == -1) error("there is no pipe to the master process");
	if (TYPEOF(what) != RAWSXP) error("content to send must be RAW, use serialize if needed");
	len = LENGTH(what);
	b = RAW(what);
	if (write(master_fd, &len, sizeof(len)) != sizeof(len)) {
		close(master_fd);
		master_fd = -1;
		error("write error, closing pipe to the master");
	}
	while (i < len) {
		int n = write(master_fd, b + i, len - i);
		if (n < 1) {
			close(master_fd);
			master_fd = -1;
			error("write error, closing pipe to the master");
		}
		i += n;
	}
	return ScalarLogical(1);
}

SEXP select_children(SEXP sTimeout, SEXP sWhich) {
	int maxfd = 0, sr, wstat;
	unsigned int wlen = 0, wcount = 0;
	SEXP res;
	int *res_i, *which = 0;
	child_info_t *ci = &children;
	fd_set fs;
	struct timeval tv = { 0, 0 }, *tvp = &tv;
	if (isReal(sTimeout) && LENGTH(sTimeout) == 1) {
		double tov = asReal(sTimeout);
		if (tov < 0.0) tvp = 0; /* Note: I'm not sure we really should allow this .. */
		else {
			tv.tv_sec = (int) tov;
			tv.tv_usec = (int) ((tov - ((double) tv.tv_sec)) * 1000000.0);
		}
	}
	if (TYPEOF(sWhich) == INTSXP && LENGTH(sWhich)) {
		which = INTEGER(sWhich);
		wlen = LENGTH(sWhich);
	}
	while (waitpid(-1, &wstat, WNOHANG) > 0) {}; /* check for zombies */
	FD_ZERO(&fs);
	while (ci && ci->pid) {
		if (ci->pfd > maxfd) maxfd = ci->pfd;
		if (ci->pfd > 0) {
			if (which) { /* check for the FD only if it's on the list */
				unsigned int k = 0;
				while (k < wlen) if (which[k++] == ci->pid) { FD_SET(ci->pfd, &fs); wcount++; break; }
			} else
				FD_SET(ci->pfd, &fs);
		}
		ci = ci -> next;
	}
#ifdef MC_DEBUG
	Rprintf("select_children: maxfd=%d, wlen=%d, wcount=%d, timeout=%d:%d\n", maxfd, wlen, wcount, tv.tv_sec, tv.tv_usec);
#endif
	if (maxfd == 0 || (wlen && !wcount)) return R_NilValue; /* NULL signifies no children to tend to */
	sr = select(maxfd + 1, &fs, 0, 0, tvp);
#ifdef MC_DEBUG
	Rprintf("  sr = %d\n", sr);
#endif
	if (sr < 0) {
		perror("select");
		return ScalarLogical(0); /* FALSE on select error */
	}
	if (sr < 1) return ScalarLogical(1); /* TRUE on timeout */
	ci = &children;
	maxfd = 0;
	while (ci && ci->pid) { /* pass 1 - count the FDs (in theory not necessary since that's what select should have returned)  */
		if (ci->pfd > 0 && FD_ISSET(ci->pfd, &fs)) maxfd++;
		ci = ci -> next;
	}
	ci = &children;
	res = allocVector(INTSXP, maxfd);
	res_i = INTEGER(res);
	while (ci && ci->pid) { /* pass 2 - fill the array */
		if (ci->pfd > 0 && FD_ISSET(ci->pfd, &fs)) (res_i++)[0] = ci->pid;
		ci = ci -> next;
	}
	return res;
}

static SEXP read_child_ci(child_info_t *ci) {
	unsigned int len = 0;
	int fd = ci->pfd;
	int n = read(fd, &len, sizeof(len));
#ifdef MC_DEBUG
	Rprintf(" read_child_ci - read length returned %d\n", n);
#endif
	if (n != sizeof(len) || len == 0) { /* error or child is exiting */
		int pid = ci->pid;
		close(fd);
		ci->pfd = -1;
		rm_child_(pid);
		return ScalarInteger(pid);
	} else {
		SEXP rv = allocVector(RAWSXP, len);
		unsigned char *rvb = RAW(rv);
		unsigned int i = 0;
		while (i < len) {
			n = read(fd, rvb + i, len - i);
#ifdef MC_DEBUG
			Rprintf(" read_child_ci - read %d at %d returned %d\n", len-i, i, n);
#endif
			if (n < 1) {
				close(fd);
				return ScalarInteger(ci->pid);
			}
			i += n;
		}
		PROTECT(rv);
		{
			SEXP pa = allocVector(INTSXP, 1);
			INTEGER(pa)[0] = ci->pid;
			setAttrib(rv, install("pid"), pa);
		}
		UNPROTECT(1);
		return rv;
	}
}

SEXP read_child(SEXP sPid) {
	int pid = asInteger(sPid);
	child_info_t *ci = &children;
	while (ci) {
		if (ci->pid == pid) break;
		ci = ci->next;
	}
	if (!ci) return R_NilValue; /* if the child doesn't exist anymore, returns NULL */
	return read_child_ci(ci);	
}

SEXP read_children(SEXP sTimeout) {
	int maxfd = 0, sr, wstat;
	child_info_t *ci = &children;
	fd_set fs;
	struct timeval tv = { 0, 0 }, *tvp = &tv;
	if (isReal(sTimeout) && LENGTH(sTimeout) == 1) {
		double tov = asReal(sTimeout);
		if (tov < 0.0) tvp = 0; /* Note: I'm not sure we really should allow this .. */
		else {
			tv.tv_sec = (int) tov;
			tv.tv_usec = (int) ((tov - ((double) tv.tv_sec)) * 1000000.0);
		}
	}
	while (waitpid(-1, &wstat, WNOHANG) > 0) {}; /* check for zombies */
	FD_ZERO(&fs);
	while (ci && ci->pid) {
		if (ci->pfd > maxfd) maxfd = ci->pfd;
		if (ci->pfd > 0) FD_SET(ci->pfd, &fs);
		ci = ci -> next;
	}
#ifdef MC_DEBUG
	Rprintf("read_children: maxfd=%d, timeout=%d:%d\n", maxfd, tv.tv_sec, tv.tv_usec);
#endif
	if (maxfd == 0) return R_NilValue; /* NULL signifies no children to tend to */
	sr = select(maxfd+1, &fs, 0, 0, tvp);
#ifdef MC_DEBUG
	Rprintf("sr = %d\n", sr);
#endif
	if (sr < 0) {
		perror("select");
		return ScalarLogical(0); /* FALSE on select error */
	}
	if (sr < 1) return ScalarLogical(1); /* TRUE on timeout */
	ci = &children;
	while (ci && ci->pid) {
		if (ci->pfd > 0 && FD_ISSET(ci->pfd, &fs)) break;
		ci = ci -> next;
	}
#ifdef MC_DEBUG
	Rprintf("set ci=%p (%d, %d)\n", ci, ci?ci->pid:0, ci?ci->pfd:0);
#endif
	/* this should never occur really - select signalled a read handle
	   but none of the handles is set - let's treat it as a timeout */
	if (!ci) return ScalarLogical(1);
	else
		return read_child_ci(ci);
	/* we should never land here */
	return R_NilValue;
}

SEXP rm_child(SEXP sPid) {
	int pid = asInteger(sPid);
	return ScalarLogical(rm_child_(pid));
}

SEXP mc_children() {
	unsigned int count = 0;
	SEXP res;
	int *pids;
	child_info_t *ci = &children;
	while (ci && ci->pid > 0) {
		count++;
		ci = ci->next;
	}
	res = allocVector(INTSXP, count);
	if (count) {
		pids = INTEGER(res);
		ci = &children;
		while (ci && ci->pid > 0) {
			(pids++)[0] = ci->pid;
			ci = ci->next;
		}
	}
	return res;
}

SEXP mc_kill(SEXP sPid, SEXP sSig) {
	int pid = asInteger(sPid);
	int sig = asInteger(sSig);
	if (kill((pid_t) pid, sig))
		error("Kill failed.");
	return ScalarLogical(1);
}

SEXP mc_exit(SEXP sRes) {
	int res = asInteger(sRes);
	if (is_master) error("exit can only be used in a child process");
	if (master_fd != -1) { /* send 0 to signify that we're leaving */
		unsigned int len = 0;
		write(master_fd, &len, sizeof(len));
	}
	exit(res);
	error("exit failed");
	return R_NilValue;
}
