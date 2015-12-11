/*
  exec proxy for epmd - so we could properly shutdown when port is closed on erlang side.
 */
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>

int child_dead = 0;

static void sig_handler(int sig, siginfo_t *si, void *unused) {
  child_dead = 1;
}

int main(int argc, char **argv) {
  /* We can't use more simpler signal(2) because it will trigger automatic restart in read(2) */
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = sig_handler;
  if (sigaction(SIGCHLD, &sa, NULL) == -1) {
    perror("sigaction");
    exit(1);
  }

  int pid = fork();
  if (pid > 0) {
    int read_bytes;
    char buf[1024];
    while (1) {
      read_bytes = read(STDIN_FILENO, buf, sizeof(buf));
      if ( read_bytes == 0 ) { // eof
        break;
      }
      if ( read_bytes == -1 ) {
        if ( errno == EINTR && !child_dead ) {
          continue;
        }
        break;
      }
    }

    /* try to tear-down epmd if it's still alive */
    kill(pid, SIGKILL);
    exit(0);
  } else if (pid == 0) {
    argv[0] = "epmd";
    execvp("epmd", argv);
  } else {
    perror("fork");
    exit(1);
  }
}
