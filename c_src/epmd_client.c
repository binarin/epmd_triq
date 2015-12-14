#include <sys/select.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <errno.h>
#include <arpa/inet.h>

// max 2-byte length
#define EPMD_LENGTH 65535

int parse_opts(int argc, char** argv, char **node_name, int *epmd_port);
int connect_to_epmd(unsigned int port);
void send_alive2_req(int sock, int port_no, const char* node_name);

int parse_opts(int argc, char** argv, char **node_name, int *epmd_port) {
  int c;
  while (1) {
    int option_index = 0;
    static struct option long_options[] = {
      {"name", required_argument, 0, 'n'},
      {"port", required_argument, 0, 'p'},
      {0, 0, 0, 0}
    };
    c = getopt_long(argc, argv, "", long_options, &option_index);
    if ( c == -1 ) {
      break;
    }
    switch (c) {
    case 'n':
      *node_name = optarg;
      break;
    case 'p':
      *epmd_port = atoi(optarg);
    }
  }
  return 0;
}

int connect_to_epmd(unsigned int port) {
  struct sockaddr_in saddr;

  memset(&saddr, 0, sizeof(saddr));
  saddr.sin_port = htons(port);
  saddr.sin_family = AF_INET;
  saddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

  int sock = socket(AF_INET, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("epmd socket open");
    exit(1);
  }

  if (connect(sock, (struct sockaddr*)&saddr, sizeof(saddr))) {
    perror("epmd connect");
    exit(1);
  }

  return sock;
}

#define put8(buf, val) do { \
    (buf)[0] = (val);       \
    (buf) += 1;             \
  } while (0)

#define put16(buf, val) do {                    \
    (buf)[0] = ((val) >> 8) & 0xff;             \
    (buf)[1] = (val) & 0xff;                    \
    (buf) += 2;                                  \
  } while (0)

#define putstr(buf, str) do {                   \
    int buf__strlen = strlen(str);              \
    put16(buf, buf__strlen);                    \
    memcpy(buf, str, buf__strlen);              \
    buf += buf__strlen;                         \
  } while (0)

void send_alive2_req(int sock, int port_no, const char* node_name) {
  char buf[EPMD_LENGTH];
  char *ptr = buf;
  ptr += 2; // reserve space for packet length

  put8(ptr, 120); // req id
  put16(ptr, port_no);
  put8(ptr, 77); // normal erlang node
  put8(ptr, 0); // tcp/ip
  put16(ptr, 5); // highest distribution version
  put16(ptr, 5); // lowest distribution version
  putstr(ptr, node_name);
  putstr(ptr, ""); // extra

  int len = ptr - buf;
  ptr = buf;
  put16(ptr, len - 2);

  int bytes_written = write(sock, buf, len);
  // give epmd some time to process data. Otherwise SO_LINGER settings
  // below may cause discarding of this data even before processing by epmd.
  sleep(1);

  // Close socket in such a way that write(2) will imediately fail on
  // the other side of the socket.
  struct linger lo = { 1, 0 };
  setsockopt(sock, SOL_SOCKET, SO_LINGER, &lo, sizeof(lo));
  close(sock);

  if ( bytes_written != len ) {
    perror("write alive2");
    exit(1);
  }

  int resp_bytes = read(sock, buf, 4);
  if (resp_bytes != 4) {
    fprintf(stdout, "Failed to receive alive resp\n");
    exit(1);
  }

  if (buf[1]) {
    fprintf(stdout, "Non successful alive resp: %d\n", buf[1]);
    exit(1);
  }

  fprintf(stdout, "ok");
  fflush(stdout);
}

int main(int argc, char** argv) {
  char *node_name = "test";
  int epmd_port = 4369;

  if (parse_opts(argc, argv, &node_name, &epmd_port)) {
    exit(1);
  }

  int epmd_sock = connect_to_epmd(epmd_port);
  send_alive2_req(epmd_sock, 6666, node_name);

  fd_set read_fds;
  FD_ZERO(&read_fds);
  FD_SET(STDIN_FILENO, &read_fds);
  FD_SET(epmd_sock, &read_fds);
  select(epmd_sock + 1, &read_fds, 0, 0, 0);
}
