#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>

#define GPIO_A 23
#define GPIO_B 24

int read_gpio(int n)
{
    int r = -1;
    char path[256];
    snprintf(path, 255, "/sys/class/gpio/gpio%d/value", n);
    int fd = open(path, O_RDONLY);
    if (fd >= 0) {
        char c;
        if (read(fd, &c, 1) == 1) {
            if (c >= '0' && c <= '1')
                r = c - '0';
        }
        close(fd);
    }

    return r;
}

int main (void)
{
    //  Socket to talk to clients
    void *context = zmq_ctx_new();
    void *responder = zmq_socket(context, ZMQ_REP);
#ifdef ZMQ_IPV6
    int ipv6 = 1;
    zmq_setsockopt(responder, ZMQ_IPV6, &ipv6, sizeof(int));
#endif
    int rc = zmq_bind(responder, "tcp://*:5555");
    assert(rc == 0);

    while(1) {
        /* Receive and ignore */
        zmq_recv(responder, NULL, 0, 0);

        /* Read current state */
        signed char result = -1;
        int a = read_gpio(GPIO_A);
        if (a == 1)
            result = 1;
        else if (a == 0) {
            int b = read_gpio(GPIO_B);
            if (b == 1)
                result = 2;
            else if (b == 0)
                result = 0;
        }
        zmq_send(responder, &result, 1, 0);
    }
    return 0;
}
