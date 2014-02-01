#include <zmq.h>
#include <stdio.h>
#include <assert.h>

int main (void)
{
    //  Socket to talk to clients
    void *context = zmq_ctx_new();
    void *client = zmq_socket(context, ZMQ_REQ);
    int rc = zmq_connect(client, "tcp://beere:5555");
    assert(rc == 0);

    zmq_send(client, "", 0, 0);
    signed result;
    zmq_recv(client, &result, 1, 0);
    printf("result: %i\n", result);

    return 0;
}
