PKGS=libzmq
OBJECTS=main.o
CFLAGS=-Wall -Os

gpio-sensor: $(OBJECTS)
	gcc -o $@ $(OBJECTS) `pkg-config --libs $(PKGS)`

client: client.o
	gcc -o $@ $< `pkg-config --libs $(PKGS)`

%.o: %.c
	gcc $(CFLAGS) -o $@ -c $< `pkg-config --cflags $(PKGS)`

clean:
	rm -f gpio-sensor $(OBJECTS) client client.o
