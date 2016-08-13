#!/bin/bash


gcc -o bin/c_node_server \
-O3 -std=c99 -Wall -Wmissing-prototypes \
-I/Users/hezhiqiang/.kerl/installs/18.3_dirty_schedulers/lib/erl_interface-3.8.2/include -I./include \
-L/Users/hezhiqiang/.kerl/installs/18.3_dirty_schedulers/lib/erl_interface-3.8.2/lib \
src/complex.c src/cnode_s.c \
-lerl_interface -lei


gcc -o bin/c_node_server2 \
-O3 -std=c99 -Wall -Wmissing-prototypes \
-I/Users/hezhiqiang/.kerl/installs/18.3_dirty_schedulers/lib/erl_interface-3.8.2/include -I./include \
-L/Users/hezhiqiang/.kerl/installs/18.3_dirty_schedulers/lib/erl_interface-3.8.2/lib \
src/complex.c src/cnode_s2.c \
-lerl_interface -lei


gcc -o bin/c_node_client \
-O3 -std=c99 -Wall -Wmissing-prototypes \
-I/Users/hezhiqiang/.kerl/installs/18.3_dirty_schedulers/lib/erl_interface-3.8.2/include -I./include \
-L/Users/hezhiqiang/.kerl/installs/18.3_dirty_schedulers/lib/erl_interface-3.8.2/lib \
src/complex.c src/cnode_c.c \
-lerl_interface -lei
