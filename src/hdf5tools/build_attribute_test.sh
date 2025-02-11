#! /bin/sh
gcc -DHAVE_CONFIG_H -I. -I..  -D_GNU_SOURCE -I/usr/include   -W -Wall -g -O2 -c -o attribute_test.o attribute_test.c

gcc -W -Wall -g -O2 -o attribute_test attribute_test.o -L/usr/lib64 -ldl -lhdf5 -lhdf5_hl -lz -lm -lc


