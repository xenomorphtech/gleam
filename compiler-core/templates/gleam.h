#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct gleamString {
      uint64_t refcount;
      uint64_t size;
      char * text;
    } gleamString;

void throwError();

void gleamString_release(gleamString*);
#define dont_release(a)
gleamString * gleamString_from(char *);
char * to_char_ptr(gleamString * this);
gleamString * gleamString_concat(gleamString * a, gleamString * b);
  
