#include "gleam.h"
#include <string.h>

void gleamString_release(gleamString * this) {
  
}

void throwError() {
  exit(1);
}

gleamString * gleamString_from(char * arg) {
  gleamString * ret = malloc(sizeof(gleamString * ));

  ret->refcount = 1;
  ret->text = arg;
  return ret;
}

//printf()
char * to_char_ptr(gleamString * this) {
  return this->text;
}

gleamString * gleamString_concat(gleamString * a, gleamString * b) {
  gleamString * ret = malloc(sizeof(gleamString * ));

  char * newtext = malloc(strlen(a->text) + strlen(b->text) + 1);
  strcpy(newtext, a->text);
  strcpy(newtext+strlen(a->text), b->text);
  ret->refcount = 1;
  ret->text = newtext;
  return ret;
}
