
void run_cc() {

}

void first(void ** tailcall) {

  if ((int64_t)tailcall[2] == 10) {
    void** continuation = malloc(8 * 2 + 8);
    continuation[0] = &first_tail_0;
    continuation[1] = tailcall[1]; //next continuation
    continuation[2] = tailcall[2]; //preserve scope 

    tailcall[1]= continuation; //continuation pointer
    tailcall[0] = &random_cat;
    return;
  }

  (int64_t)tailcall[2] = (int64_t)tailcall[2] + 1;

  return; //same as calling first() 
}

void first_tail_0(void ** tailcall) {

}

void random_cat(void ** tailcall) {

}


