#include <stdio.h>
#include <stdlib.h>

#define MEMORY_SIZE 8192
#define TAPE_SIZE 8192

int memory[MEMORY_SIZE] = { 0 };
int *ptr = memory + (MEMORY_SIZE / 2);

char tape[TAPE_SIZE] = { 0 };
char *tptr = tape;
char *ltptr = tape;

char next() {
  tptr++;
  if(tptr <= ltptr) {
    return *tptr;
  }
  int c = getchar();
  if(c == EOF) exit(0);
  *tptr = (char) c;
  ltptr++;

  return *tptr;
}

void seek_left(void) {
  for(tptr--; *tptr != '['; tptr--) {
    if(*tptr == ']') seek_left();
  }
}

void seek_right(void) {
  for(next(); *tptr != ']'; next()) {
    if(*tptr == '[') seek_right();
  }
}


int main() {
  printf("----- BRAINFUCK C -----\n");

  for(;;) {
    char c = next();

    switch(c) {
      case '>':
        ptr++;
        break;
      case '<':
        ptr--;
        break;
      case '+':
        (*ptr)++;
        break;
      case '-':
        (*ptr)--;
        break;
      case '.':
        printf("%c", *ptr);
        fflush(stdout);
        break;
      case ',':
        *ptr = next();
        break;
      case '[':
        if(!*ptr) seek_right();
        break;
      case ']':
        if(*ptr) seek_left();
        break;
    }
  }

  return 0;
}
