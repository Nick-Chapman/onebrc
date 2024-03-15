
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/stat.h>

// original non-memory-mapped version...
/*static FILE* the_file;
void open_the_file(char* filename) {
  the_file = fopen(filename,"r");
  if (!the_file) {
    printf("**error:fopen failed: %s\n", filename);
    exit(1);
  }
}
char read_char(void) {
  return fgetc(the_file);
}*/


// memory mapped files...
static char* the_contents;
static unsigned long the_file_size;
static unsigned long the_pointer;
void open_the_file(char* filename) {
  int fd = open(filename, O_RDWR);
  struct stat st;
  fstat(fd,&st);
  the_file_size = st.st_size;
  the_contents = (char*)mmap(NULL,the_file_size,PROT_WRITE,MAP_SHARED,fd,0);
  the_pointer = 0;
}
char read_char(void) {
  if (the_pointer >= the_file_size) return -1;
  return the_contents[the_pointer++];
}


#define MAX_NAME_LEN 30

static char buf[MAX_NAME_LEN];

typedef struct entry {
  //char name[MAX_NAME_LEN];
  int count;
  int tot;
  int min;
  int max;
} entry_t;

/*
#define MAX_ENTRIES 500

static entry_t dict[MAX_ENTRIES];

static int nextEntry = 0;

void print_dict(void) {
  int comma = 0;
  printf("{");
  for (int i = 0; i < nextEntry; i++) {
    double min = (double)(dict[i].min) / 10;
    double max = (double)(dict[i].max) / 10;
    //double meanA = ((double)dict[i].tot / (double)dict[i].count); // you would think
    double meanA = ((double)dict[i].tot / 10 / (double)dict[i].count) * 10; // baseline
    int mi = floor (meanA+0.5);
    double mean = (double)mi/10;
    // bug: can print "-0.0" !
    printf("%s%s=%.1f/%.1f/%.1f", (comma?", ":""), dict[i].name, min, mean, max);
    comma = 1;
  }
  printf("}\n");
}

entry& insert_dict(char* key) { // openSpace
  if (nextEntry >= MAX_ENTRIES) {
    printf("**error: too many enties\n");
    exit(1);
  }
  int x = 0;
  while ((x < nextEntry) && (strcmp(dict[x].name,key) < 0)) {
    x++;
  }
  for (int i = nextEntry-1; i >= x; i--) {
    dict[i+1] = dict[i];
  }
  nextEntry++;
  return dict[x];
}

entry* lookup_dict(char* key) { // binary_search
  int lower = 0;
  int upper = nextEntry-1;
  while (lower <= upper) {
    int i = lower + (upper-lower)/2;
    int comp = strcmp(key,dict[i].name);
    if (comp == 0) {
      return &dict[i];
    } else if (comp < 0) {
      upper = i-1;
    } else {
      lower = i+1;
    }
  }
  return 0;
}
*/


#include <unordered_map>
#include <vector>
#include <algorithm>

static std::unordered_map<std::string,entry_t> the_m;

bool compare(std::pair<std::string, entry_t> one, std::pair<std::string, entry_t> two) {
  return one.first < two.first;
}

void print_dict(void) {
  std::vector<std::pair<std::string, entry_t>> elems(the_m.begin(), the_m.end());
  std::sort(elems.begin(), elems.end(), compare);
  int comma = 0;
  printf("{");
  for(auto it = elems.begin(); it!=elems.end();it++) {
    entry_t& entry = it->second;
    const char* name = it->first.c_str();
    double min = (double)(entry.min) / 10;
    double max = (double)(entry.max) / 10;
    double meanA = ((double)entry.tot / 10 / (double)entry.count) * 10; // baseline
    int mi = floor (meanA+0.5);
    double mean = (double)mi/10;
    printf("%s%s=%.1f/%.1f/%.1f", (comma?", ":""), name, min, mean, max);
    comma = 1;
  }
  printf("}\n");
}

entry& insert_dict(char* key) {
  entry_t e = {};
  the_m[key] = e;
  return the_m[key];
}

entry* lookup_dict(char* key) {
  auto it = the_m.find(key);
  if (it == the_m.end()) {
    return 0;
  }
  entry_t& entry = it->second;
  return &entry;
}


int tokName(void) {
  int i = 0;
  for (;;) {
    char c = read_char();
    if (c == -1) {
      return 1; //done
    }
    if (i>=MAX_NAME_LEN) {
      buf[MAX_NAME_LEN-1] = 0;
      printf("**error: name too long: %s\n", buf);
      exit(1);
    }
    if (c == ';') {
      buf[i] = 0;
      break;
    }
    buf[i] = c;
    i++;
  }
  return 0;
}

int readTemp(void) {
  int i = 0;
  int sign = 1;
  char c = read_char();
  if (c == '-') {
    sign = -1;
    c = read_char();
  }
  for (;;) {
    if (c == '\n') break;
    if (c == '.') {
      c = read_char();
      continue;
    }
    if ((c < '0') || (c > '9')) {
      printf("**not a digit: %c\n",c);
      exit(1);
    }
    int d = c - '0';
    i *= 10;
    i += d;
    c = read_char();
  }
  return (sign * i);
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    puts("**error:argc!=2");
    exit(1);
  }
  char* filename = argv[1];
  open_the_file(filename);
  for (;;) {
    int done = tokName();
    if (done) break;
    entry_t* x = lookup_dict(buf);
    int t = readTemp();
    if (x==0) {
      entry_t& entry = insert_dict(buf);
      //strcpy (entry.name, buf);
      entry.count = 1;
      entry.tot = t;
      entry.min = t;
      entry.max = t;
    } else {
      entry_t& entry = *x;
      entry.count ++;
      entry.tot += t;
      if (t < entry.min) entry.min = t;
      if (t > entry.max) entry.max = t;
    }
  }
  print_dict();
  return 0;
}
