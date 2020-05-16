#include <cassert>
#include <cctype>
#include <climits>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

// ugly, but faster to compile than sstream
[[noreturn]] void fatal(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  printf("FATAL: ");
  vprintf(fmt, args);
  puts("");
  va_end(args);
  exit(1);
}

// ugly, but faster to compile than sstream
void syntax_error(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  printf("Syntax error: ");
  vprintf(fmt, args);
  puts("");
  va_end(args);
}

void check_eq(int a, int b) {
  if (a != b) {
    fatal("%d not equal to %d. Exiting", a, b);
  }
}

template <typename T>
struct vector {
  ~vector() { free(ptr_); }
  void push_back(const T& t) {
    if (size_ == capacity_) {
      grow();
    }
    assert(size_ < capacity_);

    ptr()[size_++] = t;
  }
  int size() const { return size_; }
  int capacity() const { return capacity_; }
  T& operator[](int idx) { return ptr()[idx]; }

 private:
  void grow() {
    assert(size_ == capacity_);
    auto new_capacity = (capacity_) ? 2 * capacity_ : 1;
    ptr_ = (ptr_) ? realloc(ptr_, new_capacity * sizeof(T))
                  : malloc(new_capacity * sizeof(T));
    assert(ptr_);
    capacity_ = new_capacity;
  }
  T* ptr() { return reinterpret_cast<T*>(ptr_); }

  int capacity_ = 0;
  int size_ = 0;
  void* ptr_ = nullptr;
};

void vec_test() {
  vector<int> buf;
  assert(buf.size() == 0);
  const int N = 1024;
  for (int i = 0; i < N; ++i) {
    buf.push_back(i);
  }
  assert(buf.size() == N);
  for (int i = 0; i < N; ++i) {
    assert(buf[i] == i);
  }
}

struct StringInterner {
  ~StringInterner() {
    for (int i = 0; i < interns.size(); ++i) {
      const auto& data = interns[i];
      free((void*)data.str);
    }
  }
  const char* intern_range(const char* start, const char* end) {
    int len = end - start;
    for (int i = 0; i < interns.size(); ++i) {
      const auto& data = interns[i];
      if ((data.len == len) && (strncmp(data.str, start, len) == 0)) {
        return data.str;
      }
    }

    char* ptr = (char*)malloc(len + 1);
    memcpy(ptr, start, len);
    ptr[len] = 0;

    interns.push_back(Data{len, ptr});
    return ptr;
  }

  const char* intern(const char* str) {
    // slow method
    return intern_range(str, str + strlen(str));
  }

 private:
  struct Data {
    int len;
    const char* str;
  };
  vector<Data> interns;
};

void str_intern_test() {
  char x[] = "hello";
  char y[] = "hello";
  assert(x != y);

  StringInterner interner;
  assert(interner.intern(x) == interner.intern(y));

  char z[] = "hello!";
  assert(interner.intern(x) != interner.intern(z));
}

inline StringInterner g_str_interner;

enum TokenKind { TokenInt = 128, TokenName };

struct Token {
  TokenKind kind;
  const char* start;
  const char* end;
  union {
    uint64_t int_val;
    const char* name;
    // ...
  };
};
static_assert(sizeof(Token) == 32);

void print_token(const Token& token) {
  printf("kind: %d", token.kind);
  switch (token.kind) {
    case TokenInt:
      printf(" value: %lu", token.int_val);
      break;
    case TokenName:
      printf(" value: %.*s", (int)(token.end - token.start), token.start);
      break;
    default:
      break;
  }
  puts("");
}

// Bad: returns a pointer to static buffer, will be overwritten by next call
const char* token_kind_name(TokenKind kind) {
  static char buf[256];
  switch (kind) {
    case TokenInt:
      sprintf(buf, "integer");
      break;
    case TokenName:
      sprintf(buf, "name");
      break;
    default:
      if (kind < 128 && isprint(kind)) {
        sprintf(buf, "%c", kind);
      } else {
        sprintf(buf, "<ASCII %d>", kind);
      }
  }
  return buf;
}

class Tokenizer {
 public:
  Tokenizer(const char* stream) : stream_(stream) {}
  __attribute__((noinline)) Token next() {
    Token token{};
    token.start = stream_;
    switch (*stream_) {
      case '0' ... '9': {
        uint64_t val = 0;
        while (isdigit(*stream_)) {
          int digit = *stream_++ - '0';
          if (val > (UINT_MAX - digit) / 10) {
            syntax_error("integer literal overflow");
            while (isdigit(*stream_)) {
              stream_++;
            }
            val = 0;
          } else {
            val = val * 10 + digit;
          }
        }
        token.kind = TokenInt;
        token.int_val = val;
      } break;
      case 'a' ... 'z':
      case 'A' ... 'Z':
      case '_': {
        while (isalnum(*stream_) || (*stream_ == '_')) {
          stream_++;
        }
        token.kind = TokenName;
        token.name = g_str_interner.intern_range(token.start, stream_);
      } break;
      default:
        token.kind = static_cast<TokenKind>(*stream_++);
        break;
    }
    token.end = stream_;
    return token;
  }

 private:
  const char* stream_;
};

void lex_test() {
  const char* str = "XY+(XY)_HELLO+997-hello";
  Tokenizer tokenizer(str);

  [[maybe_unused]] auto expect_str = [&](const char* str) {
    auto token = tokenizer.next();

    assert(token.kind == TokenName);
    assert(g_str_interner.intern_range(token.start, token.end) ==
           g_str_interner.intern(str));
  };
  [[maybe_unused]] auto expect_char = [&](char c) {
    auto token = tokenizer.next();
    assert(token.kind == c);
  };
  [[maybe_unused]] auto expect_int = [&](int i) {
    auto token = tokenizer.next();
    assert(token.kind == TokenInt);
    assert(token.int_val == i);
  };

  // expect_str("XY");
  // expect_char('+');
  // expect_char('(');
  // expect_str("XY");
  // expect_char(')');
  // expect_str("_HELLO");
  // expect_char('+');
  // expect_int(997);
  // expect_char('-');
  // expect_str("hello");

  {
    str = "2147483648";
    Tokenizer tokenizer(str);
    auto token = tokenizer.next();
    assert(token.kind == TokenInt);
    check_eq(token.val, 0);
  }
}

// ion grammar
//
// Tokens
// (  ) [  ] {  }
//
// Unary:
// + - ! ~ & *
//
// Binary
//
// LSHIFT = '<<'
// RSHIFT = '>>'
// EQ = '=='
// NEQ = '!='
// LEQ = '<='
// GEQ = '>='
// AND = '&&'
// OR = '||'
//
// + - | ^ LSHIFT RSHIFT
// * / % &
// EQ NEQ < LEQ > GEQ
// AND
// OR
// ?:
//
// Assignment operators:
//
// COLON_ASSIGN = ':='
// ADD_ASSIGN = '+='
// SUB_ASSIGN = '-='
// OR_ASSIGN = '|='
// XOR_ASSIGN = '^='
// LSHIFT_ASSIGN = '<<='
// RSHIFT_ASSIGN = '>>='
// MUL_ASSIGN = '*='
// DIV_ASSIGN = '/='
// MOD_ASSIGN = '%='

// Names/literals:
//
// NAME = [a-zA-Z_][a-zA-Z0-9_]*
// INT = [1-9][0-9]* | 0[xX][0-9a-fA-F]+
// CHAR = '\'' . '\''
// STR = '""' [^"]* '"'

void run_tests() {
  vec_test();
  lex_test();
  str_intern_test();
}

int main() {
  run_tests();
  return 0;
}
