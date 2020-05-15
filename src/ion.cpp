#include <cassert>
#include <cctype>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

// ugly, but faster to compile than sstream
void fatal(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  printf("FATAL: ");
  vprintf(fmt, args);
  puts("");
  va_end(args);
  exit(1);
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
    int val;
    const char* name;
    // ...
  };
};
static_assert(sizeof(Token) == 32);

void print_token(const Token& token) {
  printf("kind: %d", token.kind);
  switch (token.kind) {
    case TokenInt:
      printf(" value: %d", token.val);
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
        while (std::isdigit(*stream_)) {
          val *= 10;
          val += *stream_++ - '0';
        }
        token.kind = TokenInt;
        token.val = val;
      } break;
      case 'a' ... 'z':
      case 'A' ... 'Z':
      case '_': {
        while (std::isalnum(*stream_) || (*stream_ == '_')) {
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
  auto token = tokenizer.next();
  while (token.kind) {
    // print_token(token);
    token = tokenizer.next();
  }
}

// parser grammar (EBNF format)
//
// expr3 = INT | '(' expr ')'
// expr2 = [-]expr2 | expr3
// expr1 = expr2 ([/*] expr2)*
// expr0 = expr1 ([+-] expr1)*
// expr  = expr0
//
//
// Some comments:
// Consider expr0: when we are parsing expr1 we know that we have parsed all the
// higher precedence stuff first before start thinking about [+-]

class Parser {
 public:
  Parser(const char* stream) : tokenizer_(stream) { next_token(); }
  int parse_expr();  // forward declare

  int parse_expr3() {
    int value = 0;
    if (is_token(TokenInt)) {
      value = token_.val;
      next_token();
    } else if (match_token(tk('('))) {
      value = parse_expr();
      expect_token(tk(')'));
    } else {
      fatal("expected integer or (. got %s", token_kind_name(token_.kind));
    }
    return value;
  }

  int parse_expr2() {
    if (match_token(tk('-'))) {
      return -parse_expr2();
    } else {
      return parse_expr3();
    }
  }
  int parse_expr1() {
    int value = parse_expr2();
    while (is_token(tk('*')) || is_token(tk('/'))) {
      char op = token_.kind;
      next_token();
      int rvalue = parse_expr2();
      if (op == '*') {
        value *= rvalue;
      } else {
        assert(op == '/');
        assert(rvalue != 0);
        value /= rvalue;
      }
    }
    return value;
  }

  int parse_expr0() {
    int value = parse_expr1();
    while (is_token(tk('+')) || is_token(tk('-'))) {
      char op = token_.kind;
      next_token();
      int rvalue = parse_expr1();
      if (op == '+') {
        value += rvalue;
      } else {
        assert(op == '-');
        value -= rvalue;
      }
    }

    return value;
  }

 private:
  // helper functions
  TokenKind tk(char c) const { return (TokenKind)c; }
  void next_token() { token_ = tokenizer_.next(); }
  bool is_token(TokenKind kind) { return token_.kind == kind; }
  bool is_token_name(const char* name) {
    return token_.kind == TokenName && token_.name == name;
  }
  bool match_token(TokenKind kind) {
    if (is_token(kind)) {
      next_token();
      return true;
    } else {
      return false;
    }
  }
  bool expect_token(TokenKind kind) {
    if (is_token(kind)) {
      next_token();
      return true;
    } else {
      char buf[256];
      auto expected = token_kind_name(kind);
      strncpy(buf, expected, 256);
      fatal("expected token: %s, got token %s", buf,
            token_kind_name(token_.kind));
      return false;
    }
  }

  Token token_;
  Tokenizer tokenizer_;
};

int Parser::parse_expr() {
  return parse_expr0();
}

void parse_test() {
  auto test = [](const char* expr) {
    Parser parser(expr);
    return parser.parse_expr();
  };

  assert(test("1") == 1);
  assert(test("(1)") == 1);
  assert(test("3-4+10") == 9);
  assert(test("3+4*5") == 23);
  assert(test("-3") == -3);
  assert(test("1+10/2") == 6);
  assert(test("2*3+4*5") == 26);
  assert(test("2+-3") == -1);
  assert(test("--3") == 3);
}

int main() {
  vec_test();
  lex_test();
  str_intern_test();
  parse_test();
}
