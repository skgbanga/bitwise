#include <cassert>
#include <cctype>
#include <climits>
#include <cmath>
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

void syntax_error(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  printf("Syntax error: ");
  vprintf(fmt, args);
  puts("");
  va_end(args);
}

void check_eq(uint64_t a, uint64_t b) {
  if (a != b) {
    fatal("%lu not equal to %lu. Exiting", a, b);
  }
}
void check_eq(double a, double b) {
  if (a != b) {
    fatal("%f not equal to %f. Exiting", a, b);
  }
}

template <typename T>
struct vector {
  vector() = default;

  vector(const vector&) = delete;
  vector& operator=(const vector&) = delete;

  vector(vector&& other) noexcept { move(std::move(other)); }
  vector& operator=(vector&& other) noexcept {
    free(ptr_);
    move(std::move(other));
    return *this;
  }
  ~vector() { free(ptr_); }

 public:
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
  const T* data() { return reinterpret_cast<const T*>(ptr_); }
  const T* data() const { return reinterpret_cast<const T*>(ptr_); }

 private:
  void grow() {
    assert(size_ == capacity_);
    auto new_capacity = (capacity_) ? 2 * capacity_ : 1;
    ptr_ = (ptr_) ? realloc(ptr_, new_capacity * sizeof(T))
                  : malloc(new_capacity * sizeof(T));
    assert(ptr_);
    capacity_ = new_capacity;
  }

  void move(vector&& other) {
    capacity_ = other.capacity_;
    size_ = other.size_;
    ptr_ = other.ptr_;

    other.ptr_ = nullptr;
    other.size_ = 0;
    other.capacity_ = 0;
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

enum TokenKind : int {
  TokenLastChar = 127,
  TokenInt,
  TokenFloat,
  TokenStr,
  TokenName,
  TokenLeftshift,     // <<
  TokenRightshift,    // >>
  TokenEq,            // ==
  TokenNotEq,         // !=
  TokenLTEq,          // <=
  TokenGTEq,          // >=
  TokenAnd,           // &&
  TokenOr,            // ||
  TokenInc,           // ++
  TokenDec,           // --
  TokenAddAssign,     // +=
  TokenSubAssign,     // -=
  TokenColonAssign,   // :=
  TokenAndAssign,     // &=
  TokenOrAssign,      // |=
  TokenXorAssign,     // ^=
  TokenLshiftAssign,  // <<=
  TokenRshiftAssign,  // >>=
  TokenMulAssign,     // *=
  TokenDivAssign,     // /=
  TokenModAssign,     // %=
};
enum TokenMod {
  TokenModNone = 0,
  TokenModDec,
  TokenModHex,
  TokenModOct,
  TokenModBin,
  TokenModChar,
};

struct Token {
  TokenKind kind;
  TokenMod mod;
  const char* start;
  const char* end;
  union {
    uint64_t int_val;
    double float_val;
    const char* name;
  };
  // since vector has a ctor/dtor, it is not being put in the union
  vector<char> str_val;
};
static_assert(sizeof(Token) == 48);

void print_token(const Token& token) {
  printf("kind: %d", token.kind);
  switch (token.kind) {
    case TokenInt:
      printf(" value: %lu", token.int_val);
      break;
    case TokenFloat:
      printf(" value: %f", token.float_val);
      break;
    case TokenStr:
      printf(" value %s", token.str_val.data());
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
    case TokenFloat:
      sprintf(buf, "float");
    default:
      if (kind < 128 && isprint(kind)) {
        sprintf(buf, "%c", kind);
      } else {
        sprintf(buf, "<ASCII %d>", kind);
      }
  }
  return buf;
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
// INC = '++'
// DEC = '--'

// Names/literals:
//
// NAME  = [a-zA-Z_][a-zA-Z0-9_]*
// INT   = [1-9][0-9]* | 0[xX][0-9a-fA-F]+ | 0[0-7]+ | 0[bB][0-1]+
// FLOAT = [0-9]*[.]?[0-9]*([eE][+-]?[0-9]+)?
// CHAR  = '\'' . '\''
// STR   = '""' [^"]* '"'

class Tokenizer {
 public:
  Tokenizer(const char* stream) : stream_(stream) {}

  Token& next() {
  top:
    token_.mod = TokenModNone;
    token_.start = stream_;
    switch (*stream_) {
      case ' ':
      case '\n':
      case '\r':
      case '\t':
      case '\v': {
        while (isspace(*stream_)) {
          stream_++;
        }
        goto top;  // don't want to generate white space tokens
      } break;
      case '\'':
        scan_char();
        break;
      case '"':
        scan_str();
        break;
      case '.': {
        scan_float();
      } break;
      case '0' ... '9': {
        while (isdigit(*stream_)) {
          stream_++;
        }
        if (*stream_ == '.' || tolower(*stream_) == 'e') {
          stream_ = token_.start;
          scan_float();
        } else {
          stream_ = token_.start;
          scan_int();
        }
      } break;
      case 'a' ... 'z':
      case 'A' ... 'Z':
      case '_': {
        while (isalnum(*stream_) || (*stream_ == '_')) {
          stream_++;
        }
        token_.kind = TokenName;
        token_.name = g_str_interner.intern_range(token_.start, stream_);
      } break;

#define CASE1(c, c1, t1)                              \
  case c: {                                           \
    token_.kind = static_cast<TokenKind>(*stream_++); \
    if (*stream_ == c1) {                             \
      token_.kind = t1;                               \
      stream_++;                                      \
    }                                                 \
  } break

#define CASE2(c, c1, t1, c2, t2)                      \
  case c: {                                           \
    token_.kind = static_cast<TokenKind>(*stream_++); \
    if (*stream_ == c1) {                             \
      token_.kind = t1;                               \
      stream_++;                                      \
    } else if (*stream_ == c2) {                      \
      token_.kind = t2;                               \
      stream_++;                                      \
    }                                                 \
  } break

#define CASE3(c, c1, t1, c2, t2, c3, t3)              \
  case c: {                                           \
    token_.kind = static_cast<TokenKind>(*stream_++); \
    if (*stream_ == c1) {                             \
      token_.kind = t1;                               \
      stream_++;                                      \
      if (*stream_ == c3) {                           \
        token_.kind = t3;                             \
        stream_++;                                    \
      }                                               \
    } else if (*stream_ == c2) {                      \
      token_.kind = t2;                               \
      stream_++;                                      \
    }                                                 \
  } break

        CASE1('=', '=', TokenEq);
        CASE1('!', '=', TokenNotEq);
        CASE1(':', '=', TokenColonAssign);
        CASE1('^', '=', TokenXorAssign);
        CASE1('*', '=', TokenMulAssign);
        CASE1('/', '=', TokenDivAssign);
        CASE1('%', '=', TokenModAssign);

        CASE2('+', '+', TokenInc, '=', TokenAddAssign);
        CASE2('-', '-', TokenDec, '=', TokenSubAssign);
        CASE2('|', '|', TokenOr, '=', TokenOrAssign);
        CASE2('&', '&', TokenAnd, '=', TokenAndAssign);

        CASE3('<', '<', TokenLeftshift, '=', TokenLTEq, '=', TokenLshiftAssign);
        CASE3('>', '>', TokenRightshift, '=', TokenGTEq, '=',
              TokenRshiftAssign);

#undef CASE1
#undef CASE2
#undef CASE3
      default:
        token_.kind = static_cast<TokenKind>(*stream_++);
        break;
    }
    token_.end = stream_;

    return token_;
  }

  void scan_int() {
    // decimal, hexadecimal, octal, binary
    static uint8_t table[256] = {
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  1,  2,  3, 4, 5, 6, 7, 8, 9,  0,  0,
        0,  0,  0,  0, 0, 10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 10, 11, 12,
        13, 14, 15, 0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0, 0,  0,  0,
        0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0,
    };

    uint64_t value = 0;
    uint64_t base = 10;
    token_.mod = TokenModDec;
    if (*stream_ == '0') {
      stream_++;
      if (tolower(*stream_) == 'x') {
        stream_++;
        base = 16;
        token_.mod = TokenModHex;
      } else if (tolower(*stream_) == 'b') {
        stream_++;
        base = 2;
        token_.mod = TokenModBin;
      } else if (isdigit(*stream_)) {
        base = 8;
        token_.mod = TokenModOct;
      } else {
        if (!isspace(*stream_) && (*stream_ != 0)) {
          syntax_error("Invalid integer literal suffix '%c' <ASCII %d>",
                       *stream_, *stream_);
        }
      }
    }

    while (true) {
      auto digit = table[(int)*stream_];
      if (digit == 0 && *stream_ != '0') {
        break;
      }

      if (digit >= base) {
        syntax_error("Digit '%c' out of range for base %lu", digit, base);
      }

      if (value > (UINT64_MAX - digit) / base) {
        syntax_error("integer literal overflow");
        while (isdigit(*stream_)) {  // small bug here
          stream_++;
        }
        value = 0;
      } else {
        value = value * base + digit;
        stream_++;
      }
    }

    token_.kind = TokenInt;
    token_.int_val = value;
  }

  void scan_float() {
    // FLOAT = [0-9]*[.][0-9]*([eE][+-]?[0-9]+)?
    const char* start = stream_;
    while (isdigit(*stream_)) {
      stream_++;
    }
    if (*stream_ == '.') {
      stream_++;
    }
    while (isdigit(*stream_)) {
      stream_++;
    }
    if (islower(*stream_) == 'e') {
      stream_++;
      if ((*stream_ == '+') || (*stream_ == '-')) {
        stream_++;
      }
      if (!isdigit(*stream_)) {
        syntax_error("Expected a digit. Found %d\n", *stream_);
      }
      while (isdigit(*stream_)) {
        stream_++;
      }
    }
    [[maybe_unused]] const char* end =
        stream_;  // use it to validate the strtod
    double val = strtod(start, nullptr);
    if (val == HUGE_VAL || val == -HUGE_VAL) {
      syntax_error("Float literal overflow");
    }

    token_.kind = TokenFloat;
    token_.float_val = val;
  }

  void scan_char() {
    auto escape_to_char = [](char c) -> char {
      // clang-format off
      if (c == 'n') { return '\n'; }
      if (c == 'r') { return '\r'; }
      if (c == 't') { return '\r'; }
      if (c == 'v') { return '\v'; }
      if (c == 'b') { return '\b'; }
      if (c == 'a') { return '\a'; }
      if (c == '0') { return '\0'; }
      // clang-format on
      return 0;
    };

    assert(*stream_ == '\'');
    stream_++;

    char val = 0;
    if (*stream_ == '\'') {
      syntax_error("char literal cannot be empty");
    } else if (*stream_ == '\n') {
      syntax_error("char literal cannot contain a new line");
    } else if (*stream_ == '\\') {
      stream_++;
      auto c = escape_to_char(*stream_);
      if (c == 0 && *stream_ != '0') {
        syntax_error("invalid char literal escape '\\%c'", *stream_);
      }
      val = c;
    } else {
      val = *stream_;
    }
    stream_++;

    if (*stream_ != '\'') {
      syntax_error("incomplete char literal");
    }
    stream_++;

    token_.kind = TokenInt;
    token_.mod = TokenModChar;
    token_.int_val = val;
  }

  void scan_str() {
    assert(*stream_ == '"');
    stream_++;

    vector<char> value;
    while (*stream_ && *stream_ != '"') {
      auto c = *stream_;
      if (c == '\n') {
        syntax_error("string literal cannot contain a new line");
      } else if (*stream_ == '\\') {
        stream_++;
        c = escape_to_char(*stream_);
        if (c == 0 && *stream_ != '0') {
          syntax_error("invalid char literal escape '\\%c'", *stream_);
        }
      }
      value.push_back(c);
      stream_++;
    }

    if (*stream_) {
      assert(*stream_ == '"');
      stream_++;
    } else {
      syntax_error("Unexpected end of file within string literal");
    }

    value.push_back(0);

    token_.kind = TokenStr;
    token_.str_val = std::move(value);
  }

 private:
  char escape_to_char(char c) {
    // clang-format off
      if (c == 'n') { return '\n'; }
      if (c == 'r') { return '\r'; }
      if (c == 't') { return '\r'; }
      if (c == 'v') { return '\v'; }
      if (c == 'b') { return '\b'; }
      if (c == 'a') { return '\a'; }
      if (c == '0') { return '\0'; }
    // clang-format on
    return 0;
  };

  const char* stream_;
  Token token_;
};

void lex_test() {
#define expect_name(X)                                            \
  {                                                               \
    auto& token = tokenizer.next();                               \
    assert(token.kind == TokenName);                              \
    assert(g_str_interner.intern_range(token.start, token.end) == \
           g_str_interner.intern(X));                             \
  }

#define expect_token(X)             \
  {                                 \
    auto& token = tokenizer.next(); \
    assert(token.kind == X);        \
  }

#define expect_int(X, M)            \
  {                                 \
    auto& token = tokenizer.next(); \
    assert(token.kind == TokenInt); \
    assert(token.mod == M);         \
    assert(token.int_val == X);     \
  }

#define expect_float(X)               \
  {                                   \
    auto& token = tokenizer.next();   \
    assert(token.kind == TokenFloat); \
    check_eq(token.float_val, X);     \
  }

#define expect_str(X)                             \
  {                                               \
    auto& token = tokenizer.next();               \
    assert(token.kind == TokenStr);               \
    assert(strcmp(token.str_val.data(), X) == 0); \
  }

  {
    const char* str = "XY+(XY)_HELLO+997-hello";
    Tokenizer tokenizer(str);

    expect_name("XY");
    expect_token('+');
    expect_token('(');
    expect_name("XY");
    expect_token(')');
    expect_name("_HELLO");
    expect_token('+');
    expect_int(997, TokenModDec);
    expect_token('-');
    expect_name("hello");
  }

  {
    const char* str = "0xA2    1234 0x10 042 0b1010 0";
    Tokenizer tokenizer(str);
    expect_int(162, TokenModHex);
    expect_int(1234, TokenModDec);
    expect_int(16, TokenModHex);
    expect_int(34, TokenModOct);
    expect_int(10, TokenModBin);
    expect_int(0, TokenModDec);
  }

  {
    const char* str = "3.14 .123 42.    3e10";
    Tokenizer tokenizer(str);
    expect_float(3.14);
    expect_float(.123);
    expect_float(42.);
    expect_float(3e10);
  }

  // char literal
  {
    const char* str = "  '\\n'  'a'";
    Tokenizer tokenizer(str);
    expect_int('\n', TokenModChar);
    expect_int('a', TokenModChar);
  }

  // string literal
  {
    const char* str = " \"hello\" \"a\\nb\"";
    Tokenizer tokenizer(str);
    expect_str("hello");
    expect_str("a\nb");
  }

  // multi char tokens
  {
    const char* str =
        "== != := ^= *= /= %= ++ -- += -= == || |= && &= << <= <<= >> >= >>=";
    Tokenizer tokenizer(str);
    expect_token(TokenEq);
    expect_token(TokenNotEq);
    expect_token(TokenColonAssign);
    expect_token(TokenXorAssign);
    expect_token(TokenMulAssign);
    expect_token(TokenDivAssign);
    expect_token(TokenModAssign);
    expect_token(TokenInc);
    expect_token(TokenDec);
    expect_token(TokenAddAssign);
    expect_token(TokenSubAssign);
    expect_token(TokenEq);
    expect_token(TokenOr);
    expect_token(TokenOrAssign);
    expect_token(TokenAnd);
    expect_token(TokenAndAssign);
    expect_token(TokenLeftshift);
    expect_token(TokenLTEq);
    expect_token(TokenLshiftAssign);
    expect_token(TokenRightshift);
    expect_token(TokenGTEq);
    expect_token(TokenRshiftAssign);
  }

#undef expect_str
#undef expect_token
#undef expect_int
#undef expect_float
}

void run_tests() {
  vec_test();
  lex_test();
  str_intern_test();
}

int main() {
  run_tests();
  return 0;
}
