#include <cassert>
#include <cctype>
#include <climits>
#include <cmath>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <type_traits>  // is_base_of, enable_if, is_trivial, is_trivially_copyable

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

template <typename T>
struct vector {
  static_assert(std::is_trivial_v<T> && std::is_trivially_copyable_v<T>);
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
  T& push_back(T t) {
    if (size_ == capacity_) {
      grow();
    }
    assert(size_ < capacity_);

    ptr()[size_++] = std::move(t);
    return ptr()[size_ - 1];
  }

  int size() const { return size_; }
  int capacity() const { return capacity_; }
  T& operator[](int idx) {
    assert(idx < size_);
    return ptr()[idx];
  }
  const T* data() { return reinterpret_cast<const T*>(ptr_); }
  const T* data() const { return reinterpret_cast<const T*>(ptr_); }

  T* begin() { return reinterpret_cast<T*>(ptr_); }
  const T* begin() const { return reinterpret_cast<const T*>(ptr_); }
  T* end() { return reinterpret_cast<T*>(ptr_) + size_; }
  const T* end() const { return reinterpret_cast<const T*>(ptr_) + size_; }

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

template <typename T>
struct unique_ptr {
  unique_ptr(T* p) : p_(p) {}

  template <typename U, std::enable_if_t<std::is_base_of_v<T, U>, int> = 0>
  unique_ptr(unique_ptr<U>&& other) : p_(other.release()) {}

  ~unique_ptr() { delete p_; }
  unique_ptr(const unique_ptr&) = delete;
  unique_ptr& operator=(const unique_ptr&) = delete;
  unique_ptr(unique_ptr&& other) noexcept : p_(other.p_) { other.p_ = nullptr; }
  unique_ptr& operator=(unique_ptr&& other) noexcept {
    delete p_;
    p_ = other.p_;
    other.p_ = nullptr;
    return *this;
  }
  T* operator->() { return p_; }
  T* get() { return p_; }
  const T* operator->() const { return p_; }
  T* release() {
    T* p = p_;
    p_ = nullptr;
    return p;
  }

 private:
  T* p_ = nullptr;
};

template <typename T, typename... Args>
unique_ptr<T> make_unique(Args&&... args) {
  return unique_ptr<T>(new T(std::forward<Args>(args)...));
}

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

  // iteration
  {
    vector<int> buf;
    for (int i = 0; i < 5; ++i) {
      buf.push_back(i);
    }

    int sum = 0;
    for (auto i : buf) {
      sum += i;
    }
    assert(sum == 10);
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
    assert(token.float_val == X);     \
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

// clang-format off
//
// parser EBNF for ion
//
// ==== DECLARATIONS ====
//
// type_list = type (',' type)*
// name_list = NAME (',' NAME)*
//
// base_type =  NAME
//            | 'func' '(' type_list? ')' (':' type)?
//            | '(' type ')'
// type = base_type ('[' expr? ']' | '*')*
//
// enum_item = NAME (= expr)?
// enum_items = enum_item (',' enum_item)* ','?
// enum_decl = NAME '{' enum_items '}'
//
// aggregate_field = name_list ':' type ';'
// aggregate_decl = NAME '{' (aggregate_field ';')* '}'
//
// var_decl = NAME '=' expr
//          | NAME ':' type ('=' expr)?
//
// const_decl = NAME '=' expr
//
// typedef_decl = NAME '=' type
//
// func_param = NAME ':' type
// func_param_list = func_param (',' func_param)?
// func_decl = NAME '(' func_param_list? ')' (':' type)? '{' stmt_block '}'
//
// decl = 'enum' enum_decl
//      | 'struct' aggregate_decl
//      | 'union' aggregate_decl
//      | 'var' var_decl
//      | 'const' const_decl
//      | 'typedef' typedef_decl
//      | 'func' func_decl
//
//
//  ==== STATEMENTS ====
//
// assign_op = '=' | ADD_ASSIGN | COLON_ASSIGN | ...
//
// switch_case = (CASE expr | DEFAULT) ':' stmt*
// switch_block = '{' switch_case* '}'
//
// stmt = 'return' expr
//       | 'break' ';'
//       | 'continue' ';'
//       | '{' stmt* '}'
//       | 'if' '(' expr ')' stmt_block ('else' 'if' '(' expr ')' stmt_block)* ('else' stmt_block)?
//       | 'while' '(' expr ')' stmt_block
//       | 'for' '(' stmt_list  ';' expr ';' stmt_block ')'
//       | 'do' stmt_block 'while' '(' expr ')'
//       | 'switch' '(' expr ')' switch_block
//       | expr (INC | DEC | assign_op expr)?
//
//
//  ===== EXPRESSIONS =====
//
// cmp_op = EQ | NEQ | '<' | LTEQ | '>' | GTEQ
// add_op = '+' | '-' | '|'
// mul_op = '*' | '/' | '%' | RightShift | LeftShift | '&'
//
//
// typespec = NAME | '(' ':' type ')'
// operand_expr = INT
//              | FLOAT
//              | STR
//              | NAME
//              | typespec? '{' expr_list '}'     // compound literal
//              | CAST '(' type ')' expr
//              | '(' expr ')'
//
// base_expr = operand_expr ('(' expr_list')' | '[' expr ']' | '.' NAME)*
// unary_expr = [+-&*] unary_expr
//            | base_expr
// mul_expr = unary_expr (mul_op unary_exp)*
// add_expr = mul_expr (add_aop mul_expr )*
// cmp_expr = mul_expr (cmp_op mul_expr)*
// and_expr = cmp_expr (AND cmp_expr)*
// or_expr = and_expr (OR and_expr)*
// ternary_expr = or_expr  ('?' ternary_expr ':' ternary_expr)?
// expr = ternary_expr
//
// clang-format on

// ast

// Forward Declarations
struct TypeSpec;
struct Decl;
struct Expr;
struct Stmt;

void print_type(TypeSpec* type);
void print_decl(Decl* decl);
void print_stmt(Stmt* stmt);
void print_expr(Expr* expr);

struct StmtBlock {
  vector<Stmt*> stmts;
};

template <typename T, typename U>
T get(U u) {
  static_assert(std::is_pointer_v<T>);
  static_assert(std::is_pointer_v<U>);

  assert(dynamic_cast<T>(u->base.get()));
  return static_cast<T>(u->base.get());
}

// Types
enum class TypeSpecKind {
  None,
  Name,
  Func,
  Array,
  Pointer,
  //...
};

struct BaseTypeSpec {
  virtual ~BaseTypeSpec() = default;
};

struct NameTypeSpec : BaseTypeSpec {
  static const TypeSpecKind s_kind = TypeSpecKind::Name;
  NameTypeSpec(const char* n) : name(n) {}
  const char* name;
};

struct FuncTypeSpec : BaseTypeSpec {
  static const TypeSpecKind s_kind = TypeSpecKind::Func;
  FuncTypeSpec(vector<TypeSpec*> a, TypeSpec* r)
      : args(std::move(a)), ret_type(r) {}

  vector<TypeSpec*> args;
  TypeSpec* ret_type;
};

struct ArrayTypeSpec : BaseTypeSpec {
  static const TypeSpecKind s_kind = TypeSpecKind::Array;
  ArrayTypeSpec(TypeSpec* t, Expr* l) : type(t), len(l) {}
  TypeSpec* type;
  Expr* len;
};

struct PointerTypeSpec : BaseTypeSpec {
  static const TypeSpecKind s_kind = TypeSpecKind::Pointer;
  PointerTypeSpec(TypeSpec* t) : type(t) {}
  TypeSpec* type;
};

struct TypeSpec {
  TypeSpecKind kind;
  unique_ptr<BaseTypeSpec> base;
};

class TypeSpecs {
 public:
  ~TypeSpecs() {
    for (auto t : specs_) {
      delete t;
    }
  }

  template <typename T, typename... Args>
  TypeSpec* create(Args&&... args) {
    auto up = make_unique<T>(std::forward<Args>(args)...);
    return specs_.push_back(new TypeSpec{T::s_kind, std::move(up)});
  }

 private:
  vector<TypeSpec*> specs_;
};

void print_type(TypeSpec* type) {
  switch (type->kind) {
    case TypeSpecKind::None:
      assert(false);
      break;
    case TypeSpecKind::Name:
      printf("%s", get<NameTypeSpec*>(type)->name);
      break;
    case TypeSpecKind::Func: {
      auto* ft = get<FuncTypeSpec*>(type);
      printf("(func (");
      for (auto arg : ft->args) {
        printf(" ");
        print_type(arg);
      }
      printf(") ");
      print_type(ft->ret_type);
      printf(")");
    } break;
    case TypeSpecKind::Array: {
      auto* array = get<ArrayTypeSpec*>(type);
      printf("(array ");
      print_type(array->type);
      printf(" ");
      print_expr(array->len);
      printf(")");
    } break;
    case TypeSpecKind::Pointer: {
      auto* ptr = get<PointerTypeSpec*>(type);
      printf("(pointer ");
      print_type(ptr->type);
      printf(")");
    } break;
    default:
      assert(false);
      break;
  }
}

// Declarations
enum class DeclKind {
  None,
  Enum,
  Struct,
  Union,
  Var,
  Const,
  TypeDef,
  Func,
  //...
};

struct DeclBase {
  virtual ~DeclBase() = default;
};

struct EnumDecl : DeclBase {
  static const DeclKind s_kind = DeclKind::Enum;

  struct Data {
    const char* name;
    Expr* init;
  };

  EnumDecl(const char* n, vector<Data> i) : name(n), items(std::move(i)) {}

  const char* name;
  vector<Data> items;
};

template <DeclKind kind>
struct AggregateDecl : DeclBase {
  static const DeclKind s_kind = kind;
  struct Data {
    vector<const char*> names;
    TypeSpec* type;
  };

  AggregateDecl(const char* n, vector<Data*> i)
      : name(n), items(std::move(i)) {}
  const char* name;
  vector<Data*> items;  // TODO(sandeep) Correct this. Should it be vector<Data>
};
using StructDecl = AggregateDecl<DeclKind::Struct>;
using UnionDecl = AggregateDecl<DeclKind::Union>;

struct VarDecl : DeclBase {
  static const DeclKind s_kind = DeclKind::Var;

  VarDecl(const char* n, TypeSpec* t, Expr* e) : name(n), type(t), expr(e) {}
  const char* name;
  TypeSpec* type;
  Expr* expr;
};

struct ConstDecl : DeclBase {
  static const DeclKind s_kind = DeclKind::Const;

  ConstDecl(const char* n, Expr* e) : name(n), expr(e) {}

  const char* name;
  Expr* expr;
};

struct TypeDefDecl : DeclBase {
  static const DeclKind s_kind = DeclKind::TypeDef;

  TypeDefDecl(const char* n, TypeSpec* t) : name(n), type(t) {}

  const char* name;
  TypeSpec* type;
};

struct FuncDecl : DeclBase {
  static const DeclKind s_kind = DeclKind::Func;

  struct Param {
    const char* name;
    TypeSpec* type;
  };

  FuncDecl(const char* n, vector<Param> p, TypeSpec* ret, StmtBlock b)
      : name(n), params(std::move(p)), return_type(ret), block(std::move(b)) {}

  const char* name;
  vector<Param> params;
  TypeSpec* return_type;
  StmtBlock block;
};

struct Decl {
  DeclKind kind;
  unique_ptr<DeclBase> base;
};

class Declarations {
 public:
  ~Declarations() {
    for (auto d : decls_) {
      delete d;
    }
  }

  template <typename T, typename... Args>
  Decl* create(Args&&... args) {
    auto up = make_unique<T>(std::forward<Args>(args)...);
    return decls_.push_back(new Decl{T::s_kind, std::move(up)});
  }

 private:
  vector<Decl*> decls_;
};

template <typename T>
void print_aggregate_decl(T* decl) {
  printf("(struct %s\n", decl->name);
  for (const auto* item : decl->items) {
    printf("\n");
    printf("(");
    print_type(item->type);
    for (const auto* name : item->names) {
      printf(" %s", name);
    }
    printf(")");
  }
  printf(")");
}

void print_stmt_block(const StmtBlock& block) {
  printf("(block");
  for (auto* stmt : block.stmts) {
    printf("\n");
    print_stmt(stmt);
  }
  printf(")");
}

void print_decl(Decl* decl) {
  switch (decl->kind) {
    case DeclKind::None:
      assert(false);
      break;
    case DeclKind::Enum: {
      EnumDecl* d = get<EnumDecl*>(decl);
      printf("(enum %s", d->name);
      for (const auto& item : d->items) {
        printf("\n");
        printf("(%s ", item.name);
        if (item.init) {
          print_expr(item.init);
        } else {
          printf("nil");
        }
        printf(")");
      }
      printf(")");
    } break;
    case DeclKind::Struct: {
      print_aggregate_decl(get<StructDecl*>(decl));
    } break;
    case DeclKind::Union: {
      print_aggregate_decl(get<UnionDecl*>(decl));
    } break;
    case DeclKind::Var: {
      VarDecl* d = get<VarDecl*>(decl);
      printf("(var %s ", d->name);
      if (d->type) {
        print_type(d->type);
      } else {
        printf("nil");
      }
      printf(" ");
      print_expr(d->expr);  // TODO(sandeep) a variable declaration always has
                            // an expression?
      printf(")");
    } break;
    case DeclKind::Const: {
      ConstDecl* d = get<ConstDecl*>(decl);
      printf("(const %s", d->name);
      print_expr(d->expr);
      printf(")");
    } break;
    case DeclKind::TypeDef: {
      TypeDefDecl* d = get<TypeDefDecl*>(decl);
      printf("(const %s", d->name);
      print_type(d->type);
      printf(")");
    } break;
    case DeclKind::Func: {
      FuncDecl* d = get<FuncDecl*>(decl);
      printf("(func %s", d->name);
      printf("(");
      for (const auto& param : d->params) {
        printf(" %s ", param.name);
        print_type(param.type);
      }
      printf(") ");
      if (d->return_type) {
        print_type(d->return_type);
      } else {
        printf("nil");
      }
      print_stmt_block(d->block);
      printf(")");
    } break;
  }
}

enum class StmtKind {
  None,
  Return,
  Break,
  Continue,
  Block,
  If,
  While,
  DoWhile,
  For,
  Switch,
  Assign,
  Init,
  Expr,
};

struct StmtBase {
  virtual ~StmtBase() = default;
};

struct ReturnStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::Return;

  ReturnStmt(Expr* e) : expr(e) {}
  Expr* expr;
};

struct BreatStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::Break;
};
struct ContinueStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::Continue;
};

struct BlockStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::Block;

  BlockStmt(StmtBlock b) : block(std::move(b)) {}
  StmtBlock block;
};

struct IfStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::If;

  struct Data {
    Expr* cond;
    StmtBlock* block;
  };

  IfStmt(Expr* i, StmtBlock then, vector<Data> ei, StmtBlock eb)
      : if_expr(i),
        then_block(std::move(then)),
        elseifs(std::move(ei)),
        else_block(std::move(eb)) {}

  Expr* if_expr;
  StmtBlock then_block;
  vector<Data> elseifs;
  StmtBlock else_block;
};

struct WhileStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::While;

  WhileStmt(Expr* e, StmtBlock b) : expr(e), block(std::move(b)) {}

  Expr* expr;
  StmtBlock block;
};

struct DoWhileStmt : StmtBase {
  DoWhileStmt(StmtBlock b, Expr* e) : block(std::move(b)), expr(e) {}

  StmtBlock block;
  Expr* expr;
};

struct ForStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::For;

  ForStmt(Stmt* i, Expr* c, Stmt* n, StmtBlock b)
      : init(i), cond(c), next(n), block(std::move(b)) {}

  Stmt* init;
  Expr* cond;
  Stmt* next;
  StmtBlock block;
};

struct SwitchStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::Switch;

  struct Data {
    vector<Expr*> exprs;
    bool is_default;
    StmtBlock block;
  };

  SwitchStmt(Expr* e, vector<Data*> c) : expr(e), cases(std::move(c)) {}

  Expr* expr;
  vector<Data*> cases;
};

struct AssignStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::Assign;

  AssignStmt(TokenKind o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

  TokenKind op;
  Expr* left;
  Expr* right;
};

struct InitStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::Init;

  InitStmt(const char* n, Expr* e) : name(n), expr(e) {}

  const char* name;
  Expr* expr;
};

struct ExprStmt : StmtBase {
  static const StmtKind s_kind = StmtKind::Expr;

  ExprStmt(Expr* e) : expr(e) {}
  Expr* expr;
};

struct Stmt {
  StmtKind kind;
  unique_ptr<StmtBase> base;
};

class Statements {
 public:
  ~Statements() {
    for (auto* stmt : stmts_) {
      delete stmt;
    }
  }

  template <typename T, typename... Args>
  Stmt* create(Args&&... args) {
    auto up = make_unique<T>(std::forward<Args>(args)...);
    return stmts_.push_back(new Stmt{T::s_kind, std::move(up)});
  }

 private:
  vector<Stmt*> stmts_;  // owning
};

void print_stmt(Stmt* stmt) {
  switch (stmt->kind) {
    case StmtKind::None:
      assert(false);
      break;
    case StmtKind::Return: {
    } break;
    case StmtKind::Break: {
    } break;
    case StmtKind::Continue: {
    } break;
    case StmtKind::Block: {
    } break;
    case StmtKind::If: {
    } break;
    case StmtKind::While: {
    } break;
    case StmtKind::DoWhile: {
    } break;
    case StmtKind::For: {
    } break;
    case StmtKind::Switch: {
    } break;
    case StmtKind::Assign: {
    } break;
    case StmtKind::Init: {
    } break;
    case StmtKind::Expr: {
    } break;
  }
}

enum class ExprKind {
  None,
  Int,
  Float,
  Name,
  Str,
  Cast,
  Compound,
  Call,
  Index,
  Field,
  Unary,
  Binary,
  Ternary
};

struct ExprBase {
  virtual ~ExprBase() = default;
};

template <typename T, ExprKind kind>
struct LiteralExpr : ExprBase {
  static const ExprKind s_kind = kind;
  LiteralExpr(T v) : val(v) {}
  T val;
};

using IntExpr = LiteralExpr<uint64_t, ExprKind::Int>;
using FloatExpr = LiteralExpr<double, ExprKind::Float>;
using NameExpr = LiteralExpr<const char*, ExprKind::Name>;
using StrExpr = LiteralExpr<const char*, ExprKind::Str>;

struct CastExpr : ExprBase {
  static const ExprKind s_kind = ExprKind::Cast;
  CastExpr(TypeSpec* t, Expr* e) : type(t), expr(e) {}
  TypeSpec* type;
  Expr* expr;
};

struct CompoundLiteralExpr : ExprBase {
  static const ExprKind s_kind = ExprKind::Compound;
  CompoundLiteralExpr(TypeSpec* t, vector<Expr*> a)
      : type(t), args(std::move(a)) {}
  TypeSpec* type;
  vector<Expr*> args;
};

struct UnaryExpr : ExprBase {
  static const ExprKind s_kind = ExprKind::Unary;
  UnaryExpr(TokenKind o, Expr* e) : op(o), operand(e) {}
  TokenKind op;
  Expr* operand;
};

struct CallExpr : ExprBase {
  static const ExprKind s_kind = ExprKind::Call;
  CallExpr(Expr* o, vector<Expr*> a) : operand(o), args(std::move(a)) {}
  Expr* operand;
  vector<Expr*> args;
};

struct IndexExpr : ExprBase {
  static const ExprKind s_kind = ExprKind::Index;
  IndexExpr(Expr* o, Expr* i) : operand(o), index(i) {}
  Expr* operand;
  Expr* index;
};

struct FieldExpr : ExprBase {
  static const ExprKind s_kind = ExprKind::Field;
  FieldExpr(Expr* e, const char* f) : operand(e), field(f) {}
  Expr* operand;
  const char* field;
};

struct BinaryExpr : ExprBase {
  static const ExprKind s_kind = ExprKind::Binary;
  BinaryExpr(TokenKind o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

  TokenKind op;
  Expr* left;
  Expr* right;
};

struct TernaryExpr : ExprBase {
  static const ExprKind s_kind = ExprKind::Ternary;
  TernaryExpr(Expr* c, Expr* then, Expr* el)
      : cond(c), then_expr(then), else_expr(el) {}

  Expr* cond;
  Expr* then_expr;
  Expr* else_expr;
};

struct Expr {
  ExprKind kind;
  unique_ptr<ExprBase> base;
};

class Expressions {
 public:
  ~Expressions() {
    for (int i = 0; i < exprs_.size(); ++i) {
      delete exprs_[i];
    }
  }

  template <typename T, typename... Args>
  Expr* create(Args&&... args) {
    auto up = make_unique<T>(std::forward<Args>(args)...);
    return exprs_.push_back(new Expr{T::s_kind, std::move(up)});
  }

 private:
  vector<Expr*> exprs_;  // owning
};

void print_expr(Expr* expr) {
  switch (expr->kind) {
    case ExprKind::None:
      assert(false);
      break;
    case ExprKind::Int:
      printf("%lu", get<IntExpr*>(expr)->val);
      break;
    case ExprKind::Float:
      printf("%f", get<FloatExpr*>(expr)->val);
      break;
    case ExprKind::Name:
      printf("%s", get<NameExpr*>(expr)->val);
      break;
    case ExprKind::Str:
      printf("\"%s\"", get<StrExpr*>(expr)->val);
      break;
    case ExprKind::Cast: {
      auto* exp = get<CastExpr*>(expr);

      printf("(cast ");
      print_type(exp->type);
      printf(" ");
      print_expr(exp->expr);
      printf(")");
    } break;
    case ExprKind::Call: {
      auto* call = get<CallExpr*>(expr);
      printf("(");
      print_expr(call->operand);
      for (auto arg : call->args) {
        printf(" ");
        print_expr(arg);
      }
      printf(")");
    } break;
    case ExprKind::Index: {
      auto* index = get<IndexExpr*>(expr);
      printf("(index ");
      print_expr(index->operand);
      printf(" ");
      print_expr(index->index);
      printf(")");
    } break;
    case ExprKind::Field: {
      auto* field = get<FieldExpr*>(expr);
      printf("(field ");
      print_expr(field->operand);
      printf(" %s)", field->field);
    } break;
    case ExprKind::Binary: {
      auto* bin = get<BinaryExpr*>(expr);
      printf("(%c ", bin->op);
      print_expr(bin->left);
      printf(" ");
      print_expr(bin->right);
      printf(")");
    } break;
    case ExprKind::Compound: {
      printf("(compound...)");
    } break;
    case ExprKind::Unary: {
      auto* un = get<UnaryExpr*>(expr);
      printf("(%c ", un->op);
      print_expr(un->operand);
      printf(")");
    } break;
    case ExprKind::Ternary: {
      auto* tern = get<TernaryExpr*>(expr);
      printf("(if ");
      print_expr(tern->cond);
      printf(" ");
      print_expr(tern->then_expr);
      printf(" ");
      print_expr(tern->else_expr);
      printf(")");
      break;
    }
  }
}

void expr_test() {
  auto print = [](Expr* expr) {
    print_expr(expr);
    printf("\n");
  };

  Expressions exprs{};
  TypeSpecs types{};

  vector<Expr*> args{};
  args.push_back(exprs.create<IntExpr>(42));
  args.push_back(exprs.create<IntExpr>(1779));

  Expr* collection[] = {
      exprs.create<BinaryExpr>((TokenKind)'+', exprs.create<IntExpr>(1),
                               exprs.create<IntExpr>(2)),
      exprs.create<UnaryExpr>((TokenKind)'-', exprs.create<FloatExpr>(3.14)),
      exprs.create<TernaryExpr>(exprs.create<NameExpr>("cond"),
                                exprs.create<StrExpr>("then"),
                                exprs.create<StrExpr>("else")),
      exprs.create<FieldExpr>(exprs.create<NameExpr>("person"), "name"),
      exprs.create<CallExpr>(exprs.create<NameExpr>("add"), std::move(args)),
      exprs.create<IndexExpr>(
          exprs.create<FieldExpr>(exprs.create<NameExpr>("person"), "siblings"),
          exprs.create<IntExpr>(3)),
      exprs.create<CastExpr>(types.create<NameTypeSpec>("int_ptr"),
                             exprs.create<NameExpr>("void_ptr")),
  };

  for (auto expr : collection) {
    print(expr);
  }
}

void stmt_test() {
  auto print = [](Stmt* stmt) {
    print_stmt(stmt);
    printf("\n");
  };

  Expressions exprs{};
  TypeSpecs types{};
  Statements stmts{};

  StmtBlock block1{};
  block1.stmts.push_back(stmts.create<BreatStmt>());
  block1.stmts.push_back(stmts.create<ContinueStmt>());

  vector<Expr*> args1{};
  args1.push_back(exprs.create<IntExpr>(1));
  args1.push_back(exprs.create<IntExpr>(2));

  StmtBlock block2{};
  block2.stmts.push_back(stmts.create<ReturnStmt>(exprs.create<IntExpr>(1)));

  StmtBlock block3;
  block3.stmts.push_back(stmts.create<ReturnStmt>(exprs.create<IntExpr>(2)));

  vector<IfStmt::Data> block4{};
  block4.push_back({exprs.create<NameExpr>("flag2"), &block3});

  StmtBlock block5{};
  block5.stmts.push_back(stmts.create<ReturnStmt>(exprs.create<IntExpr>(3)));

  StmtBlock block6{};
  block5.stmts.push_back(stmts.create<AssignStmt>(TokenKind::TokenAddAssign,
                                                  exprs.create<NameExpr>("i"),
                                                  exprs.create<IntExpr>(16)));

  SwitchStmt::Data case1{};
  case1.exprs.push_back(exprs.create<IntExpr>(3));
  case1.exprs.push_back(exprs.create<IntExpr>(4));
  case1.is_default = false;
  case1.block.stmts.push_back(
      stmts.create<ReturnStmt>(exprs.create<NameExpr>("val")));

  SwitchStmt::Data case2{};
  case2.exprs.push_back(exprs.create<IntExpr>(3));
  case2.is_default = true;
  case2.block.stmts.push_back(
      stmts.create<ReturnStmt>(exprs.create<IntExpr>(0)));

  vector<SwitchStmt::Data*> cases{};
  cases.push_back(&case1);
  cases.push_back(&case2);

  // clang-format off
  Stmt* collection[] = {
      stmts.create<ReturnStmt>(exprs.create<IntExpr>(42)),
      stmts.create<BreatStmt>(),
      stmts.create<ContinueStmt>(),
      stmts.create<BlockStmt>(std::move(block1)),
      stmts.create<ExprStmt>(exprs.create<CallExpr>(
          exprs.create<NameExpr>("print"), std::move(args1))),
      stmts.create<InitStmt>("x", exprs.create<IntExpr>(42)),
      stmts.create<IfStmt>(
          exprs.create<NameExpr>("flag1"),
          std::move(block2),
          std::move(block4),
          std::move(block5)
          ),
      stmts.create<WhileStmt>(
          exprs.create<NameExpr>("running"),
          std::move(block6)
          ),
      stmts.create<SwitchStmt>(
          exprs.create<NameExpr>("val"),
          std::move(cases)
          )
  };
  // clang-format on

  for (auto stmt : collection) {
    print(stmt);
  }
}

void ast_test() {
  expr_test();
  stmt_test();
}

void run_tests() {
  vec_test();
  str_intern_test();
  lex_test();
  ast_test();
}

int main() {
  run_tests();
  puts("tests ran successfully...");
  return 0;
}
