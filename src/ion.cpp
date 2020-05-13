#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

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
  const int N = 1024;
  for (int i = 0; i < N; ++i) {
    buf.push_back(i);
  }
  assert(buf.size() == N);
  for (int i = 0; i < N; ++i) {
    assert(buf[i] == i);
  }
}

enum TokenKind { TokenInt = 128, TokenName };

struct Token {
  TokenKind kind;
  union {
    uint64_t val;
    struct {
      const char* start;
      const char* end;
    };
  };
};
void print_token(const Token& token) {
  printf("kind: %d", token.kind);
  switch (token.kind) {
    case TokenInt:
      printf(" value: %lu", token.val);
      break;
    case TokenName:
      printf(" value: %.*s", (int)(token.end - token.start), token.start);
      break;
    default:
      break;
  }
  puts("");
}

class Tokenizer {
 public:
  Tokenizer(const char* stream) : stream_(stream) {}
  Token next() {
    Token token{};
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
        token.start = stream_;
        while (std::isalnum(*stream_) || (*stream_ == '_')) {
          stream_++;
        }
        token.end = stream_;
        token.kind = TokenName;
      } break;
      default:
        token.kind = static_cast<TokenKind>(*stream_++);
        break;
    }
    return token;
  }

 private:
  const char* stream_;
};

void lex_test() {
  const char* str = "+()_HELLO+997-hello";
  Tokenizer tokenizer(str);
  auto token = tokenizer.next();
  while (token.kind) {
    print_token(token);
    token = tokenizer.next();
  }
}

int main() {
  vec_test();
  lex_test();
}
