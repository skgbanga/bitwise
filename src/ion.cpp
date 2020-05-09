#include <cassert>
#include <cstdlib>
#include <cstring>
#include <type_traits>
#include <cstdio>

template <typename T>
struct vector {
  static_assert(std::is_trivial_v<T> && std::is_standard_layout_v<T>);
  ~vector() { delete[] ptr_; }
  void push_back(const T& t) {
    if (size_ == capacity_) {
      grow();
    }
    assert(size_ < capacity_);

    ptr_[size_++] = t;
    return;
  }
  int size() const { return size_; }
  int capacity() const { return capacity_; }
  T& operator[](int idx) { return ptr_[idx]; }

 private:
  void grow() {
    assert(size_ == capacity_);
    auto new_capacity = (capacity_) ? 2 * capacity_ : 1;
    T* nptr = ::new T(new_capacity);
    assert(nptr);
    std::memcpy(nptr, ptr_, sizeof(T) * size_);
    ptr_ = nptr;
    capacity_ = new_capacity;
  }

  int capacity_ = 0;
  int size_ = 0;
  T* ptr_ = nullptr;
};

int main() {
  vector<int> buf;
  buf.push_back(42);
  buf.push_back(1234);
  for (int i = 0; i < buf.size(); ++i) {
    printf("%d\n", buf[i]);
  }
  return 0;
}
