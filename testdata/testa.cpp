template <class T>
class shared_ptr {
    T* t;
};


namespace A {

/// Some awesome class that we've written.
///
/// How awesome? SO awesome.
class Test {
public:
    int do_something(float a);
};

void free_function(int a, const Test& b);

}

namespace cppmm_bind {

/// A comment here
class Test  {
    using BoundType = ::A::Test;

    int do_something(float a);

} __attribute__((annotate("cppmm|blah")));

using TestPtr = shared_ptr<::A::Test>;

auto& free_function = ::A::free_function;

}