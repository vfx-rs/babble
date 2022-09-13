template <typename T, int N=4>
class shared_ptr {
    T* t;

public:
    const T* get() const;
    T* get();
};

class A {int a;};
class B {int b;};

using APtr = shared_ptr<A>;
typedef shared_ptr<B> BPtr;

using APtr2 = APtr;
typedef BPtr BPtr2;