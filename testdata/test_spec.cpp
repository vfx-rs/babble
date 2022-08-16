// #include <memory>

namespace spec {

template <typename T, int N=3, typename U=char>
class Base {
public:
    T get_t();
    void take_n(T t[N]);
};

template <typename T>
void take_base_t(Base<T> b);

void take_base_int(Base<int> b);

void take_base_int_2(Base<int, 2> b);

class Concrete {
public:
    int get_int();
};

using BaseFloat1 = spec::Base<float, 1>;
typedef spec::Base<float, 2> BaseFloat2;
using BaseInt3 = spec::Base<int>;

// using ConcretePtr = std::shared_ptr<Concrete>;
// typedef std::shared_ptr<BaseFloat1> BaseFloat1Ptr;
// using BaseFloat2Ptr = std::shared_ptr<BaseFloat2>;
// typedef std::shared_ptr<BaseInt3> BaseInt3Ptr;

}

