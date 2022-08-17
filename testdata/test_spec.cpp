namespace spec {

namespace base {

struct SBase {
    int a;
    float b;
};

template <typename T, int N=3, typename U=char>
class Base {
    T m_arr[N];

    void private_method();

public:
    Base();
    Base(const Base&);

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

}

using BaseFloat1 = ::spec::base::Base<float, 1>;
typedef ::spec::base::Base<float, 2> BaseFloat2;
using BaseInt3 = ::spec::base::Base<int>;

}

