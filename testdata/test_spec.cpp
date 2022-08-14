template <typename T, int N=3>
class Base {
public:
    T get_t();
    void take_n(T t[N]);
};

template <typename T>
void take_base_t(Base<T> b);

void take_base_int(Base<int> b);

void take_base_int_2(Base<int, 2> b);

using BaseFloat1 = Base<float, 1>;
typedef Base<float, 2> BaseFloat2;
using BaseInt3 = Base<int>;