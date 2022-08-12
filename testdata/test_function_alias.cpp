template <typename T>
T do_something(const T& t);

int do_other(int& a);
float do_other(float& a);

auto& do_something_int = do_something<int>;

const auto& do_other_int = static_cast<int(*)(int&)>(do_other);
const auto& do_other_float = static_cast<float(*)(float&)>(do_other);