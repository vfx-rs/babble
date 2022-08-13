namespace Imath_3_1 {

template <typename T>
class Vec3 {
public:
    T return_t();

    const T* take_t_return_pointer(const T& a);

    template <typename S>
    T take_s_return_t(S a);

    template <typename S>
    void take_s_return_void(S a);

    template <typename S>
    int take_s_and_t_return_int(S a, T b);
};

}