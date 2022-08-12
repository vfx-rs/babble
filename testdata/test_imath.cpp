#include <Imath/ImathVec.h>
#include <string>

namespace cppmm_bind {

namespace IMATH_INTERNAL_NAMESPACE {

namespace Imath = ::IMATH_INTERNAL_NAMESPACE;

template <typename T>
class Vec3<T> {
    using BoundType = Imath::Vec3<T>;

} __attribute__((annotate("CPPMM|valuetype")));

using V3f = Imath::Vec3<float>;

// using V3i = Imath::Vec3<int>;

}

}