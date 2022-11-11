#pragma once

namespace Imath_3_1
{
    template <typename T>
    class Vec3
    {
    public:
        T x, y, z;

        T length() const;
    };

    typedef Vec3<float> V3f;
    typedef Vec3<int> V3i;
    typedef Vec3<double> V3d;
}

namespace Imath = Imath_3_1;