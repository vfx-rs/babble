#pragma once

#include "ImathBox.h"

namespace Imath_3_1
{
    template <typename V>
    class Box
    {
    public:
        V min;
        V max;

        V size() const;
        V center() const;
        void extendBy(const V &point);
    };

    typedef Box<V3f> Box3f;
    typedef Box<V3i> Box3i;
    typedef Box<V3d> Box3d;
}

namespace Imath = Imath_3_1;
