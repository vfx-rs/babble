#include "bbl.hpp"

#include <Imath/ImathVec.h>
#include <Imath/ImathBox.h>

#define BIND_V3(NAME)              \
    bbl::Class<Imath::NAME>(#NAME) \
        .m(&Imath::NAME::length);

BBL_MODULE(imath)
{
    rename_namespace("Imath_3_1", "Imath");
    // bbl::Class<Imath::V3f>("V3f")
    //     .m(&Imath::V3f::length);
    BIND_V3(V3f);
    BIND_V3(V3i);

    // bbl::Class<Imath::Box3f>("Box3f")
    //     .rename("RENAMEBox3f")
    //     .m(&Imath::Box3f::extendBy);
}