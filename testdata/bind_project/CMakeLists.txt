cmake_minimum_required(VERSION 3.5)
project(bind)

find_package(Imath REQUIRED)
find_package(take_string REQUIRED)

add_executable(bind bind_imath.cpp bind_take_string.cpp)
target_link_libraries(bind PUBLIC Imath::Imath take_string::take_string)
