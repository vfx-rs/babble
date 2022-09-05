#pragma once 

#include <string>
#include <iostream>

namespace Test_1_0 {

class Class {
public:
    std::string take_string(const std::string& s) {
        return std::string("Hello, ") + s + "!";
    }
};

}