#pragma once 

#include <string>
#include <iostream>

namespace Test_1_0 {

typedef void(*Callback)(int);

class Class {
public:
    int a;
    // Class(){}

    std::string take_string(const std::string& s) {
        return std::string("Hello, ") + s + "!";
    }

    void take_callback(Callback cb) {

    }
};

}