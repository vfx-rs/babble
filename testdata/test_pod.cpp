class A {
public:
    int a;
    float b;
};

class B {
public:
    A a;
};

class C {
public:
    int a;
    float b;
    C(const C& c) { a = c.a; b = c.b; }
};

class D {
public:
    C c;
};