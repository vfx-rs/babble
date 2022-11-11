#define BBL_MODULE(name) \
    static void bbl_bind(const char *modname = #name)

template <typename C>
class Class
{
public:
    Class(const char *name)
    {
    }

    template <typename Func>
    Class m(Func fn)
    {
        return *this;
    }

    template <typename Func>
    Class m(Func fn, const char *rename)
    {
        return *this;
    }

    Class rename(const char *name) { return *this; }
};

struct Foo
{
    int a;
    void baz();
};

template <typename T>
struct Bar
{
    T t;
};

typedef Bar<int> IntBar;
typedef Bar<Foo> FooBar;

class Base
{
public:
    void do_inherited_thing();
};

class Test : public Base
{
    int _a;

public:
    float b;
    void do_thing();

    template <typename T>
    T do_templated_thing(T *);
};

BBL_MODULE(Test::Module)
{
    Class<Foo>("Foo")
        .rename("Foo")
        .m(&Foo::baz);
    // Class<Bar<Foo>>("FooBar");

    // Class<Test>("Test")
    //     .m(&Test::do_thing)
    //     .m(&Test::do_inherited_thing)
    //     .m(&Test::do_templated_thing<Foo>, "do_templated_thing_foo")
    //     .m(&Test::do_templated_thing<FooBar>, "do_templated_thing_foobar")
    //     .m(&Test::do_templated_thing<int>, "do_templated_thing_int");
}