#ifndef BBL_HPP
#define BBL_HPP

#define BBL_MODULE(name) \
    static void bbl_bind(const char *modname = #name)

namespace bbl
{
    template <typename... Args>
    struct Ctor
    {
    };

    struct CopyCtor
    {
    };
    struct MoveCtor
    {
    };
    struct Dtor
    {
    };

    template <typename C>
    class Class
    {
    public:
        Class()
        {
        }

        Class(const char *name)
        {
        }

        template <
            typename... Args,
            typename = std::enable_if_t<std::is_constructible<C, Args...>::value>>
        Class m(Ctor<Args...> ctor, const char *rename = "")
        {
            return *this;
        }

        template <
            typename = std::enable_if_t<std::is_copy_constructible<C>::value>>
        Class m(CopyCtor ctor, const char *rename = "")
        {
            return *this;
        }

        template <
            typename = std::enable_if_t<std::is_move_constructible<C>::value>>
        Class m(MoveCtor ctor, const char *rename = "")
        {
            return *this;
        }

        template <
            typename = std::enable_if_t<std::is_destructible<C>::value>>
        Class m(Dtor ctor, const char *rename = "")
        {
            return *this;
        }

        template <typename Func>
        Class m(Func fn, const char *rename = "")
        {
            return *this;
        }
    };

    template <typename Result, typename... Args>
    void fn(Result (*fun)(Args...), const char *rename = "") {}

    void rename_namespace(const char *from, const char *to)
    {
    }
}

#endif