#ifndef BBL_HPP
#define BBL_HPP

#define BBL_MODULE(name) \
    static void bbl_bind(const char *modname = #name)

namespace bbl
{
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
    };
}

void rename_namespace(const char *from, const char *to) {}

#endif