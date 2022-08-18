#define IMATH_INTERNAL_NAMESPACE Imath_3_1

#ifndef IMATH_NAMESPACE
#    define IMATH_NAMESPACE Imath
#endif

#ifndef IMATH_INTERNAL_NAMESPACE
#    define IMATH_INTERNAL_NAMESPACE IMATH_NAMESPACE
#endif

#ifdef __cplusplus

//
// We need to be sure that we import the internal namespace into the public one.
// To do this, we use the small bit of code below which initially defines
// IMATH_INTERNAL_NAMESPACE (so it can be referenced) and then defines
// IMATH_NAMESPACE and pulls the internal symbols into the public
// namespace.
//

namespace IMATH_INTERNAL_NAMESPACE
{}
namespace IMATH_NAMESPACE
{
using namespace IMATH_INTERNAL_NAMESPACE;
}

//
// There are identical pairs of HEADER/SOURCE ENTER/EXIT macros so that
// future extension to the namespace mechanism is possible without changing
// project source code.
//

#define IMATH_INTERNAL_NAMESPACE_HEADER_ENTER                                                      \
    namespace IMATH_INTERNAL_NAMESPACE                                                             \
    {
#define IMATH_INTERNAL_NAMESPACE_HEADER_EXIT }

#define IMATH_INTERNAL_NAMESPACE_SOURCE_ENTER                                                      \
    namespace IMATH_INTERNAL_NAMESPACE                                                             \
    {
#define IMATH_INTERNAL_NAMESPACE_SOURCE_EXIT }

#endif // __cplusplus

/// @endcond

#define IMATH_USE_NOEXCEPT 1
#if IMATH_USE_NOEXCEPT
#define IMATH_NOEXCEPT noexcept
#else
#define IMATH_NOEXCEPT
#endif

#ifndef IMATH_FOREIGN_VECTOR_INTEROP
#  if defined(__GNUC__) && __GNUC__ == 4 && !defined(__clang__)
#    define IMATH_FOREIGN_VECTOR_INTEROP 0
#  else
#    define IMATH_FOREIGN_VECTOR_INTEROP 1
#  endif
#endif


//
// Decorator that makes a function available for both CPU and GPU, when
// compiling for Cuda.
//
#ifdef __CUDACC__
  #define IMATH_HOSTDEVICE __host__ __device__
#else
  #define IMATH_HOSTDEVICE
#endif

#if (IMATH_CPLUSPLUS_VERSION >= 14)
  #define IMATH_CONSTEXPR14 constexpr
#else
  #define IMATH_CONSTEXPR14 /* can not be constexpr before c++14 */
#endif

namespace Imath_3_1 {

template <class T> class Vec3;

template <class T> class Vec3
{
  public:

    /// @{
    /// @name Direct access to elements

    T x, y, z;

    /// @}
    
    /// Element access by index.  
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 T& operator[] (int i) IMATH_NOEXCEPT;

    /// Element access by index.  
    IMATH_HOSTDEVICE constexpr const T& operator[] (int i) const IMATH_NOEXCEPT;

    /// @{
    ///	@name Constructors and Assignment

    /// Uninitialized by default
    IMATH_HOSTDEVICE Vec3() IMATH_NOEXCEPT;
    
    /// Initialize to a scalar `(a,a,a)`
    IMATH_HOSTDEVICE constexpr explicit Vec3 (T a) IMATH_NOEXCEPT;

    /// Initialize to given elements `(a,b,c)`
    IMATH_HOSTDEVICE constexpr Vec3 (T a, T b, T c) IMATH_NOEXCEPT;

    /// Copy constructor
    IMATH_HOSTDEVICE constexpr Vec3 (const Vec3& v) IMATH_NOEXCEPT;

    /// Construct from Vec3 of another base type
    template <class S> IMATH_HOSTDEVICE constexpr Vec3 (const Vec3<S>& v) IMATH_NOEXCEPT;

    /// Assignment
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& operator= (const Vec3& v) IMATH_NOEXCEPT;

    /// Destructor
    ~Vec3() IMATH_NOEXCEPT = default;

    /// @}


    /// @{
    /// @name Compatibility with Sb

    /// Set the value
    template <class S> IMATH_HOSTDEVICE void setValue (S a, S b, S c) IMATH_NOEXCEPT;

    /// Set the value
    template <class S> IMATH_HOSTDEVICE void setValue (const Vec3<S>& v) IMATH_NOEXCEPT;

    /// Return the value in `a`, `b`, and `c`
    template <class S> IMATH_HOSTDEVICE void getValue (S& a, S& b, S& c) const IMATH_NOEXCEPT;

    /// Return the value in `v`
    template <class S> IMATH_HOSTDEVICE void getValue (Vec3<S>& v) const IMATH_NOEXCEPT;

    /// Return a raw pointer to the array of values
    IMATH_HOSTDEVICE T* getValue() IMATH_NOEXCEPT;

    /// Return a raw pointer to the array of values
    IMATH_HOSTDEVICE const T* getValue() const IMATH_NOEXCEPT;

    /// @}

    /// @{
    /// @name Arithmetic and Comparison
    
    /// Equality
    template <class S> IMATH_HOSTDEVICE constexpr bool operator== (const Vec3<S>& v) const IMATH_NOEXCEPT;

    /// Inequality
    template <class S> IMATH_HOSTDEVICE constexpr bool operator!= (const Vec3<S>& v) const IMATH_NOEXCEPT;

    /// Compare two matrices and test if they are "approximately equal":
    /// @return True if the coefficients of this and `m` are the same
    /// with an absolute error of no more than e, i.e., for all i, j:
    ///
    ///     abs (this[i][j] - m[i][j]) <= e
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 bool equalWithAbsError (const Vec3<T>& v, T e) const IMATH_NOEXCEPT;

    /// Compare two matrices and test if they are "approximately equal":
    /// @return True if the coefficients of this and m are the same with
    /// a relative error of no more than e, i.e., for all i, j:
    ///
    ///     abs (this[i] - v[i][j]) <= e * abs (this[i][j])
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 bool equalWithRelError (const Vec3<T>& v, T e) const IMATH_NOEXCEPT;

    /// Dot product
    IMATH_HOSTDEVICE constexpr T dot (const Vec3& v) const IMATH_NOEXCEPT;

    /// Dot product
    IMATH_HOSTDEVICE constexpr T operator^ (const Vec3& v) const IMATH_NOEXCEPT;

    /// Right-handed cross product
    IMATH_HOSTDEVICE constexpr Vec3 cross (const Vec3& v) const IMATH_NOEXCEPT;

    /// Right-handed cross product
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& operator%= (const Vec3& v) IMATH_NOEXCEPT;

    /// Right-handed cross product
    IMATH_HOSTDEVICE constexpr Vec3 operator% (const Vec3& v) const IMATH_NOEXCEPT;

    /// Component-wise addition
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& operator+= (const Vec3& v) IMATH_NOEXCEPT;

    /// Component-wise addition
    IMATH_HOSTDEVICE constexpr Vec3 operator+ (const Vec3& v) const IMATH_NOEXCEPT;

    /// Component-wise subtraction
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& operator-= (const Vec3& v) IMATH_NOEXCEPT;

    /// Component-wise subtraction
    IMATH_HOSTDEVICE constexpr Vec3 operator- (const Vec3& v) const IMATH_NOEXCEPT;

    /// Component-wise multiplication by -1
    IMATH_HOSTDEVICE constexpr Vec3 operator-() const IMATH_NOEXCEPT;

    /// Component-wise multiplication by -1
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& negate() IMATH_NOEXCEPT;

    /// Component-wise multiplication
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& operator*= (const Vec3& v) IMATH_NOEXCEPT;

    /// Component-wise multiplication
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& operator*= (T a) IMATH_NOEXCEPT;

    /// Component-wise multiplication
    IMATH_HOSTDEVICE constexpr Vec3 operator* (const Vec3& v) const IMATH_NOEXCEPT;

    /// Component-wise multiplication
    IMATH_HOSTDEVICE constexpr Vec3 operator* (T a) const IMATH_NOEXCEPT;

    /// Component-wise division
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& operator/= (const Vec3& v) IMATH_NOEXCEPT;

    /// Component-wise division
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 const Vec3& operator/= (T a) IMATH_NOEXCEPT;

    /// Component-wise division
    IMATH_HOSTDEVICE constexpr Vec3 operator/ (const Vec3& v) const IMATH_NOEXCEPT;

    /// Component-wise division
    IMATH_HOSTDEVICE constexpr Vec3 operator/ (T a) const IMATH_NOEXCEPT;

    /// @}

    /// @{
    /// @name Query and Manipulation

    /// Return the Euclidean norm
    IMATH_HOSTDEVICE T length() const IMATH_NOEXCEPT;

    /// Return the square of the Euclidean norm, i.e. the dot product
    /// with itself.
    IMATH_HOSTDEVICE constexpr T length2() const IMATH_NOEXCEPT;

    /// Normalize in place. If length()==0, return a null vector.
    IMATH_HOSTDEVICE const Vec3& normalize() IMATH_NOEXCEPT;

    /// Normalize in place. If length()==0, throw an exception.
    const Vec3& normalizeExc();

    /// Normalize without any checks for length()==0. Slightly faster
    /// than the other normalization routines, but if v.length() is
    /// 0.0, the result is undefined.
    IMATH_HOSTDEVICE const Vec3& normalizeNonNull() IMATH_NOEXCEPT;

    /// Return a normalized vector. Does not modify *this.
    IMATH_HOSTDEVICE Vec3<T> normalized() const IMATH_NOEXCEPT; // does not modify *this

    /// Return a normalized vector. Does not modify *this. Throw an
    /// exception if length()==0.
    Vec3<T> normalizedExc() const;

    /// Return a normalized vector. Does not modify *this, and does
    /// not check for length()==0. Slightly faster than the other
    /// normalization routines, but if v.length() is 0.0, the result
    /// is undefined.
    IMATH_HOSTDEVICE Vec3<T> normalizedNonNull() const IMATH_NOEXCEPT;

    /// @}

    /// @{
    /// @name Numeric Limits

    /// Largest possible negative value
    IMATH_HOSTDEVICE constexpr static T baseTypeLowest() IMATH_NOEXCEPT { return std::numeric_limits<T>::lowest(); }

    /// Largest possible positive value
    IMATH_HOSTDEVICE constexpr static T baseTypeMax() IMATH_NOEXCEPT { return std::numeric_limits<T>::max(); }

    /// Smallest possible positive value
    IMATH_HOSTDEVICE constexpr static T baseTypeSmallest() IMATH_NOEXCEPT { return std::numeric_limits<T>::min(); }

    /// Smallest possible e for which 1+e != 1
    IMATH_HOSTDEVICE constexpr static T baseTypeEpsilon() IMATH_NOEXCEPT { return std::numeric_limits<T>::epsilon(); }

    /// @}
    
    /// Return the number of dimensions, i.e. 3
    IMATH_HOSTDEVICE constexpr static unsigned int dimensions() IMATH_NOEXCEPT { return 3; }

    /// The base type: In templates that accept a parameter `V`, you
    /// can refer to `T` as `V::BaseType`
    typedef T BaseType;

  private:
    IMATH_HOSTDEVICE IMATH_CONSTEXPR14 T lengthTiny() const IMATH_NOEXCEPT;
};

}
