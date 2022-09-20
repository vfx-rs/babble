namespace ns1 {

template<typename T>
class shared_ptr {
    T* ptr;
};

}

namespace ns2 {
class A {};

void take_sptr(ns1::shared_ptr<A>);
}

namespace ns3 {
typedef ns1::shared_ptr<ns2::A> APtr;
void take_aptr(APtr);
void take_sp_a(ns1::shared_ptr<ns2::A>);
typedef const int Int;
}

typedef ns2::A B;
typedef ns3::APtr BPtr;
void take_b(B);
void take_bptr(BPtr);
void take_Int(ns3::Int);