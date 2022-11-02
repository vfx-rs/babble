namespace detail {
template <typename Key, typename Value> class map {
public:
  typedef Key key_type;
  typedef Value value_type;

  void insert(key_type k, value_type v);
  const value_type &at(const key_type &k);
};
} // namespace detail

namespace Test {
class Class : private detail::map<int, int> {
public:
  using map::key_type;
  using map::value_type;

  using map::at;
  using map::insert;
  using map::map;

  void erase(const key_type &key);
};
} // namespace Test
