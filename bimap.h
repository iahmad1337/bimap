#pragma once

#include "splay_tree.h"
#include <cstddef>
#include <functional>
#include <stdexcept>

namespace detail {
struct left_tag;
struct right_tag;

struct left_tag {
  using flip = right_tag;
};

struct right_tag {
  using flip = left_tag;
};

template <typename Tag>
constexpr inline bool is_left_tag = std::is_same_v<Tag, left_tag>;

template <typename L, typename R>
struct bimap_node : public splay_node<L, left_tag>,
                    public splay_node<R, right_tag> {
  using lbase = splay_node<L, left_tag>;
  using rbase = splay_node<R, right_tag>;

  template <typename Tag>
  using base = std::conditional_t<std::is_same_v<left_tag, Tag>, lbase, rbase>;

  template <typename Tag>
  static bimap_node* from_splay_node_base(splay_node_base* node) {
    return static_cast<bimap_node*>(static_cast<base<Tag>*>(node));
  }

  template <typename Left, typename Right>
  bimap_node(Left&& l_val, Right&& r_val)
      : lbase{std::forward<Left>(l_val)}, rbase{std::forward<Right>(r_val)} {}

  template <typename Tag, typename T>
  std::conditional_t<std::is_same_v<Tag, left_tag>, L, R>& get_tagged() {
    if constexpr (std::is_same_v<Tag, left_tag>) {
      return lbase::get();
    } else {
      return rbase::get();
    }
  }
};
} // namespace detail

template <typename Left, typename Right, typename CompareLeft = std::less<Left>,
          typename CompareRight = std::less<Right>>
struct bimap
    : private detail::splay_node_base,
      private detail::splay_tree<Left, CompareLeft, detail::left_tag>,
      private detail::splay_tree<Right, CompareRight, detail::right_tag> {
public:
  using left_t = Left;
  using right_t = Right;

  using node_t = detail::bimap_node<left_t, right_t>;

  struct left_iterator;
  struct right_iterator;

private:
  /*******************************************************************************
   *                           Aliases & iterator base *
   *******************************************************************************/

  template <typename Tag>
  using type =
      typename std::conditional_t<std::is_same_v<detail::left_tag, Tag>, left_t,
                                  right_t>;

  template <typename Tag>
  using iterator =
      typename std::conditional_t<std::is_same_v<detail::left_tag, Tag>,
                                  left_iterator, right_iterator>;

  template <typename Tag>
  using comparator_t = std::conditional_t<std::is_same_v<detail::left_tag, Tag>,
                                          CompareLeft, CompareRight>;

  template <typename Tag>
  using tree = detail::splay_tree<type<Tag>, comparator_t<Tag>, Tag>;

  template <typename IterType, typename Tag>
  class iterator_base {

  public:
    type<Tag> const& operator*() const {
      return node_t::template from_splay_node_base<Tag>(node_base)
          ->template get_tagged<Tag, type<Tag>>();
    }

    type<Tag> const* operator->() const {
      return &node_t::template from_splay_node_base<Tag>(node_base)
                  ->template get_tagged<Tag, type<Tag>>();
    }

    IterType& operator++() {
      node_base->splay();
      if (is_fake(node_base->r)) {
        node_base = node_base->r;
      } else if (node_base->r) {
        auto* fake = node_base->rightmost()->r;
        node_base = node_base->r->leftmost();
        node_base->splay();
        static_cast<bimap*>(fake)->template root<Tag>() = node_base;
      }
      return static_cast<IterType&>(*this);
    }

    IterType operator++(int) {
      IterType tmp{node_base};
      ++(*this);
      return tmp;
    }

    IterType& operator--() {
      if (is_end()) {
        if constexpr (detail::is_left_tag<Tag>) {
          node_base = node_base->l->rightmost();
        } else {
          node_base = node_base->r->rightmost();
        }
        assert(!is_end());
      } else {
        node_base->splay();
        auto* fake = node_base->rightmost()->r;
        node_base = node_base->l->rightmost();
        node_base->splay();
        static_cast<bimap*>(fake)->template root<Tag>() = node_base;
      }
      return static_cast<IterType&>(*this);
    }

    IterType operator--(int) {
      IterType tmp{node_base};
      --(*this);
      return tmp;
    }

    iterator<typename Tag::flip> flip() const {
      if (is_end()) {
        return iterator<typename Tag::flip>{node_base};
      }
      node_t* binode{static_cast<node_t*>(
          static_cast<detail::splay_node<type<Tag>, Tag>*>(node_base))};
      return iterator<typename Tag::flip>{static_cast<detail::splay_node_base*>(
          static_cast<detail::splay_node<type<typename Tag::flip>,
                                         typename Tag::flip>*>(binode))};
    }

    friend bool operator==(const iterator_base& a, const iterator_base& b) {
      return a.node_base == b.node_base;
    }

    friend bool operator!=(const iterator_base& a, const iterator_base& b) {
      return a.node_base != b.node_base;
    }

  public:
    iterator_base(iterator_base const& other) : node_base{other.node_base} {}
    explicit iterator_base(detail::splay_node_base* node) : node_base{node} {}

    bool is_end() const {
      return node_base == node_base->p; // fake node marker
    }

  private:
    friend bimap;
    detail::splay_node_base* node_base{nullptr};
  };

public:
  struct right_iterator
      : public iterator_base<right_iterator, detail::right_tag> {
    using base = iterator_base<right_iterator, detail::right_tag>;
    using base::base;
  };

  struct left_iterator : public iterator_base<left_iterator, detail::left_tag> {
    using base = iterator_base<left_iterator, detail::left_tag>;
    using base::base;
  };

public:
  /*******************************************************************************
   *                              Public interface *
   *******************************************************************************/

  // Создает bimap не содержащий ни одной пары.
  bimap(CompareLeft compare_left = CompareLeft(),
        CompareRight compare_right = CompareRight())
      : tree<detail::left_tag>(std::move(compare_left)), tree<detail::right_tag>(std::move(compare_right)) {
    p = this; // initialize fake node
  }

  // Конструкторы от других и присваивания
  bimap(bimap const& other) : tree<detail::left_tag>{}, tree<detail::right_tag>{} {
    p = this; // initialize fake node
    if (other.empty()) {
      return;
    }
    for (left_iterator it = other.begin_left(); it != other.end_left(); it++) {
      insert(*it, *(it.flip()));
    }
  }

  bimap(bimap&& other) noexcept
      : tree<detail::left_tag>{std::move(
            *other.template splay_tree<detail::left_tag>())},
        tree<detail::right_tag>{
            std::move(*other.template splay_tree<detail::right_tag>())},
        _size{other._size} {
    p = this; // initialize fake node
    l = other.l;
    r = other.r;
    other.l = other.r = nullptr;
    other._size = 0;
    inject_fake_node();
  }

  bimap& operator=(bimap const& other) {
    if (this == &other) {
      return *this;
    }
    bimap tmp{other};
    swap(tmp); // injection done
    return *this;
  }

  bimap& operator=(bimap&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    swap(other); // injection done
    other.clear();
    return *this;
  }

  // Деструктор. Вызывается при удалении объектов bimap.
  // Инвалидирует все итераторы ссылающиеся на элементы этого bimap
  // (включая итераторы ссылающиеся на элементы следующие за последними).
  ~bimap() {
    clear();
    assert(l == nullptr);
    assert(r == nullptr);
    assert(_size == 0);
  }

  // Вставка пары (left, right), возвращает итератор на left.
  // Если такой left или такой right уже присутствуют в bimap, вставка не
  // производится и возвращается end_left().
  left_iterator insert(left_t const& left, right_t const& right) {
    return universal_insert(left, right);
  }
  left_iterator insert(left_t const& left, right_t&& right) {
    return universal_insert(left, std::move(right));
  }
  left_iterator insert(left_t&& left, right_t const& right) {
    return universal_insert(std::move(left), right);
  }
  left_iterator insert(left_t&& left, right_t&& right) {
    return universal_insert(std::move(left), std::move(right));
  }

  // Удаляет элемент и соответствующий ему парный.
  // erase невалидного итератора неопределен.
  // erase(end_left()) и erase(end_right()) неопределены.
  // Пусть it ссылается на некоторый элемент e.
  // erase инвалидирует все итераторы ссылающиеся на e и на элемент парный к e.
  left_iterator erase_left(left_iterator it) {
    return erase_it<detail::left_tag>(it);
  }

  // Аналогично erase, но по ключу, удаляет элемент если он присутствует, иначе
  // не делает ничего Возвращает была ли пара удалена
  bool erase_left(left_t const& left) {
    return erase_key<detail::left_tag>(left);
  }

  right_iterator erase_right(right_iterator it) {
    return erase_it<detail::right_tag>(it);
  }

  bool erase_right(right_t const& right) {
    return erase_key<detail::right_tag>(right);
  }

  // erase от ренжа, удаляет [first, last), возвращает итератор на последний
  // элемент за удаленной последовательностью
  left_iterator erase_left(left_iterator first, left_iterator last) {
    return erase_range<detail::left_tag>(first, last);
  }
  right_iterator erase_right(right_iterator first, right_iterator last) {
    return erase_range<detail::right_tag>(first, last);
  }

  // Возвращает итератор по элементу. Если не найден - соответствующий end()
  left_iterator find_left(left_t const& left) const {
    return find<detail::left_tag>(left);
  }
  right_iterator find_right(right_t const& right) const {
    return find<detail::right_tag>(right);
  }

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует -- бросает std::out_of_range
  right_t const& at_left(left_t const& key) const {
    return at<detail::left_tag>(key);
  }
  left_t const& at_right(right_t const& key) const {
    return at<detail::right_tag>(key);
  }

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует, добавляет его в bimap и на противоположную
  // сторону кладет дефолтный элемент, ссылку на который и возвращает
  // Если дефолтный элемент уже лежит в противоположной паре - должен поменять
  // соответствующий ему элемент на запрашиваемый (смотри тесты)
  template <typename LR = right_t,
            typename = std::enable_if_t<std::is_default_constructible_v<LR>>>
  right_t const& at_left_or_default(left_t const& key) {
    return at_or_default<detail::left_tag>(key);
  }
  template <typename LR = left_t,
            typename = std::enable_if_t<std::is_default_constructible_v<LR>>>
  left_t const& at_right_or_default(right_t const& key) {
    return at_or_default<detail::right_tag>(key);
  }

  // lower и upper bound'ы по каждой стороне
  // Возвращают итераторы на соответствующие элементы
  // Смотри std::lower_bound, std::upper_bound.
  left_iterator lower_bound_left(const left_t& left) const {
    return universal_bound<detail::left_tag, true>(left);
  }
  left_iterator upper_bound_left(const left_t& left) const {
    return universal_bound<detail::left_tag, false>(left);
  }

  right_iterator lower_bound_right(const right_t& right) const {
    return universal_bound<detail::right_tag, true>(right);
  }
  right_iterator upper_bound_right(const right_t& right) const {
    return universal_bound<detail::right_tag, false>(right);
  }

  // Возващает итератор на минимальный по порядку left.
  left_iterator begin_left() const {
    return begin<detail::left_tag>();
  }
  // Возващает итератор на следующий за последним по порядку left.
  left_iterator end_left() const {
    return end<detail::left_tag>();
  }

  // Возващает итератор на минимальный по порядку right.
  right_iterator begin_right() const {
    return begin<detail::right_tag>();
  }
  // Возващает итератор на следующий за последним по порядку right.
  right_iterator end_right() const {
    return end<detail::right_tag>();
  }

  // Проверка на пустоту
  bool empty() const {
    return _size == 0;
  }

  // Возвращает размер бимапы (кол-во пар)
  std::size_t size() const {
    return _size;
  }

  // операторы сравнения
  friend bool operator==(bimap const& a, bimap const& b) {
    // O(nlogn) time & O(1) space
    if (a.size() != b.size()) {
      return false;
    }
    if (a.empty() && b.empty()) {
      return true;
    }
    left_iterator a_it{a.begin_left()};
    left_iterator b_it{b.begin_left()};
    while (!a_it.is_end() && !b_it.is_end()) {
      auto f_a_it = a_it.flip();
      auto f_b_it = b_it.flip();
      if (!a.equal<detail::left_tag>(*a_it, *b_it) ||
          !a.equal<detail::right_tag>(*f_a_it, *f_b_it)) {
        return false;
      }
      ++a_it;
      ++b_it;
    }
    assert(a_it.is_end() == b_it.is_end());
    return true;
  }

  friend bool operator!=(bimap const& a, bimap const& b) {
    return !(a == b);
  }

  void swap(bimap& other) {
    using std::swap;
    swap(_size, other._size);
    splay_tree<detail::left_tag>()->swap(
        *other.template splay_tree<detail::left_tag>());
    splay_tree<detail::right_tag>()->swap(
        *other.template splay_tree<detail::right_tag>());
    swap(l, other.l);
    swap(r, other.r);
    inject_fake_node();
    other.inject_fake_node();
  }

  void clear() {
    if (!empty()) {
      // O(nlogn) time & O(1) space
      erase_left(begin_left(), end_left());
      assert(empty());
    }
  }

private:
  /*******************************************************************************
   *                 Templated analogues for interface functions *
   *******************************************************************************/

  template <typename L, typename R>
  left_iterator universal_insert(L&& left, R&& right) {
    if (!find_left(left).is_end() || !find_right(right).is_end()) {
      // 'left' or 'right' is already present in tree
      return end_left();
    }
    auto* node = new node_t{std::forward<L>(left), std::forward<R>(right)};
    auto* l_node =
        static_cast<detail::splay_node<left_t, detail::left_tag>*>(node);
    auto* r_node =
        static_cast<detail::splay_node<right_t, detail::right_tag>*>(node);
    splay_tree<detail::left_tag>()->insert(l, l_node);
    splay_tree<detail::right_tag>()->insert(r, r_node);
    _size++;
    if (_size == 1) {
      inject_fake_node();
    }
    validate_state();
    return left_iterator{l_node};
  }

  template <typename Tag>
  iterator<Tag> erase_it(iterator<Tag> it) {
    iterator<typename Tag::flip> it_flip{it.flip()};
    iterator<Tag> res{it};
    ++res;
    node_t* bi_node{node_t::template from_splay_node_base<Tag>(it.node_base)};
    splay_tree<Tag>()->erase(root<Tag>(), it.node_base);
    splay_tree<typename Tag::flip>()->erase(root<typename Tag::flip>(),
                                            it_flip.node_base);
    delete bi_node;
    _size--;
    validate_state();
    return res;
  }

  template <typename Tag>
  bool erase_key(type<Tag> const& key) {
    auto it{find<Tag>(key)};
    if (it.is_end()) {
      return false;
    }
    erase_it<Tag>(it);
    return true;
  }

  template <typename Tag>
  iterator<Tag> erase_range(iterator<Tag> first, iterator<Tag> last) {
    while (first != last) {
      first = erase_it<Tag>(first);
    }
    return last;
  }

  template <typename Tag, bool LowerBound>
  iterator<Tag> universal_bound(type<Tag> const& key) const {
    detail::splay_node_base* res =
        splay_tree<Tag>()->template universal_bound<LowerBound>(
            const_cast<bimap*>(this)->root<Tag>(), key);
    validate_state();
    if (res) {
      return iterator<Tag>{res};
    } else {
      return end<Tag>();
    }
  }

  template <typename Tag>
  iterator<Tag> find(type<Tag> const& key) const {
    detail::splay_node_base* node =
        splay_tree<Tag>()->find(const_cast<bimap*>(this)->root<Tag>(), key);
    if (node) {
      return iterator<Tag>{node};
    } else {
      return end<Tag>();
    }
  }

  template <typename Tag>
  type<typename Tag::flip> const& at(type<Tag> const& key) const {
    auto it{find<Tag>(key)};
    if (it.is_end()) {
      throw std::out_of_range{"No element with given key"};
    }
    return *(it.flip());
  }

  template <typename Tag>
  type<typename Tag::flip> const& at_or_default(type<Tag> const& key) {
    auto it{find<Tag>(key)};
    if (it.is_end()) {
      auto other_it{find<typename Tag::flip>(type<typename Tag::flip>{})};
      if (!other_it.is_end()) {
        erase_it<typename Tag::flip>(other_it);
      }
      if constexpr (std::is_same_v<detail::left_tag, Tag>) {
        return *(insert(key, type<typename Tag::flip>{}).flip());
      } else {
        return *(insert(type<typename Tag::flip>{}, key));
      }
    } else {
      return *(it.flip());
    }
  }

  template <typename Tag>
  iterator<Tag> begin() const {
    bimap* map{const_cast<bimap*>(this)};
    auto* node_base = map->root<Tag>()->leftmost();
    node_base->splay();
    map->root<Tag>() = node_base;
    return iterator<Tag>{node_base};
  }

  template <typename Tag>
  iterator<Tag> end() const {
    return iterator<Tag>{
        static_cast<detail::splay_node_base*>(const_cast<bimap*>(this))};
  }

private:
  /*******************************************************************************
   *                              Helper functions *
   *******************************************************************************/

  template <typename Tag>
  bool cmp(const type<Tag>& x, const type<Tag>& y) const {
    return splay_tree<Tag>()->cmp(x, y);
  }

  template <typename Tag>
  bool equal(const type<Tag>& x, const type<Tag>& y) const {
    return !(cmp<Tag>(x, y) || cmp<Tag>(y, x));
  }

  template <typename Tag>
  tree<Tag> const* splay_tree() const {
    return static_cast<tree<Tag> const*>(this);
  }

  template <typename Tag>
  tree<Tag>* splay_tree() {
    return static_cast<tree<Tag>*>(this);
  }

  template <typename Tag>
  detail::splay_node_base*& root() {
    // accessor
    if constexpr (std::is_same_v<Tag, detail::left_tag>) {
      return l;
    } else {
      return r;
    }
  }

  template <typename Tag>
  detail::splay_node_base const* root() const {
    // const accessor
    if constexpr (std::is_same_v<Tag, detail::left_tag>) {
      return l;
    } else {
      return r;
    }
  }

  void inject_fake_node() {
    if (!empty()) {
      // O(height), no splaying
      splay_tree<detail::left_tag>()->inject_fake_node(l, fake_node());
      splay_tree<detail::right_tag>()->inject_fake_node(r, fake_node());
    }
  }

  detail::splay_node_base* fake_node() {
    return static_cast<detail::splay_node_base*>(this);
  }

  void validate_state() const {
#ifdef STRESS
    if (!empty()) {
      assert(!l->p);
      assert(!r->p);
      assert(detail::one_fake(l));
      assert(detail::one_fake(r));
      assert(detail::valid_fake(l));
      assert(detail::valid_fake(r));
    }
#endif
  }

private:
  size_t _size{0};
};
