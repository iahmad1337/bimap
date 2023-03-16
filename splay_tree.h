#pragma once

#include <cassert>
#include <utility>

#include "member_switches.h"

namespace detail {

// One base for all splay_node instantiations
struct splay_node_base {
  splay_node_base* l{nullptr}; // left child
  splay_node_base* r{nullptr}; // right child
  splay_node_base* p{nullptr}; // parent

  void rotate_left();
  void rotate_right();
  splay_node_base* splay();
  splay_node_base const* leftmost() const;
  splay_node_base const* rightmost() const;
  splay_node_base* leftmost();
  splay_node_base* rightmost();
  static splay_node_base* merge(splay_node_base* lhs, splay_node_base* rhs);
  splay_node_base* erase();
};

// Every splay tree has fake node as a right son of its rightmost "true" node
// The fake node criteria: `&fake == fake.p`
// `fake.l` and `fake.r` can store arbitrary information (for example a link
// to a tree root or a link to the rightmost true node of a tree)
inline bool is_fake(splay_node_base const* node) {
  return node && node == node->p; // here we rely on laziness
}

template <typename T, typename Tag>
struct splay_node : splay_node_base {
public:
  template <typename ForwardT>
  explicit splay_node(ForwardT&& _val) : val{std::forward<ForwardT>(_val)} {}

  splay_node() = delete;
  splay_node(splay_node const&) = delete;
  splay_node(splay_node&&) = delete;
  splay_node& operator=(splay_node const&) = delete;
  splay_node& operator=(splay_node&&) = delete;
  ~splay_node() = default;

  static splay_node* from_splay_node_base(splay_node_base* node) {
    return static_cast<splay_node*>(node);
  }

  // useless? No, if we need to obtain a pointer
  splay_node_base* to_splay_node_base() {
    return static_cast<splay_node_base*>(this);
  }

  const T& get() const {
    return val;
  }

  T& get() {
    return val;
  }

private:
  T val;
};

template <typename T, typename Comparator, typename Tag>
struct splay_tree : Comparator {
public:

  splay_tree() = default;

  template<typename = void>
  splay_tree(const Comparator& other) : Comparator{other} {}

  template<typename = void>
  splay_tree(Comparator&& other) : Comparator{std::move(other)} {}

  splay_tree(splay_tree&&) = default;
  splay_tree& operator=(const splay_tree&) = default;
  splay_tree& operator=(splay_tree&&) = default;

  template <bool LowerBound = true>
  splay_node_base* universal_bound(splay_node_base*& root, const T& val) const {
    splay_node_base* cur{root};
    splay_node_base* ans{nullptr};

    splay_node_base* new_root{root};
    while (cur != nullptr) {
      new_root = cur;
      bool leftCondition{false};
      if constexpr (LowerBound) {
        leftCondition = !cmp(get_val(cur), val);
      } else {
        leftCondition = cmp(val, get_val(cur));
      }
      if (leftCondition) {
        ans = cur;
        cur = cur->l;
      } else {
        cur = cur->r;
        if (is_fake(cur)) {
          cur = nullptr;
        }
      }
    }
    if (new_root) {
      // these operations modify the tree!
      // We could omit this if-clause, but then the operation amortized cost
      // wouldn't be O(log(n)). For same reasons we do splays in iterators'
      // operator++ and operator--
      new_root->splay();
      root = new_root;
    }
    // ans may be nullptr!
    return ans;
  }

  splay_node_base* lower_bound(splay_node_base*& root, const T& val) const {
    return universal_bound<true>(root, val);
  }

  splay_node_base* find(splay_node_base*& root, const T& key) const {
    splay_node_base* it{lower_bound(root, key)};
    if (it && equal(key, get_val(it))) {
      return it;
    }
    return nullptr;
  }
  void erase(splay_node_base*& root, splay_node_base* node) {
    node->splay();
    root = node->erase();
    assert(!root || !root->p);
  }

  splay_node_base* insert(splay_node_base*& root,
                          splay_node<T, Tag>* val_node) {
    // Call this method only if the node's value is not present in the tree
    assert(!root || !root->p);
    splay_node_base* node{val_node};
    splay_node_base* parent{descend_to_parent(root, val_node->get())};
    if (root) {
      // if we're not empty
      hang_child(parent, node);
    }

    root = node;
    root->splay();
    return node;
  }

  void swap(splay_tree& other) {
    using std::swap;
    swap(static_cast<Comparator&>(*this), static_cast<Comparator&>(other));
  }

  bool empty(splay_node_base*& root) const {
    return root == nullptr;
  }

  void inject_fake_node(splay_node_base*& root, splay_node_base* fake) {
    assert(root);
    root->rightmost()->r = fake;
  }

  bool cmp(const T& x, const T& y) const {
    return Comparator::operator()(x, y);
  }

  bool equal(const T& x, const T& y) const {
    return !(cmp(x, y) || cmp(y, x));
  }

private:
  /*******************************************************************************
   *                              Helper functions *
   *******************************************************************************/

  splay_node_base* descend_to_parent(splay_node_base*& root, T const& key) {
    // Return a node that may serve as a parent to key.
    // Return nullptr if the key already is in the tree
    splay_node_base* cur{root};
    splay_node_base* parent{nullptr};

    while (cur != nullptr) {
      parent = cur;
      if (cmp(key, get_val(cur))) {
        cur = cur->l;
      } else {
        if (!cmp(get_val(cur), key)) {
          return nullptr;
        }
        cur = cur->r;
        if (is_fake(cur)) {
          cur = nullptr;
        }
      }
    }

    return parent;
  }

  void hang_child(splay_node_base* parent, splay_node_base* child) {
    assert(parent && child);
    if (cmp(get_val(parent), get_val(child))) {
      assert(!parent->r || is_fake(parent->r));
      if (is_fake(parent->r)) {
        assert(!child->r);
        child->r = parent->r;
      }
      parent->r = child;
    } else {
      assert(!parent->l);
      parent->l = child;
    }
    child->p = parent;
  }

  static T const& get_val(splay_node_base* node) {
    return splay_node<T, Tag>::from_splay_node_base(node)->get();
  }
};
//#define STRESS
#ifdef STRESS
inline int count_fakes(splay_node_base const* node) {
  if (!node) {
    return 0;
  } else if (node == node->p) {
    return 1;
  } else {
    return count_fakes(node->l) + count_fakes(node->r);
  }
}

inline bool valid_fake(splay_node_base const* node) {
  return is_fake(node->rightmost()->r);
}

inline bool one_fake(splay_node_base const* node) {
  return count_fakes(node) == 1;
}
#endif

} // namespace detail
