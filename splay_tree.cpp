#include "splay_tree.h"
#include <optional>

namespace detail {

// Zag (a.k.a. left rotation)
//  gran(?)      gran(?)
//   |             |
//  root          piv.
//  / \           / \
// *  piv. -->  root *
//    / \       / \
//   x   *     *   x
// Note that the fake node here can be only on the rightmost position (right
// son of `pivot`) and the pointer to it won't ever be accessed
void splay_node_base::rotate_left() {
  assert(r); // pivot
  auto root = this;
  auto pivot = r;
  auto x = pivot->l;
  auto grandparent = p;

  if (grandparent) {
    if (root == grandparent->l) {
      grandparent->l = pivot;
    } else {
      grandparent->r = pivot;
    }
  }
  pivot->p = grandparent;
  root->p = pivot;
  if (x) {
    x->p = root;
  }

  pivot->l = root;
  root->r = x;
}

// Zig (a.k.a. right rotation)
//    gran(?)     gran(?)
//     |           |
//    root        piv.
//    / \         / \
//  piv. *  -->  *  root
//  / \             / \
// *   x           x   *
// Note that the fake node here can be only on the rightmost position
// (root's right son) and the pointer to it won't ever be accessed
void splay_node_base::rotate_right() {
  assert(l); // pivot
  auto root = this;
  auto pivot = l;
  auto x = pivot->r;
  auto grandparent = p;

  if (grandparent) {
    if (root == grandparent->l) {
      grandparent->l = pivot;
    } else {
      grandparent->r = pivot;
    }
  }
  pivot->p = grandparent;
  root->p = pivot;
  if (x) {
    x->p = root;
  }

  pivot->r = root;
  root->l = x;
}

// No need to worry about fake node unless we invoke `splay` directly on it
splay_node_base* splay_node_base::splay() {
  while (p) {
    auto parent = p;
    auto grandparent = p->p;
    if (!grandparent) {
      if (parent->l == this) {
        // Zig
        parent->rotate_right();
        assert(r == parent);
      } else {
        // Zag
        parent->rotate_left();
        assert(l == parent);
      }
    } else if (grandparent->l == parent && parent->l == this) {
      // Zig-Zig
      grandparent->rotate_right();
      assert(parent->l == this && parent->r == grandparent);
      parent->rotate_right();
      assert(r == parent);
    } else if (grandparent->l == parent && parent->r == this) {
      // Zig-Zag
      parent->rotate_left();
      assert(l == parent && p == grandparent);
      grandparent->rotate_right();
      assert(l == parent && r == grandparent);
    } else if (grandparent->r == parent && parent->r == this) {
      // Zag-Zag
      grandparent->rotate_left();
      assert(parent->l == grandparent && parent->r == this);
      parent->rotate_left();
      assert(l == parent);
    } else if (grandparent->r == parent && parent->l == this) {
      // Zag-Zig
      parent->rotate_right();
      assert(p == grandparent && r == parent);
      grandparent->rotate_left();
      assert(l == grandparent && r == parent);
    } else {
      // invalid state
      assert(false);
    }
  }
  // return the splayed node
  return this;
}

splay_node_base const* splay_node_base::leftmost() const {
  auto res = this;
  while (res->l) {
    res = res->l;
  }
  return res;
}

splay_node_base const* splay_node_base::rightmost() const {
  assert(!is_fake(this));
  auto res = this;
  while (res->r) {
    if (is_fake(res->r)) {
      break;
    }
    res = res->r;
  }
  return res;
}

splay_node_base* splay_node_base::leftmost() {
  return const_cast<splay_node_base*>(std::as_const(*this).leftmost());
}

splay_node_base* splay_node_base::rightmost() {
  return const_cast<splay_node_base*>(std::as_const(*this).rightmost());
}

splay_node_base* splay_node_base::merge(splay_node_base* lhs, splay_node_base* rhs) {
  if (!lhs) {
    return rhs;
  }
  if (!rhs) {
    return lhs;
  }
  // both must be the roots
  assert(lhs->p == nullptr);
  assert(rhs->p == nullptr);
  auto* new_root = lhs;
  lhs = const_cast<splay_node_base*>(lhs->rightmost());
  lhs->r = rhs;
  rhs->p = lhs;
  return new_root;
}

splay_node_base* splay_node_base::erase() {
  assert(!is_fake(this));
  std::optional<splay_node_base*> fake_right;
  if (is_fake(r)) {
    fake_right.emplace(r);
    r = nullptr;
  }
  splay();
  if (l) {
    l->p = nullptr;
  }
  if (r) {
    r->p = nullptr;
  }
  auto v = merge(l, r);
  if (fake_right.has_value() && v) {
    // move fake node to its new position
    auto rightmost_v = v->rightmost();
    rightmost_v->r = fake_right.value();
  }
  // return the new root
  return v;
}

} // namespace detail
