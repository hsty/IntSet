

object lastExer extends App {

  trait IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(x: IntSet): IntSet
    def intersection( x: IntSet): IntSet
    def excl(x: Int): IntSet
    def isEmpty: Boolean
  }

  class EmptySet extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
    def union(x: IntSet): IntSet = x
    def intersection (x: IntSet): IntSet = this
    def isEmpty = true
    def excl(x: Int): IntSet = this
  }

  class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {

    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmptySet(elem, left incl x, right)
      else if (x > elem) new NonEmptySet(elem, left, right incl x)
      else this

    def union(other: IntSet): IntSet = {
      val set = left.union(right.union(other))
      set.incl(elem)
    }

    def intersection(other: IntSet): IntSet = {
      val l = left.intersection(other)
      val r = right.intersection(other)
      val u = l.union(r)
      if(other.contains(elem)) u.incl(elem) else u
    }

    def isEmpty = false

    def excl(x: Int): IntSet =
      if(x < elem) new NonEmptySet(elem, left.excl(x), right)
      else if (x>elem) new NonEmptySet(elem, left, right.excl(x))
      else left.union(right)
  }

}
