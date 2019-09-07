package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = x => {
      if (x == elem)
        true
      else
        false
    }
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = x => {
      if (contains(s,x) || contains(t,x))
        true
      else
        false
    }
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = x => {
      if (contains(s,x) && contains(t,x))
        true
      else
        false
    }
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = x => {
      if (contains(s,x) &&  !contains(t,x))
        true
      else
        false
    }
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = x => {
      if (p(x))
        true
      else
        false
    }
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {

      def forallIsTrueForEverythingFromAToThousand(a:Int, s: Set, p: Int => Boolean): Boolean = {
        if (a > 1000)
          true
        else {
          if (s(a) && !p(a)) {
            false
          }
          else
            forallIsTrueForEverythingFromAToThousand(a+1, s, p)
        }



      }
      forallIsTrueForEverythingFromAToThousand(-1000,s,p)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = {
      def existsIsTrueForEverythingFromAToThousand(a:Int, s: Set, p: Int => Boolean): Boolean = {
        if (a > 1000)
          false
        else {
          if (s(a) && p(a)) {
            true
          }
          else
            existsIsTrueForEverythingFromAToThousand(a+1, s, p)
        }



      }
      existsIsTrueForEverythingFromAToThousand(-1000,s,p)
    }
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = x => {
//      Does there exist an item y in s such that f(y) is x?
      if (exists(s, y => {
        if (f(y) == x)
          true
        else
          false
      }))
        true
      else
        false
    }
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
