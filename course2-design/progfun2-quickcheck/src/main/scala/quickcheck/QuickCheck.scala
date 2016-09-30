package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    elem <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(elem, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("reinsert min and findMin yields min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of an empty heap with two elements a and b inserted should yield min of the two elements a and b") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (if (a < b) a else b)
  }
  
  property("inserting any element a into a heap then deleting the min of the heap yields an emty heap") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("the minimum of a melded heap should yield the minimum of one of the constituant heaps") = forAll { (h1: H, h2: H) =>
    val minMeld = findMin(meld(h1, h2))
    minMeld == findMin(h1) || minMeld == findMin(h2)
  }
  
  property("deleting the minimum elements of a heap yields a sorted list") = forAll { h: H =>
    def removeMin (ts: H, xs: List[Int]): List[Int] = {
      if (isEmpty(ts)) xs
      else findMin(ts) :: removeMin(deleteMin(ts), xs)
    }
    
    val xs = removeMin(h, Nil)
    xs == xs.sorted
  }
  
  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def removeMin(ts: H, xs: List[Int]): List[Int] = {
      if (isEmpty(ts)) xs
      else findMin(ts) :: removeMin(deleteMin(ts), xs)
    }
    
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = removeMin(meld1, Nil)
    val xs2 = removeMin(meld2, Nil)
    xs1 == xs2
  }

}
