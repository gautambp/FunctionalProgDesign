package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k1 <- if (new java.util.Random().nextBoolean()) insert(new java.util.Random().nextInt(), empty) else empty
    k2 <- insert(new java.util.Random().nextInt(), k1)
    k3 <- insert(new java.util.Random().nextInt(), k2)
    k4 <- insert(new java.util.Random().nextInt(), k3)
    m <- oneOf[H](k1, k2, k3, k4, genHeap)
  } yield m

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) Int.MinValue else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (h: H) =>
    val a = new java.util.Random().nextInt()
    findMin(insert(a, empty)) == a
  }

  property("findMin Check") = forAll { (h: H) =>
    val m = new java.util.Random().nextInt()
    findMin(insert(m, insert(m+1, empty))) == m
  }

  property("empty Check") = forAll { (h: H) =>
    deleteMin(insert(Int.MinValue, h)) == h  
  }
  
  property("sorted seq Check") = forAll { (h: H) =>
    def isSorted(minVal : Int, h: H) : Boolean = {
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        if (ord.gt(minVal, m)) false
        else isSorted(m, deleteMin(h))
      }
    }
    isSorted(Int.MinValue, h)
  }

  property("multiple min check") = forAll { (h: H) =>
    val m = new java.util.Random().nextInt()
    val src = insert(m, insert(m-1, insert(m, empty)))
    findMin(deleteMin(src)) == m
  }

  property("melding Check") = forAll { (h: H) =>
    var src1 = h
    if (isEmpty(h)) src1 = insert(new java.util.Random().nextInt(), h)
    val src2 = insert(new java.util.Random().nextInt(), insert(new java.util.Random().nextInt(), empty))
    val m1 = findMin(src1)
    val m2 = findMin(src2)
    val m3 = findMin(meld(src1, src2))
    (ord.lteq(m3, m1) && ord.lteq(m3, m2))
  }
}
