import hw.generics._

import ListFunctions._

class Tests extends org.scalatest.FunSuite {

val list1 = Node(Node(Node(Leaf(), 6, Leaf()), 3, Node(Leaf(), 7, Leaf())), 10, Node(Node(Leaf(), 12, Leaf()), 4, Node(Leaf(), 6, Leaf())))
val list2 = Node(Node(Leaf(), 3, Leaf()), 2, Leaf())
val list3: BinTree[Int] = Leaf()
val list4 = Node(list1, 10, list2)
val list5 = Node(list2, 16, list2)
val list6 = Node(Node(Leaf(), 3, Leaf()), 5, Leaf())
val list7 = Node(Node(Leaf(), 4, Node(Leaf(), 2, Leaf())), 5, Node(Leaf(), 3, Leaf()))
val list8 = Node(list3, 4, list3)
val list9 = Node(list3, 5, list3)
val list10 = Node(list3, 6, list3)
val list11 = Node(list3, 7, list3)
val list12 = Node(list8, 2, list9) 
val list13 = Node(list10, 3, list11)
val largeList = Node(list12, 1, list13)
val intLike1 = Node(Leaf(), IntLike(3), Leaf())
val intLike2 = Node(Node(Leaf(), IntLike(3), Leaf()), IntLike(4), Leaf())
val intLike3 = Node(intLike1, IntLike(10), intLike2)
val intLike4 = Node(Node(Leaf(), IntLike(3), Leaf()), IntLike(5), Leaf())
val intLike5 = Node(Node(Leaf(), IntLike(4), Node(Leaf(), IntLike(2), Leaf())), IntLike(5), Node(Leaf(), IntLike(3), Leaf()))
val intLike6 = Node(intLike4, IntLike(1), intLike5)
val intLike7 = Node(Node(Node(Leaf(), IntLike(6), Leaf()), IntLike(3), Node(Leaf(), IntLike(7), Leaf())), IntLike(10), Node(Node(Leaf(), IntLike(12), Leaf()), IntLike(4), Node(Leaf(), IntLike(6), Leaf())))
val intLike8 = Node(Node(Leaf(), IntLike(3), Leaf()), IntLike(2), Leaf())
val intLike9 = Node(intLike7, IntLike(1), intLike8)
val intLike10 = Node(intLike9, IntLike(2), intLike6)

//--------------------------------------
//			Node tests

test("Head is Properly Defined") { 
	assert(list1.head() == Some(6))
	assert(list2.head() == Some(3))
	assert(list3.head() == None)
	assert(list4.head() == Some(6))
	assert(list5.head() == Some(3))
	assert(list8.head == Some(4))
	}
test("Cons is Properly Defined") { 
	assert(list1.cons(25) == Node(Leaf(), 25, Node(Node(Node(Leaf(), 6, Leaf()), 3, Node(Leaf(), 7, Leaf())), 10, Node(Node(Leaf(), 12, Leaf()), 4, Node(Leaf(), 6, Leaf())))))
	assert(list2.cons(25) == Node(Leaf(), 25, Node(Node(Leaf(), 3, Leaf()), 2, Leaf())))
	assert(list3.cons(1) == Node(Leaf(), 1, Leaf()))
	assert(list8.cons(1) == Node(Leaf(), 1, list8))
	assert(list8.cons(1).tail == Some(list8))
	assert(largeList.cons(1).tail == Some(largeList))
	assert(largeList.cons(0).head == Some(0))
}
test("Tail is Properly Defined") { 
	assert(list1.tail() == Some(Node(Node(Leaf(), 3, Node(Leaf(), 7, Leaf())), 10, Node(Node(Leaf(), 12, Leaf()), 4, Node(Leaf(), 6, Leaf())))))
	assert(list2.tail() == Some(Node(Leaf(), 2, Leaf())))
	assert(list3.tail() == None)
	assert(list4.tail() == Some(Node(Node(Node(Leaf(), 3, Node(Leaf(), 7, Leaf())), 10, Node(Node(Leaf(), 12, Leaf()), 4, Node(Leaf(), 6, Leaf()))), 10, Node(Node(Leaf(), 3, Leaf()), 2, Leaf()))))
	assert(list5.tail() == Some(Node(Node(Leaf(), 2, Leaf()), 16, Node(Node(Leaf(), 3, Leaf()), 2, Leaf()))))
	assert(list6.tail() == Some(Node(Leaf(), 5, Leaf())))
	assert(list7.tail() == Some(Node(Node(Leaf(), 2, Leaf()), 5, Node(Leaf(), 3, Leaf()))))
	assert(list8.tail == Some(Leaf()))
	assert(list12.tail == Some(Node(Leaf(), 2, list9)))
	assert(largeList.tail == Some(Node(Node(list3, 2, list9), 1, list13)))
}
test("isEmpty is Properly Defined") { 
	assert(list1.isEmpty() == false)
	assert(list2.isEmpty() == false)
	assert(list3.isEmpty() == true)
	assert(list4.isEmpty() == false)
	assert(list5.isEmpty() == false)
}

//--------------------------------------
//			Filter Test
def isEven[A](a: A): Boolean = a.isInstanceOf[Int] match {
	case true => (a.asInstanceOf[Int]%2)==0
	case false => false
} 

test("Filter Test") { 
	assert(filter[Int, BinTree[Int]](isEven, list1)==Node(Leaf(),6,Node(Leaf(),10,Node(Leaf(),12,Node(Leaf(),4,Node(Leaf(),6,Leaf()))))))
	assert(filter[Int, BinTree[Int]](isEven, list2)==Node(Leaf(), 2, Leaf()))
	assert(filter[Int, BinTree[Int]](isEven, list3)==Leaf())
	assert(filter[Int, BinTree[Int]](isEven, list4)==Node(Leaf(),6,Node(Leaf(),10,Node(Leaf(),12,Node(Leaf(),4,Node(Leaf(),6,Node(Leaf(),10,Node(Leaf(),2,Leaf()))))))))
	assert(filter[Int, BinTree[Int]](isEven, list5)== Node(Leaf(),2,Node(Leaf(),16,Node(Leaf(),2,Leaf()))))
	assert(filter[Int, BinTree[Int]](isEven, list11) == list3)
	assert(filter[Int, BinTree[Int]](isEven, list3) == list3)
	assert(filter[Int, BinTree[Int]](isEven, largeList) == Node(Leaf(), 4, Node(Leaf(), 2, Node(Leaf(), 6, Leaf()))))
	assert(filter[Int, BinTree[Int]](isEven, list12) == Node(Leaf(), 4, Node(Leaf(), 2, Leaf())))
}
//--------------------------------------
//			Append Test

test("Append Test") { 
	assert(append[Int, BinTree[Int]](list1, list2)== Node(Leaf(),6,Node(Leaf(),3,Node(Leaf(),7,Node(Leaf(),10,Node(Leaf(),12,Node(Leaf(),4,Node(Leaf(),6,Node(Node(Leaf(),3,Leaf()),2,Leaf())))))))))
	assert(append[Int, BinTree[Int]](list6, list9)== Node(Leaf(),3,Node(Leaf(),5,Node(Leaf(),5,Leaf()))))
	assert(append[Int, BinTree[Int]](list5, list1)== Node(Leaf(),3,Node(Leaf(),2,Node(Leaf(),16,Node(Leaf(),3,Node(Leaf(),2,Node(Node(Node(Leaf(),6,Leaf()),3,Node(Leaf(),7,Leaf())),10,Node(Node(Leaf(),12,Leaf()),4,Node(Leaf(),6,Leaf())))))))))
	assert(append[Int, BinTree[Int]](list3, list3) == list3)
	assert(append[Int, BinTree[Int]](list8, list10) == Node(list3, 4, Node(list3, 6, list3)))
	assert(append[Int, BinTree[Int]](list3, list13) == list13)
	assert(append[Int, BinTree[Int]](list12, list3) == Node(Node(Leaf(), 4, Leaf()), 2, Node(Leaf(), 5, Leaf())))
	assert(append[Int, BinTree[Int]](list12, list13) == Node(list3, 4, Node(list3, 2, Node(list3, 5, Node(Node(list3, 6, list3), 3, Node(list3, 7, list3))))))
}
//--------------------------------------
//			Sort Test


test("Sort Test") { 
	assert(sort[IntLike, BinTree[IntLike]](intLike3)==Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(4),Node(Leaf(),IntLike(10),Leaf())))))
	assert(sort[IntLike, BinTree[IntLike]](intLike6)==Node(Leaf(),IntLike(1),Node(Leaf(),IntLike(2),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(4),Node(Leaf(),IntLike(5),Node(Leaf(),IntLike(5),Leaf()))))))))
	assert(sort[IntLike, BinTree[IntLike]](intLike9)==Node(Leaf(),IntLike(1),Node(Leaf(),IntLike(2),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(4),Node(Leaf(),IntLike(6),Node(Leaf(),IntLike(6),Node(Leaf(),IntLike(7),Node(Leaf(),IntLike(10),Node(Leaf(),IntLike(12),Leaf())))))))))))
	assert(sort[IntLike, BinTree[IntLike]](intLike10)==Node(Leaf(),IntLike(1),Node(Leaf(),IntLike(1),Node(Leaf(),IntLike(2),Node(Leaf(),IntLike(2),Node(Leaf(),IntLike(2),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(3),Node(Leaf(),IntLike(4),Node(Leaf(),IntLike(4),Node(Leaf(),IntLike(5),Node(Leaf(),IntLike(5),Node(Leaf(),IntLike(6),Node(Leaf(),IntLike(6),Node(Leaf(),IntLike(7),Node(Leaf(),IntLike(10),Node(Leaf(),IntLike(12),Leaf())))))))))))))))))))
}

}


