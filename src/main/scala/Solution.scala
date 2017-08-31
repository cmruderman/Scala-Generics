import hw.generics._

sealed trait BinTree [A] extends hw.generics.ListLike[A, BinTree[A]] //head is value on left most node, tail is tree with left most value removed
case class Node[A](lhs : BinTree [A], value : A, rhs : BinTree [A]) extends BinTree[A]{
		def cons(head: A): BinTree[A] = Node(Leaf(), head, this)
		def head(): Option[A] = 
			lhs match{
				case Leaf() => Some(value)
				case Node(left, valu, right) => (left, valu, right) match{
							case (Node(l, v, r), _, _) => left.head()
							case (Leaf(), valu, _) => Some(valu)
					}
			}
		
		def tail(): Option[BinTree[A]] = { //pattern match on lhs
			lhs.isEmpty() match {
				case false => 
					if(lhs.tail()!=None) Some(Node(lhs.tail().get, value, rhs)) 
					else Some(Node(Leaf(), value, rhs))		//if we're not at the bottom of the tree 
				case true => 
					if(rhs.isEmpty()) Some(Leaf())
					else Some(rhs)
			}	
		}
		def isEmpty(): Boolean = false
}
case class Leaf[A]() extends BinTree[A]{
			def cons(head: A): BinTree[A] = Node(Leaf(), head, Leaf())
			def head(): Option[A] = None
			def tail(): Option[BinTree[A]] = None
			def isEmpty(): Boolean = true
}

object ListFunctions{

trait Ordered[A]{
	def compare(other:A): Ordering
}
case class IntLike(arg: Int) extends Ordered[IntLike]{
	def compare(argument: IntLike): Ordering = {
		if(this.arg == argument.arg) EQ
		else if(this.arg > argument.arg) GT
		else LT
	}
}


def filter[E,C <: hw.generics.ListLike[E, C]](f: E  => Boolean, alist: C): C =  //filter consumes and produces lists of A, C where A, c is subtype of ListLike
			if(alist.isEmpty()) alist
			else if(f(alist.head().get) && alist.tail()==None) alist //if it only has a head
			else if(f(alist.head().get) && alist.tail()!=None){ 
				filter(f, alist.tail().get).cons(alist.head().get)
			}
			else{filter(f, alist.tail().get)}

def append[A,C <: hw.generics.ListLike[A, C]](alist: C, blist: C): C = //////    ahead
	(alist.isEmpty(), blist.isEmpty()) match{						   //////        atail
		case(true, true) => alist							  		   //////        	bhead
		case(false, true) => alist										//////         		btail
		case(true, false) => blist
		case(false, false) => {
			if(alist.tail()==None)  blist.tail().get.cons(blist.head().get).cons(alist.head().get)
			else if(blist.tail()==None) append[A,C](blist.cons(alist.head().get), alist.tail().get)
			else append[A,C](alist.tail().get, blist).cons(alist.head().get)
		}
	}
	def sort [A <: Ordered [A], C <: hw.generics.ListLike[A, C]](alist : C):C = {
		if(alist.isEmpty()) alist
		else insert(alist.head().get, sort[A,C](alist.tail().get))}

	def insert[A <: Ordered [A], C <: hw.generics.ListLike[A, C]](x: A, xs: C): C =
		(xs.head(), xs.tail()) match{
			case (None, None) => xs.cons(x)
			case (Some(head), Some(tail)) => head.compare(x) match{
				case LT => insert[A,C](x, tail).cons(head)
				case _ => xs.cons(x)
			}
			case _ => ???
		}
}

	class C1 extends T2[Int, Int, String, String] with T3[Int, Int, Int, String, String, String, Int]{
	// Do not change the class body . Simply extend T1 , T2 , and/or T3.
		def f(a: Int , b: Int): Int = 0
		def g(c: String): String = ""
		def h(d: String): Int = 0
	}
	class C2 extends T1[Int, Int] with T2[Int, Int, Int, Int] with T3[Int, Int, Int, Int, Int, Int, Int] {
		// Do not change the class body . Simply extend T1 , T2 , and/or T3.
		def f(a: Int, b: Int ): Int = 0
		def g(c: Int): Int = 0
		def h(d: Int): Int = 0
	}
	class C3 [A](x : A) extends T3[Int, A, Int, A, String, String, A] {
		// Do not change the class body . Simply extend T1 , T2 , and/or T3.
		def f(a: Int, b: A): Int = 0
		def g(c: A): String = ""
		def h(d: String): A = x
	}
	class C4 [A](x : Int, y: C4[A]) extends T1[Int, C4[A]] with T3[Int, C4[A], C4[A], Int, C4[A], C4[A], Int]{
		// Do not change the class body . Simply extend T1 , T2 , and/or T3.
		def f(a: Int, b: C4[A]): C4[A] = b
		def g(c: Int): C4[A] = y
		def h(d: C4[A]): Int = x
	}