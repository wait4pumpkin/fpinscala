import List._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def tail[A](as: List[A]): List[A] = as match {
		case Nil => Nil
		case Cons(_, t) => t
	}

	def setHead[A](as: List[A], h: A): List[A] = as match {
		case Nil => Cons(h, Nil)
		case Cons(_, t) => Cons(h, t)
	}

	def drop[A](l: List[A], n: Int): List[A] = {
		if (n <= 0) l
		else l match {
			case Nil => Nil
			case Cons(_, t) => drop(t, n -1)
		}
	}

	def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
		l match {
			case Cons(h, t) if f(h) => dropWhile(t)(f)
			case _ => l
		}
	}

	def init[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(h, Nil) => Nil
		case Cons(h, t) => Cons(h, init(t))
	}

	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
		as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}
	}

	@annotation.tailrec
	def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
		as match {
			case Nil => z
			case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
		}
	}

	def length[A](as: List[A]): Int = {
		foldRight(as, 0)((_, y) => 1 + y)
	}

	def append[A](a: List[A], b: List[A]): List[A] = {
		foldRight(a, b)(Cons(_, _))
	}

	def concat[A](as: List[List[A]]): List[A] = {
		foldRight(as, Nil: List[A])(append)
	}

	def map[A, B](as: List[A])(f: A => B): List[B] = {
		foldRight(as, Nil: List[B])((x, xs) => Cons(f(x), xs))
	}

	def filter[A](as: List[A])(f: A => Boolean): List[A] = {
		foldRight(as, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
	}

	def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
		concat(map(as)(f))
	}

	def sumWith(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumWith(xs, ys))
	}

	def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
	}

	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
		@annotation.tailrec
		def check[B](as: List[B], bs: List[B]): Boolean = (as, bs) match {
			case (_, Nil) => true
			case (Cons(x, xs), Cons(y, ys)) if x == y => check(xs, ys)
			case _ => false
		}

		sup match {
			case _ if check(sup, sub) => true
			case Cons(_, xs) if check(xs, sub) => true
			case _ => false
		}
	}
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree
{
	def size[A](as: Tree[A]): Int = as match {
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(l) + size(r)
	}

	def maximum(as: Tree[Int]): Int = as match {
		case Leaf(x) => x
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	def depth[A](as: Tree[A]): Int = as match {
		case Leaf(_) => 1
		case Branch(l, r) => (depth(l) max depth(r)) + 1
	}

	def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
		case Leaf(x) => Leaf(f(x))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	def fold[A, B](as: Tree[A])(f: A => B)(g: (B, B) => B): B = as match {
		case Leaf(x) => f(x)
		case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}
}


object Test
{
	def main(args: Array[String]): Unit = {
		val r = List(1, 2, 3, 4, 5) match {
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + sum(t)
			case _ => 101
		}
		assert(r == 3)
		assert(tail(List(1, 2, 3)) == List(2, 3))
		assert(setHead(List(1, 2, 3), 4) == List(4, 2, 3))
		assert(drop(List(1, 2, 3), 2) == List(3))
		assert(dropWhile(List(1, 2, 3))(x => x < 3) == List(3))
		assert(init(List(1, 2, 3)) == List(1, 2))

		assert(sum(List(1, 2, 3)) == 6)
		assert(sum(List(1, 2, 3)) == foldRight(List(1, 2, 3), 0.0)(_ + _))
		assert(sum(List(1, 2, 3)) == foldLeft(List(1, 2, 3), 0.0)(_ + _))

		assert(product(List(1, 2, 3, 4)) == 24)
		assert(product(List(1, 2, 3, 4)) == foldRight(List(1, 2, 3, 4), 1)(_ * _))
		assert(product(List(1, 2, 3, 4)) == foldLeft(List(1, 2, 3, 4), 1)(_ * _))

		assert(List(1, 2, 3) == foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
		assert(3 == length(List(1, 2, 3)))
		assert(3 == foldLeft(List(1, 2, 3), 0)((x, _) => x + 1))

		// 3.12
		assert(List(3, 2, 1) == foldLeft(List(1, 2, 3), Nil: List[Int])((xs, x) => Cons(x, xs)))

		// 3.14
		assert(List(1, 2, 3, 4) == append(List(1, 2), List(3, 4)))

		// 3.15
		assert(List(1, 2, 3, 4, 5, 6) == concat(List(List(1), List(2, 3), List(4, 5, 6))))

		// 3.16
		assert(List(2, 3, 4) == foldRight(List(1, 2, 3), Nil: List[Int])((x, xs) => Cons(x + 1, xs)))

		// 3.17
		assert(List("1.0", "2.0") == foldRight(List(1.0, 2.0), Nil: List[String])((x, xs) => Cons(x.toString(), xs)))

		// 3.18
		assert(List(2, 3, 4) == map(List(1, 2, 3))(_ + 1))
		assert(List("1", "2") == map(List(1, 2))(_.toString))

		// 3.19
		assert(List(2) == filter(List(1, 2, 3))(_ == 2))

		// 3.20
		assert(List(1, 1, 2, 2, 3, 3) == flatMap(List(1, 2, 3))(x => List(x, x)))

		// 3.21
		assert(List(2) == flatMap(List(1, 2, 3))(x => if (x == 2) List(x) else Nil))

		// 3.22
		assert(List(5, 7, 9) == sumWith(List(1, 2, 3), List(4, 5, 6)))

		// 3.23
		assert(List(5, 7, 9) == zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))

		// 3.24
		assert(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3)))
		assert(!hasSubsequence(List(1, 2, 3, 4, 5), List(3, 2)))


		val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

		// 3.25
		assert(Tree.size(tree) == 5)

		// 3.26
		assert(Tree.maximum(tree) == 3)

		// 3.27
		assert(Tree.depth(tree) == 3)

		// 3.28
		assert(Tree.map(tree)(_ + 1) == Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))

		// 3.29
		assert(Tree.size(tree) == Tree.fold(tree)(_ => 1)(_ + _ + 1))
		assert(Tree.maximum(tree) == Tree.fold(tree)(x => x)(_ max _))
		assert(Tree.depth(tree) == Tree.fold(tree)(_ => 1)((x, y) => (x max y) + 1))
		assert(Tree.map(tree)(_ + 1) == Tree.fold(tree)(x => Leaf(x + 1): Tree[Int])(Branch(_, _)))
	}
	// 3.13
}
