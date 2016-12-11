package objsets

import TweetReader._
import java.util.NoSuchElementException
/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  def isEmpty: Boolean

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = that.filterAcc(x => true, this)

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = {
    if (isEmpty) Nil
    else {
      val mostPopular = mostRetweeted
      val rest = remove(mostPopular)
      new Cons(mostPopular, rest.descendingByRetweet)
    }
  }

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  val isEmpty = true

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new NoSuchElementException

  override def toString = "{" + "}"

}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  val isEmpty = false

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val rest = left union right
    if( p(elem) ) rest.filterAcc(p, acc.incl(elem))
    else rest.filterAcc(p, acc)

  }
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

//  def union(that: TweetSet): TweetSet =
//    ((left union right) union that) incl elem

  def mostRetweeted: Tweet = {
    def maxi(tw1: Tweet, tw2: Tweet): Tweet = {
      if (tw1.retweets > tw2.retweets) tw1 else tw2
    }
    if (left.isEmpty && right.isEmpty) elem
    else if (right.isEmpty) maxi(left.mostRetweeted, elem)
    else if (left.isEmpty) maxi(right.mostRetweeted, elem)
    else maxi(left.mostRetweeted, maxi(left.mostRetweeted ,elem))
  }

  override def toString = "{" + left + elem + right + "}"

}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple extends App{
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def filterWithList(xs: List[String]): TweetSet = {
    val allset = TweetReader.allTweets
    allset.filter(x => xs exists ( t => x.text.contains(t) ) )
  }

  lazy val googleTweets: TweetSet = filterWithList(google)
  lazy val appleTweets: TweetSet = filterWithList(apple)

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
  }

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}

abstract class InSet {
  def contains(x: Int): Boolean
  def include(x: Int): InSet
  def union(other: InSet): InSet

}

object Empt extends InSet {
  def contains(x: Int): Boolean = false
  def include(x: Int): InSet = new NnEmpt(x, Empt, Empt)
  def union(other: InSet): InSet = other
  override def toString = "."
}

class NnEmpt(elem: Int, val left: InSet, val right: InSet) extends InSet {
  def contains(x: Int): Boolean =
    if (x>elem) right contains x
    else if (x<elem) left contains x
    else true
  def include(x: Int): InSet =
    if (x<elem)  new  NnEmpt(elem, left include x, right)
    else if (x>elem) new  NnEmpt(elem, left, right include x)
    else this
  def union(other: InSet): InSet =
    ((left union right) union other) include elem
  override def toString = "{" + left + elem + right + "}"
}

object Tree extends App {
  val t1 = Empt
  println(t1.toString)
  val t2 = new NnEmpt(3, Empt, Empt)
  println(t2.toString)
  val t3 = new NnEmpt(3, new NnEmpt(2, Empt, Empt), new NnEmpt(4, Empt, new NnEmpt(5, Empt, Empt)))
  println(t3.toString)
  println(t3.contains(2))
  println(t3.include(1))
  println(t2.include(4))
  val t0 = new NnEmpt(6, Empt, Empt)
  println(t0 union t3)
  println(t0.left)

  val set1 = new Empty
  val set2 = set1.incl(new Tweet("a", "a body", 20))
  val set3 = set2.incl(new Tweet("b", "b body", 110))
  val set4 = set3.incl(new Tweet("c", "c body", 200))

  println(set4 filter(x => x.retweets > 20))
  println(set4.mostRetweeted)
  println(set3.descendingByRetweet)

}