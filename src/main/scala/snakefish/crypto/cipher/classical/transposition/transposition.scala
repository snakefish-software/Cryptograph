package snakefish.crypto
package cipher.classical

import scala.collection.mutable.ArrayBuffer

package object transposition {
  
  def normalizeKey(key: Array[Int], sameLetterAsNextInOrder: Boolean): Array[Int] = {
    val res = new Array[Int](key.length)
    var keySorted = key.clone.sorted
    if (!sameLetterAsNextInOrder)
      keySorted = keySorted.distinct
    
    for (i <- 0 until key.length) {
      val index = keySorted.indexOf(key(i))
      res(i) = index
      
      if (sameLetterAsNextInOrder) 
        keySorted(index) = -1
    }
    
    res
  }
  
}
