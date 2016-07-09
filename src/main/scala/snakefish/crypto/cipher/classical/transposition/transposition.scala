package snakefish.crypto
package cipher.classical

package object transposition {
  
  def normalizeKey(key: Array[Int], sameLetterAsNextInOrder: Boolean): Array[Int] = {
    val result = new Array[Int](key.length)
    var keySorted = key.clone.sorted
    if (!sameLetterAsNextInOrder)
      keySorted = keySorted.distinct
    
    for (i <- 0 until key.length) {
      val index = keySorted.indexOf(key(i))
      result(i) = index
      
      if (sameLetterAsNextInOrder) 
        keySorted(index) = -1
    }
    
    result
  }
  
}
