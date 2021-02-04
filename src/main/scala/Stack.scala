/** Defines an immutable Stack.
 *
 *  @author Massimo Angelillo
 *  @constructor create a stack from a given list
 *  @param data the initial stack items
 */
final case class Stack[T](private val data: List[T] = List()) {
  /**
   * Pop the top element of the stack
   * @return A new stack with the top element removed
   */
  def pop: Stack[T] = {
    if(data.isEmpty) throw new Exception("Cannot pop on an empty stack")
    Stack(data.drop(1))
  }

  /**
   * Peak at the top element of the stack
   * @return The top element of the stack
   */
  def peak: T = {
    if(data.isEmpty) throw new Exception("Cannot peak on an empty stack")
    data.head
  }

  /**
   * Push an element onto the stack
   * @param elem The element
   * @return A new stack with the updated top element
   */
  def push(elem: T): Stack[T] = {
    Stack(data.prepended(elem))
  }

  /**
   * Checks if the stack is empty
   * @return A boolean indicating whether the stack is empty or not
   */
  def isEmpty: Boolean = {
    data.isEmpty
  }

  /**
   * Pops the stack so long as the condition is met, or the stack is empty
   * @param f The condition to pop the stack
   * @return An updated stack
   */
  @scala.annotation.tailrec
  def popWhile(f: T => Boolean): Stack[T] = {
    if(isEmpty) return this
    if(f(peak)) {
      pop.popWhile(f)
    } else {
      this
    }
  }
}