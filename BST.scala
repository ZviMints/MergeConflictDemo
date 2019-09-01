
// Added Branch in Master
class BST {
  
  class Node(var data: Int,var left: Node,var right:Node) {
    def this(data: Int) = this(data,null,null)
  }
  
  var root : Node = null
  
  // Public methods
  def contains(x: Int) : Boolean = contains(x,root)
  def insert(x: Int) : Boolean = {
    if(this.contains(x) == true) 
          return false
    else
      root = insert(x,root)
      true
  }
  def inOrder() : Unit = inOrder(root)
  def height() : Int = height(root)
  
	def delete(key : Int) : Unit = root = deleteRec(root, key)
	
	//  override def toString() : String = ???

	
  // Private Methods
	
  /* A recursive function to insert a new key in BST */
	def deleteRec(root: Node,key: Int) : Node = {

		/* Base Case: If the tree is empty */
		if(root == null) return root;

		/* Otherwise, recur down the tree */
		if(key  < root.data)
			root.left = deleteRec(root.left, key); 
		else if (key > root.data) 
			root.right = deleteRec(root.right, key); 

		// if key is same as root's key, then This is the node 
		// to be deleted 
		else {

			// node with no child 
			if( root.left == null && root.right == null) return null;

			// node with only one child
			if (root.left == null)  // No Left Child
				return root.right; 
			else if (root.right == null) // No Right Child
				return root.left;

			// nodes with two children

			// Find Minimum Value
			var current = root.left;
			while(current.right != null ) current = current.right;
			var minValue = current.data; 

			// Set root data to minimum value
			root.data = minValue;

			// Delete the node
			root.left = deleteRec(root.left, minValue);
		}
		root;
	}
  private def inOrder(current: Node) : Unit = {
    if(current != null) {
      inOrder(current.left)
      println(current.data)
      inOrder(current.right)
    }
  }
  private def contains(x: Int, current: Node) : Boolean = {
    if(current == null) return false
    var cmp = x - current.data
    if(cmp > 0) return contains(x,current.right)
    else if(cmp < 0) return contains(x,current.left)
    true
  }
  private def insert(x: Int, current: Node) : Node = {    
    
    // Base case
    if(current == null) return new Node(x)
    
    // Compare current value to the value in the node
    var cmp = x - current.data
    
    if(cmp > 0) 
      current.right = insert(x, current.right)
    else
      current.left = insert(x,current.left)
     
    return current
  }
  def height(current: Node) : Int = {
    if(current == null) return -1
    return height(current.right) max height(current.left) + 1
  }
}
object BST extends App {
  var tree = new BST
  tree insert 3
  tree insert 4
  tree insert 2
  tree insert -1
  tree inOrder
}