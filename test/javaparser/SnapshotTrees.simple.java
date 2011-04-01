class NodeState {
  Node node;
  boolean yielded;
  NodeState init_ (Node node) {
    this.node = node;
    this.yielded = false;
  }

}

class TreeIterator implements Iterator<Integer>{
  int oldStamp;
  Stack<NodeState> context;
  TreeIterator init_ (A1B1Tree tree, int oldStamp) {
    Node tmp_1 = tree.root;
    pushLeftPath(tmp_1);
    this.oldStamp = oldStamp;
  }

  void pushLeftPath (Node node) {
    while (node != null) {
      NodeState tmp_1 = new NodeState(node);
      context.push(tmp_1);
      Node tmp_2 = node.left;
      node = tmp_2;
    } ;
  }

  boolean hasNext () {
    boolean tmp_1 = context.empty();
    return !tmp_1;
  }

  Integer next () {
    Integer result;
    int tmp_1 = this.oldStamp;
    if (stamp != tmp_1)
      new ConcurrentModificationException("Tree was modified during iteration");
    boolean tmp_2 = hasNext();
    if (tmp_2) {
      NodeState nodeState = context.peek();
      Node tmp_4 = nodeState.node;
      int tmp_5 = tmp_4.item;
      result = tmp_5;
      nodeState.yielded = true;
      Node tmp_6 = nodeState.node;
      Node tmp_7 = tmp_6.rght;
      if (tmp_7 != null) {
        Node tmp_8 = nodeState.node;
        Node tmp_9 = tmp_8.rght;
        pushLeftPath(tmp_9);
      } ;
      else {
        NodeState tmp_10 = context.peek();
        boolean tmp_11 = tmp_10.yielded;
        boolean tmp_12 = context.empty();
        while (!tmp_12 && tmp_11)
          context.pop();
      } ;
    } ;
    else
      new Error("Iterator: No more items");
    return result;
  }

  void remove () {
    new Error("remove not implemented");
  }

}

class Node {
  int item;
  Node left;
  Node rght;
  Node init_ (Node left, int item, Node rght) {
    this.left = left;
    this.item = item;
    this.rght = rght;
  }

  String toString () {
    Node tmp_2 = this.rght;
    String tmp_1;
    if (tmp_2 == null)
      tmp_1 = "_";
    else {
      String tmp_3 = rght.toString();
      tmp_1 = tmp_3;
    } ;
    int tmp_4 = this.item;
    Node tmp_6 = this.left;
    String tmp_5;
    if (tmp_6 == null)
      tmp_5 = "_";
    else {
      String tmp_7 = left.toString();
      tmp_5 = tmp_7;
    } ;
    return "[" + tmp_5 + "," + tmp_4 + tmp_1 + "]";
  }

}

interface ITree extends Iterable<Integer>{
  boolean contains (int item) { }

  boolean add (int item) { }

  int get (int i) { }

  ITree snapshot () { }

  Iterator<Integer> iterator () { }

}

class A1B1Tree implements ITree{
  Node root;
  boolean isSnapshot;
  boolean hasSnapshot;
  int stamp;
  A1B1Tree init_ (A1B1Tree tree) {
    Node tmp_1 = tree.root;
    this.root = tmp_1;
    this.isSnapshot = true;
  }

  boolean contains (int item) {
    Node node = this.root;
    boolean found = false;
    while (!found && node != null) {
      int tmp_2 = node.item;
      if (item < tmp_2) {
        Node tmp_3 = node.left;
        node = tmp_3;
      } ;
      else {
        int tmp_4 = node.item;
        if (tmp_4 < item) {
          Node tmp_5 = node.rght;
          node = tmp_5;
        } ;
        else
          found = true;
      } ;
    } ;
    return found;
  }

  int get (int i) {
    Node tmp_1 = this.root;
    int tmp_2 = getNode(tmp_1, i);
    return tmp_2;
  }

  int count (Node n) {
    int res;
    if (n == null)
      res = 0;
    else {
      Node tmp_1 = n.rght;
      res = count(tmp_1);
      Node tmp_2 = n.left;
      res = count(tmp_2);
      res = res + 1 + res;
    } ;
    return res;
  }

  int getNode (Node n, int i) {
    Node tmp_1 = n.left;
    int leftCount = count(tmp_1);
    while (i != leftCount) {
      if (i < leftCount) {
        Node tmp_3 = n.left;
        n = tmp_3;
      } ;
      else {
        Node tmp_4 = n.rght;
        n = tmp_4;
        i = i - leftCount - 1;
      } ;
      Node tmp_5 = n.left;
      leftCount = count(tmp_5);
    } ;
    int tmp_6 = n.item;
    return tmp_6;
  }

  boolean add (int item) {
    RefBool updated = new RefBool();
    boolean tmp_2 = this.isSnapshot;
    if (tmp_2)
      new RuntimeException("Illegal to add to snapshot");
    else {
      Node tmp_3 = this.root;
      Node tmp_4 = addRecursive(tmp_3, item, updated);
      this.root = tmp_4;
    } ;
    boolean tmp_5 = updated.value;
    return tmp_5;
  }

  Node addRecursive (Node node, int item, RefBool updated) {
    Node res = node;
    if (node == null) {
      updated.value = true;
      int tmp_1 = this.stamp;
      this.stamp = 1 + tmp_1;
      res = new Node(item);
    } ;
    else {
      int tmp_2 = node.item;
      if (item < tmp_2) {
        Node tmp_3 = node.left;
        Node newLeft = addRecursive(tmp_3, item, updated);
        boolean tmp_5 = updated.value;
        boolean tmp_6 = this.hasSnapshot;
        if (tmp_6 && tmp_5) {
          int tmp_7 = node.item;
          res = node.rght;
          res = new Node(newLeft, tmp_7, res);
        } ;
        else
          node.left = newLeft;
      } ;
      else {
        int tmp_8 = node.item;
        if (tmp_8 < item) {
          Node tmp_9 = node.rght;
          Node newRght = addRecursive(tmp_9, item, updated);
          boolean tmp_11 = updated.value;
          boolean tmp_12 = this.hasSnapshot;
          if (tmp_12 && tmp_11) {
            res = node.left;
            int tmp_13 = node.item;
            res = new Node(res, tmp_13, newRght);
          } ;
          else
            node.rght = newRght;
        } ;
      } ;
    } ;
    return res;
  }

  ITree snapshot () {
    boolean tmp_1 = this.isSnapshot;
    if (tmp_1)
      new RuntimeException("Illegal to snapshot a snapshot");
    else
      this.hasSnapshot = true;
    A1B1Tree tmp_2 = new A1B1Tree(this);
    return tmp_2;
  }

  Iterator<Integer> iterator () {
    int tmp_1 = this.stamp;
    TreeIterator tmp_2 = new TreeIterator(this, tmp_1);
    return tmp_2;
  }

  String toString () {
    String tmp_1 = root.toString();
    return tmp_1;
  }

}

class RefBool {
  boolean value;
}
