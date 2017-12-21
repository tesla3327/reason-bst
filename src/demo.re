type btree =
  | Leaf
  | Node(btree, int, btree);

let rec insert = (x, tree) => {
  switch tree {
  | Leaf => Node(Leaf, x, Leaf);
  | Node(left, k, right) => 
    if (x < k) {
      Node(insert(x, left), k, right);
    } else {
      Node(left, k, insert(x, right));
    }
  };
};

let rec insertList = (list, tree) => {
  switch list {
  | [] => tree
  | [head, ...tail] => insertList(tail, insert(head, tree))
  }
};

let rec traverse = (tree, func) => {
  switch tree {
  | Leaf => ()
  | Node(left, k, right) =>
    traverse(left, func);
    func(k);
    traverse(right, func);
  }
};

let rec findNode = (tree, x) => {
  switch tree {
  | Leaf => false
  | Node(left, k, right) =>
    if (x === k) {
      true
    } else if (x < k) {
      findNode(left, x);
    } else {
      findNode(right, x);
    }
  }
};

let rec findMinValue = (tree, min) => {
  switch tree {
  | Leaf => min
  | Node(left, k, _) => findMinValue(left, k)
  }
};

let rec deleteNode = (tree, x) => {
  switch tree {
  | Leaf => Leaf
  | Node(left, k, right) =>
    if (x > k) {
      Node(left, k, deleteNode(right, x))
    } else if (x < k) {
      Node(deleteNode(left, x), k, right)
    } else {
      switch (left, right) {
      | (Leaf, Leaf) => Leaf
      | (Node(_,_,_) as leftChild, Leaf) => leftChild
      | (Leaf, Node(_, _, _) as rightChild) => rightChild
      | (_ as leftChild, _ as rightChild) => 
        let min = findMinValue(rightChild, k);
        Node(leftChild, min, deleteNode(rightChild, min))
      }
    }
  }
};

let printTree = tree => traverse(tree, x => Js.log(x));

let tree = insertList([8, 2, 5, 3, 6, 1, 2, 7, 19, 3, 20], Leaf);
/* let tree = insertList([5, 3, 7, 4, 2], Leaf); */
/* Js.log(tree); */
let tree = deleteNode(tree, 3);
let tree = deleteNode(tree, 3);

/* Js.log(tree); */
printTree(tree);
