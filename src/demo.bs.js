// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function insert(x, tree) {
  if (tree) {
    var right = tree[2];
    var k = tree[1];
    var left = tree[0];
    if (x < k) {
      return /* Node */[
              insert(x, left),
              k,
              right
            ];
    } else {
      return /* Node */[
              left,
              k,
              insert(x, right)
            ];
    }
  } else {
    return /* Node */[
            /* Leaf */0,
            x,
            /* Leaf */0
          ];
  }
}

function insertList(_list, _tree) {
  while(true) {
    var tree = _tree;
    var list = _list;
    if (list) {
      _tree = insert(list[0], tree);
      _list = list[1];
      continue ;
      
    } else {
      return tree;
    }
  };
}

function traverse(_tree, func) {
  while(true) {
    var tree = _tree;
    if (tree) {
      traverse(tree[0], func);
      Curry._1(func, tree[1]);
      _tree = tree[2];
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function findNode(_tree, x) {
  while(true) {
    var tree = _tree;
    if (tree) {
      var k = tree[1];
      if (x === k) {
        return /* true */1;
      } else if (x < k) {
        _tree = tree[0];
        continue ;
        
      } else {
        _tree = tree[2];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function findMinValue(_tree, _min) {
  while(true) {
    var min = _min;
    var tree = _tree;
    if (tree) {
      _min = tree[1];
      _tree = tree[0];
      continue ;
      
    } else {
      return min;
    }
  };
}

function deleteNode(tree, x) {
  if (tree) {
    var right = tree[2];
    var k = tree[1];
    var left = tree[0];
    if (x > k) {
      return /* Node */[
              left,
              k,
              deleteNode(right, x)
            ];
    } else if (x < k) {
      return /* Node */[
              deleteNode(left, x),
              k,
              right
            ];
    } else if (left) {
      if (right) {
        var min = findMinValue(right, k);
        return /* Node */[
                left,
                min,
                deleteNode(right, min)
              ];
      } else {
        return left;
      }
    } else if (right) {
      return right;
    } else {
      return /* Leaf */0;
    }
  } else {
    return /* Leaf */0;
  }
}

function printTree(tree) {
  return traverse(tree, (function (x) {
                console.log(x);
                return /* () */0;
              }));
}

var tree = insertList(/* :: */[
      8,
      /* :: */[
        2,
        /* :: */[
          5,
          /* :: */[
            3,
            /* :: */[
              6,
              /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    7,
                    /* :: */[
                      19,
                      /* :: */[
                        3,
                        /* :: */[
                          20,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ], /* Leaf */0);

var tree$1 = deleteNode(tree, 3);

var tree$2 = deleteNode(tree$1, 3);

printTree(tree$2);

exports.insert       = insert;
exports.insertList   = insertList;
exports.traverse     = traverse;
exports.findNode     = findNode;
exports.findMinValue = findMinValue;
exports.deleteNode   = deleteNode;
exports.printTree    = printTree;
exports.tree         = tree$2;
/* tree Not a pure module */
