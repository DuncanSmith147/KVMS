

##Copyright (c) 2011 duncan g. smith
##
##Permission is hereby granted, free of charge, to any person obtaining a
##copy of this software and associated documentation files (the "Software"),
##to deal in the Software without restriction, including without limitation
##the rights to use, copy, modify, merge, publish, distribute, sublicense,
##and/or sell copies of the Software, and to permit persons to whom the
##Software is furnished to do so, subject to the following conditions:
##
##The above copyright notice and this permission notice shall be included
##in all copies or substantial portions of the Software.
##
##THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
##OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
##FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
##THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
##OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
##ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
##OTHER DEALINGS IN THE SOFTWARE.

from __future__ import division


class UnionFindSet(dict):
    """
    A set-based union-find data structure that allows
    the partition of equivalence classes as well as
    merging of equivalence classes.  All items that
    are added to the union-find must be hashable.
    """
    def __init__(self):
        """
        Initialise the instance, setting the number
        of equivalence classes, I{self.size}, to zero.

        @note: len(uf) returns the number of items in
               the union-find, uf
        """
        self.size = 0

    def add(self, item):
        """
        Adds an item to the union-find, raising an
        exception if the item is already present.

        @type  item: arbitrary hashable type
        @param item: an item to be added
        """
        if item in self:
            raise ValueError, 'item already in union-find'
        self[item] = set([item])
        self.size += 1

    def remove(self, item):
        """
        Removes the item from the union-find by removing
        it from its equivalence class and deleting its
        entry.

        @type      item: arbitrary hashable type
        @param     item: an item to be removed
        @raise KeyError: if item is not in union-find
        """
        self[item].remove(item)
        if not self[item]:
            self.size -= 1
        del self[item]

    def union(self, item1, item2):
        """
        Merges the equivalence classes that have I{item1}
        and I{item2} as members.  If the items are members
        of the same equivalence class no change is made.

        @type     item1: arbitrary hashable type
        @param    item1: member of equivalence class
                         to be merged
        @type     item2: arbitrary hashable type
        @param    item2: member of equivalence class
                         to be merged
        @rtype:          C{bool}
        @return:         True if distinct equivalence classes
                         are merged, otherwise False
        @raise KeyError: if item is not in union-find
        """
        if self[item1] is self[item2]:
            return False
        else:
            if len(self[item1]) < len(self[item2]):
                item2, item1 = item1, item2
            self[item1] |= self[item2]
            for item in self[item2]:
                self[item] = self[item1]
            self.size -= 1
            return True

    def partition(self, items):
        """
        Partitions the equivalence class containing I{items}
        into two new equivalence classes containing I{items}
        and the equivalence class's remaining items.

        @type       items: C{set}
        @param      items: new equivalence class
        @raise ValueError: if I{items} is not a subset
                           of an existing equivalence class
        """
        equiv_class = self[iter(items).next()]
        new_class = equiv_class - items
        if not len(new_class) + len(items) == len(equiv_class):
            # items is not a subset of any existing equiv_class
            raise ValueError, 'items is not a subset of any \
                               existing equivalence class'
        for item in items:
            self[item] = items
        for item in new_class:
            self[item] = new_class
        self.size += 1


class UnionFindTree(dict):
    """
    A tree-based union-find data structure that allows
    the merging of equivalence classes only.  All items
    that are added to the union-find must be hashable.

    This tree-based implementation tends to be faster
    than the set-based implementation, but with the
    restriction that partitioning of equivalence classes
    is not possible.
    """
    def __init__(self):
        self.ranks = {}
        self.size = 0

    def copy(self):
        acopy = self.__class__()
        for item in self:
            acopy.add(item)
        for item in self:
            acopy.union(item, self.find(item))
        return acopy

    def add(self, item):
        """
        Adds an item to the union-find, raising an
        exception if the item is already present.

        @type  item: arbitrary hashable type
        @param item: an item to be added
        """
        if item in self:
            raise ValueError, 'item already in union-find'
        self[item] = None
        # ranks holds ranks for each forest {root:rank}
        self.ranks[item] = 0
        self.size += 1

    def find(self, item):
        """
        Returns the root of the tree containing
        I{item}.

        @type      item: arbitrary hashable type
        @param     item: an item in the union-find
        @rtype:          arbitrary hashable type
        @return:         root of tree containining I{item}
        @raise KeyError: if item is not in union-find
        """
        cand = item
        path = []
        while True:
            parent = self[cand]
            if parent is None:
                # cand is root
                for node in path:
                    self[node] = cand
                return cand
            else:
                path.append(cand)
                cand = parent

    def union(self, item1, item2):
        """
        Merges the equivalence classes that have I{item1}
        and I{item2} as members.  If the items are members
        of the same equivalence class no change is made.

        @type     item1: arbitrary hashable type
        @param    item1: member of equivalence class
                         to be merged
        @type     item2: arbitrary hashable type
        @param    item2: member of equivalence class
                         to be merged
        @rtype:          C{bool}
        @return:         True if distinct equivalence classes
                         are merged, otherwise False
        @raise KeyError: if item is not in union-find
        """
        root1, root2 = self.find(item1), self.find(item2)
        if root1 == root2:
            return False
        else:
            if self.ranks[root1] < self.ranks[root2]:
                self[root1] = root2
            elif self.ranks[root2] < self.ranks[root1]:
                self[root2] = root1
            else:
                self[root1] = root2
                self.ranks[root2] += 1
            self.size -= 1
            return True

def partition2tree(partition):
    tree = UnionFindTree()
    for part in partition:
        part = iter(part)
        first = part.next()
        tree.add(first)
        for element in part:
            tree.add(element)
            tree.union(tree.find(first), element)
    return tree

def tree2file(tree, filename):
    import dot
    import digraph
    g = digraph.DirectedGraph()
    for node in tree:
        g.addNode(node)
    for node in tree:
        parent = tree[node]
        if parent:
            g.addEdge((parent, node))
    dot.graph2image(g, filename, True, filename[-3:])

def harmonize(trees):
    res = UnionFindTree()
    for tree in trees:
        for node in tree:
            try:
                res.add(node)
            except:
                pass
            root = tree.find(node)
            res.union(root, node)
    return res
            
