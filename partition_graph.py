
##Copyright (c) 2014 duncan g. smith
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

# requires Python 2.6 or greater (for frozensets)

from __future__ import division

import itertools
import json
from abc import ABCMeta, abstractmethod

from digraph import DirectedGraph2
from traversals import dfs
import dot


class PartitionGraphError(Exception): pass


class PartitionGraph(DirectedGraph2):
    def __init__(self):
        super(PartitionGraph, self).__init__()

    def structure_is_valid(self):
        # the induced graph containing the nodes reachable from
        # each root should be a tree
        for root in self.root_nodes:
            visited = set()
            num_edges = 0
            num_nodes = 0
            for node in dfs(self, root, post=True):
                num_nodes += 1
                num_edges += self.outdegree(node)
            if not num_nodes == num_edges + 1:
                return False
        return True

    def to_json(self):
        return json.dumps([list(self.nodes), list(self.iter_edges())])

    @classmethod
    def from_json(cls, txt):
        nodes, edges = json.loads(txt)
        g = cls()
        for node in nodes:
            g.add_node(node)
        for edge in edges:
            g.add_edge(edge)
        return g

    def to_file(self, filename):
        with open(filename, 'w') as f:
            f.write(self.to_json())

    @classmethod
    def from_file(cls, filename):
        with open(filename, 'r') as f:
            return cls.from_json(f.read())

    def to_image(self, filename, format='png'):
        dot.graph2image(self, filename, format=format)

    def reachable(self, node):
        # return a generator of the leaf
        # nodes reachable from node
        for n in dfs(self, node, post=True):
            if not self.children[n]:
                yield n

    def leaf_partition(self, nodes):
        # returns the partition of the
        # leaf nodes implied by the nodes
        # in the sequence 'nodes' as
        # a list of sets
        # raises a ValueError if 'nodes' is not
        # a valid categorization w.r.t. the graph
        parts = []
        visited = set()
        for n in nodes:
            part = set()
            for leaf in self.reachable(n):
                if leaf in visited:
                    raise ValueError('%s is reachable from more than one node' % str(leaf))
                visited.add(leaf)
                part.add(leaf)
            parts.append(part)
        if len(visited) < len(self.leaf_nodes):
            raise ValueError('Not all leaf nodes are reachable from specified nodes')
        return parts

    def flatten(self):
        # flatten the structure (in place) so that
        # all nodes are either leaf or parents of leaf nodes
        for node in dfs(self, post=True):
            grand_children = set()
            for child in list(self.children[node]):
                if self.outdegree(child):
                    self.del_edge((node, child))
                    grand_children |= self.children[child]
            for gc in grand_children:
                self.add_edge((node, gc))

    def flattened(self):
        # return a flattened copy
        acopy = self.copy()
        acopy.flatten()
        return acopy

    def deepen(self):
        # deepen structure (in place) to provide
        # nicer layout for humans
        for node in list(dfs(self, post=True)):
            children = list(self.children[node])
            if children:
                it = iter(children)
                new_parents = set(self.parents[next(it)])
                new_parents.remove(node)
                for child in it:
                    if not new_parents:
                        break
                    new_parents &= self.parents[child]
                for p in new_parents:
                    for child in children:
                        self.del_edge((p, child))
                    self.add_edge((p, node))

    def deepened(self):
        # return a deepened copy
        acopy = self.copy()
        acopy.deepen()
        return acopy

    def remove(self, node):
        # remove node whilst
        # maintaining validity
        # leaf nodes cannot be removed
        # unless their indegree is 1
        # (otherwise raises a ValueError)
        if self.children[node]:
            for p in self.parents[node]:
                for c in self.children[node]:
                    self.add_edge((p, c))
            self.del_node(node)
        else:
            # can only be removed (singly)
            # if it has indegree 1 and no siblings
            if (self.indegree(node) == 1 and
                self.outdegree(next(iter(self.parents[node]))) == 1):
                self.del_node(node)
            else:
                raise ValueError('Cannot remove leaf node')

    def discard(self, node):
        try:
            self.remove(node)
        except ValueError:
            pass

    def prune(self, to_keep, aggressive=False):
        to_keep = set(to_keep)
        if not self.nodes >= to_keep:
            raise ValueError('Not all nodes are in the graph')
        # prune non-leaf nodes first
        cands = self.nodes - to_keep - self.leaf_nodes
        for node in cands:
            self.remove(node)
        if aggressive:
            # attempt to prune leaf nodes
            leaves = self.leaf_nodes - to_keep
            while leaves:
                node = leaves.pop()
                if self.indegree(node) == 1:
                    parent = next(iter(self.parents[node]))
                    children = set(self.children[parent])
                    if (children.difference([node]) <= leaves and
                        all(self.indegree(c) == 1 for c in children)):
                        for c in children:
                            self.del_node(c)
                        if not parent in to_keep:
                            leaves.add(parent)
                        leaves -= children


class SetAPI:
    __metaclass__ = ABCMeta

    @abstractmethod
    def make_node(self, *args, **kwargs):
        pass

    def add(self, *args, **kwargs):
        # requires Python 2.6 or greater if using
        # frozensets as nodes
        # only for comparable nodes (supporting the relevant subset of the set API)
        # validity depends on added nodes being
        # exhaustive and new nodes not extending
        # cover (unless they have no intersection
        # with existing cover)
        node = self.make_node(*args, **kwargs)
        if not hasattr(node, 'issuperset'):
            raise ValueError('Node type does not support interface')
        if self.has_node(node):
            return
        leaf_nodes = list(self.leaf_nodes)
        self.add_node(node)
        for leaf in leaf_nodes:
            if node.issuperset(leaf):
                # requires existing leaves to be
                # exhaustive
                self.add_edge((node, leaf))
            else:
                intersect = node.intersection(leaf)
                if intersect:
                    if node != intersect:
                        # node is not a subset of leaf
                        self.add_node(intersect)
                        self.add_edge((node, intersect))
                    self.add_edge((leaf, intersect))
                    diffs = leaf.difference(intersect)
                    if not isinstance(diffs, list):
                        diffs = [diffs]
                    for diff in diffs:
                        self.add_node(diff)
                        self.add_edge((leaf, diff))
                    if node == intersect:
                        break
        # following code only required
        # where node extends cover
        if self.outdegree(node):
            diffs = node.difference(*leaf_nodes)
            if diffs:
                if not isinstance(diffs, list):
                    diffs = [diffs]
                for diff in diffs:
                    self.add_node(diff)
                    self.add_edge((node, diff))

    def is_valid(self, cover=None):
        if not self.structure_is_valid():
            print 'invalid structure'
            return False
        for node in self.iter_nodes():
            degree = self.outdegree(node)
            children = iter(self.children[node])
            if degree == 1:
                if not node == next(children):
                    print node
                    return False
            elif degree > 1:
                if not node == next(children).union(*children):
                    print node
                    return False
        if len(self.leaf_nodes) > 1:
            # check they are disjoint
            for a, b in itertools.combinations(self.leaf_nodes, 2):
                if a.intersection(b):
                    print 'non-disjoint leaf nodes'
                    return False
        if cover is not None:
            leaves = iter(self.leaf_nodes)
            if len(self.leaf_nodes) == 0: # empty graph
                if cover:
                    print 'incorrect cover'
                    return False
            elif len(self.leaf_nodes) == 1:
                if not cover == next(leaves):
                    print 'incorrect cover'
                    return False
            else:
                if not cover == next(leaves).union(*leaves):
                    print 'incorrect cover'
                    return False
        return True


class IntervalGraph(PartitionGraph, SetAPI):
    def __init__(self, intervals=None):
        # intervals is an iterable containing pairs of lower and upper bounds
        super(IntervalGraph, self).__init__()
        if intervals:
            for tup in intervals:
                self.add(tup)

    def make_node(self, tup):
        a, b = tup
        if not a < b:
            raise ValueError('Empty interval')
        return Interval(a, b)

    def to_json(self):
        return json.dumps([[(n.a, n.b) for n in self.nodes],
                           [((n.a, n.b), (m.a, m.b)) for n, m in self.iter_edges()]])

    @classmethod
    def from_json(cls, txt):
        node_data, edge_data = json.loads(txt)
        g = cls()
        nodes = [g.make_node(lis) for lis in node_data]
        edges = [(g.make_node(lis1), g.make_node(lis2)) for lis1, lis2 in edge_data]
        for node in nodes:
            g.add_node(node)
        for edge in edges:
            g.add_edge(edge)
        return g


class SetGraph(PartitionGraph, SetAPI):
    def __init__(self, sets=None):
        # sets is an iterable containing iterables
        super(SetGraph, self).__init__()
        if sets:
            for set_ in sets:
                self.add(set_)

    def make_node(self, set_):
        return frozenset(set_)

    def to_json(self):
        return json.dumps([[list(n) for n in self.nodes],
                           [(list(n), list(m)) for n, m in self.iter_edges()]])

    @classmethod
    def from_json(cls, txt):
        node_data, edge_data = json.loads(txt)
        g = cls()
        nodes = [g.make_node(lis) for lis in node_data]
        edges = [(g.make_node(lis1), g.make_node(lis2)) for lis1, lis2 in edge_data]
        for node in nodes:
            g.add_node(node)
        for edge in edges:
            g.add_edge(edge)
        return g


class Interval(object):
    # a very simple (non-disjoint) interval class that does not
    # explicitly handle open / closed endpoints
    def __init__(self, a, b):
        # a and b must be hashable
        if a > b:
            raise ValueError('Invalid interval, %s > %s' % (str(a), str(b)))
        self.a = a
        self.b = b

    def __nonzero__(self):
        return self.a != self.b

    def __str__(self):
        return str((self.a, self.b))

    __repr__ = __str__

    def issubset(self, other):
        return self.a >= other.a and self.b <= other.b

    def issuperset(self, other):
        return other.issubset(self)

    def union(self, *others):
        # can return a single interval or a list of intervals
        ranges = [(other.a, other.b) for other in others if other]
        if self:
            ranges.append((self.a, self.b))
        if not ranges:
            return self.__class__(0, 0)
        ranges.sort()
        res = []
        it = iter(ranges)
        a, b = next(it)
        for x, y in it:
            if x > b:
                res.append(self.__class__(a, b))
                a = x
            b = max([y, b])
        res.append(self.__class__(a, b))
        if len(res) == 1:
            res = res[0]
        return res

    def intersection(self, *others):
        # empty intersection has both parameters
        # equal to 0
        ranges = [(other.a, other.b) for other in others if other]
        try:
            if self:
                ranges.append((self.a, self.b))
        except:
            print self, self.a, self.b
            raise
        if not ranges:
            return self.__class__(0, 0)
        it = iter(ranges)
        a, b = next(it)
        for x, y in it:
            a = max([a, x])
            b = min([b, y])
            if a >= b:
                return self.__class__(0, 0)
        return self.__class__(a, b)

    def difference(self, *others):
        # can return a single interval or a list of intervals
        if not self:
            return self.__class__(0, 0)
        others = self.__class__(0, 0).union(*others)
        if not isinstance(others, list):
            others = [others]
        ranges = [(other.a, other.b) for other in others if other]
        ranges.sort()
        res = []
        a, b = self.a, self.b
        for x, y in ranges:
            if x <= a:
                if a < y < b:
                    a = y
                elif y >= b:
                    a = b = 0
                    break
            elif x >= b:
                break
            else:
                res.append(self.__class__(a, x))
                if y < b:
                    a = y
                else:
                    a = b = 0
                    break
        res.append(self.__class__(a, b))
        res = [x for x in res if x]
        if not res:
            res = [self.__class__(0, 0)]
        if len(res) == 1:
            res = res[0]
        return res

    def __eq__(self, other):
        return (self.a, self.b) == (other.a, other.b)

    def __ne__(self, other):
        return (self.a, self.b) != (other.a, other.b)

    def __hash__(self):
        return hash((self.a, self.b))


######################### Example graphs and basic tests ##########################


age_ranges = [(25, 26), (30, 45), (75, 76), (83, 84), (30, 40), (84, float('inf')),
              (89, 90), (56, 60), (56, 66), (20, 21), (20, 24), (75, 85),
              (25, 30), (36, 40), (25, 35), (36, 46), (45, 46), (92, 93),
              (26, 36), (16, 20), (16, 24), (16, 26), (88, 89), (60, float('inf')),
              (93, 94), (70, 71), (60, 65), (77, 78), (46, 50), (90, 91),
              (45, 55), (46, 56), (17, 26), (17, 25), (80, 81), (70, 75),
              (24, 25), (70, float('inf')), (95, float('inf')), (1, 2), (11, 12), (78, 79),
              (55, 56), (6, 7), (9, 10), (22, 24), (30, 31), (17, 18), (8, 9),
              (65, float('inf')), (26, 30), (50, 55), (31, 46), (16, 17), (16, 18),
              (35, 45), (40, 50), (15, 16), (0, 22), (0, 21), (2, 3), (87, 88),
              (16, float('inf')), (31, 35), (46, 60), (65, 66), (14, 15), (40, 45),
              (0, 16), (0, 17), (7, 8), (4, 5), (71, 75), (35, 36), (20, 25),
              (45, 60), (24, 31), (10, 11), (82, 83), (91, 92), (79, 80),
              (18, 20), (65, 70), (65, 75), (0, 1), (86, 87), (55, 65),
              (13, 14), (50, 60), (21, float('inf')), (84, 85), (94, 95), (76, 77),
              (21, 22), (21, 25), (5, 6), (12, 13), (81, 82), (3, 4),
              (85, 86), (0, 71), (66, 70)]

sets = [range(a, b) for a, b in age_ranges if not b==float('inf')]

def age():
    txt = """[["25-26", "30-45", "75-76", "83-84", "30-40", "84-inf",
               "89-90", "56-60", "56-66", "20-21", "20-24", "75-85",
               "25-30", "36-40", "25-35", "36-46", "45-46", "92-93",
               "50-55", "16-20", "16-24", "16-26", "80-81", "60-inf",
               "70-75", "78-79", "60-65", "77-78", "46-50", "90-91",
               "45-55", "46-56", "17-26", "17-25", "88-89", "93-94",
               "84-85", "70-inf", "95-inf", "94-95", "11-12", "70-71",
               "55-56", "6-7", "9-10", "22-24", "30-31", "17-18", "8-9",
               "65-inf", "26-30", "26-36", "31-46", "16-17", "16-18",
               "35-45", "40-50", "15-16", "0-22", "0-21", "2-3",
               "87-88", "16-inf", "31-35", "46-60", "65-66", "14-15",
               "40-45", "0-16", "0-17", "7-8", "4-5", "71-75", "35-36",
               "20-25", "81-82", "24-31", "10-11", "82-83", "91-92",
               "79-80", "18-20", "65-70", "65-75", "0-1", "86-87",
               "55-65", "13-14", "50-60", "21-inf", "24-25", "1-2",
               "76-77", "21-22", "!16-18", "21-25", "5-6", "12-13",
               "45-60", "3-4", "85-86", "0-71", "66-70"], [["30-45",
               "36-40"], ["30-45", "35-36"], ["30-45", "31-35"],
               ["30-45", "40-45"], ["30-45", "30-31"], ["30-40",
               "36-40"], ["30-40", "35-36"], ["30-40", "31-35"],
               ["30-40", "30-31"], ["84-inf", "86-87"], ["84-inf",
               "91-92"], ["84-inf", "84-85"], ["84-inf", "94-95"],
               ["84-inf", "85-86"], ["84-inf", "93-94"], ["84-inf",
               "90-91"], ["84-inf", "92-93"], ["84-inf", "88-89"],
               ["84-inf", "89-90"], ["84-inf", "95-inf"], ["84-inf",
               "87-88"], ["56-66", "56-60"], ["56-66", "60-65"],
               ["56-66", "65-66"], ["20-24", "20-21"], ["20-24",
               "22-24"], ["20-24", "21-22"], ["75-85", "82-83"],
               ["75-85", "81-82"], ["75-85", "78-79"], ["75-85",
               "79-80"], ["75-85", "77-78"], ["75-85", "75-76"],
               ["75-85", "80-81"], ["75-85", "76-77"], ["75-85",
               "83-84"], ["75-85", "84-85"], ["25-30", "25-26"],
               ["25-30", "26-30"], ["25-35", "25-26"], ["25-35",
               "31-35"], ["25-35", "26-30"], ["25-35", "30-31"],
               ["36-46", "36-40"], ["36-46", "40-45"], ["36-46",
               "45-46"], ["16-20", "16-17"], ["16-20", "17-18"],
               ["16-20", "18-20"], ["16-24", "16-17"], ["16-24",
               "22-24"], ["16-24", "17-18"], ["16-24", "18-20"],
               ["16-24", "20-21"], ["16-24", "21-22"], ["16-26",
               "16-17"], ["16-26", "22-24"], ["16-26", "20-21"],
               ["16-26", "24-25"], ["16-26", "17-18"], ["16-26",
               "25-26"], ["16-26", "18-20"], ["16-26", "21-22"],
               ["60-inf", "60-65"], ["60-inf", "91-92"], ["60-inf",
               "79-80"], ["60-inf", "77-78"], ["60-inf", "75-76"],
               ["60-inf", "90-91"], ["60-inf", "83-84"], ["60-inf",
               "78-79"], ["60-inf", "89-90"], ["60-inf", "82-83"],
               ["60-inf", "86-87"], ["60-inf", "88-89"], ["60-inf",
               "71-75"], ["60-inf", "95-inf"], ["60-inf", "87-88"],
               ["60-inf", "84-85"], ["60-inf", "94-95"], ["60-inf",
               "76-77"], ["60-inf", "92-93"], ["60-inf", "65-66"],
               ["60-inf", "85-86"], ["60-inf", "81-82"], ["60-inf",
               "80-81"], ["60-inf", "93-94"], ["60-inf", "70-71"],
               ["60-inf", "66-70"], ["70-75", "71-75"], ["70-75",
               "70-71"], ["45-55", "46-50"], ["45-55", "45-46"],
               ["45-55", "50-55"], ["46-56", "46-50"], ["46-56",
               "50-55"], ["46-56", "55-56"], ["17-26", "22-24"],
               ["17-26", "20-21"], ["17-26", "24-25"], ["17-26",
               "17-18"], ["17-26", "25-26"], ["17-26", "18-20"],
               ["17-26", "21-22"], ["17-25", "22-24"], ["17-25",
               "24-25"], ["17-25", "17-18"], ["17-25", "18-20"],
               ["17-25", "20-21"], ["17-25", "21-22"], ["70-inf",
               "82-83"], ["70-inf", "91-92"], ["70-inf", "79-80"],
               ["70-inf", "77-78"], ["70-inf", "75-76"], ["70-inf",
               "90-91"], ["70-inf", "83-84"], ["70-inf", "89-90"],
               ["70-inf", "86-87"], ["70-inf", "80-81"], ["70-inf",
               "88-89"], ["70-inf", "71-75"], ["70-inf", "95-inf"],
               ["70-inf", "70-71"], ["70-inf", "87-88"], ["70-inf",
               "84-85"], ["70-inf", "94-95"], ["70-inf", "76-77"],
               ["70-inf", "92-93"], ["70-inf", "81-82"], ["70-inf",
               "85-86"], ["70-inf", "93-94"], ["70-inf", "78-79"],
               ["65-inf", "82-83"], ["65-inf", "91-92"], ["65-inf",
               "79-80"], ["65-inf", "77-78"], ["65-inf", "75-76"],
               ["65-inf", "90-91"], ["65-inf", "83-84"], ["65-inf",
               "78-79"], ["65-inf", "89-90"], ["65-inf", "86-87"],
               ["65-inf", "85-86"], ["65-inf", "88-89"], ["65-inf",
               "71-75"], ["65-inf", "95-inf"], ["65-inf", "87-88"],
               ["65-inf", "84-85"], ["65-inf", "94-95"], ["65-inf",
               "76-77"], ["65-inf", "92-93"], ["65-inf", "65-66"],
               ["65-inf", "81-82"], ["65-inf", "80-81"], ["65-inf",
               "93-94"], ["65-inf", "70-71"], ["65-inf", "66-70"],
               ["26-36", "26-30"], ["26-36", "35-36"], ["26-36",
               "31-35"], ["26-36", "30-31"], ["31-46", "36-40"],
               ["31-46", "35-36"], ["31-46", "31-35"], ["31-46",
               "40-45"], ["31-46", "45-46"], ["16-18", "17-18"],
               ["16-18", "16-17"], ["35-45", "36-40"], ["35-45",
               "35-36"], ["35-45", "40-45"], ["40-50", "45-46"],
               ["40-50", "40-45"], ["40-50", "46-50"], ["0-22",
               "2-3"], ["0-22", "7-8"], ["0-22", "15-16"], ["0-22",
               "4-5"], ["0-22", "11-12"], ["0-22", "10-11"], ["0-22",
               "1-2"], ["0-22", "3-4"], ["0-22", "21-22"], ["0-22",
               "12-13"], ["0-22", "13-14"], ["0-22", "18-20"], ["0-22",
               "8-9"], ["0-22", "0-1"], ["0-22", "16-17"], ["0-22",
               "20-21"], ["0-22", "6-7"], ["0-22", "14-15"], ["0-22",
               "17-18"], ["0-22", "9-10"], ["0-22", "5-6"], ["0-21",
               "0-17"], ["0-21", "17-18"], ["0-21", "18-20"], ["0-21",
               "20-21"], ["16-inf", "35-45"], ["16-inf", "16-20"],
               ["16-inf", "25-35"], ["16-inf", "55-65"], ["16-inf",
               "45-55"], ["16-inf", "65-inf"], ["16-inf", "20-25"],
               ["46-60", "46-50"], ["46-60", "50-55"], ["46-60",
               "56-60"], ["46-60", "55-56"], ["0-16", "2-3"], ["0-16",
               "9-10"], ["0-16", "5-6"], ["0-16", "0-1"], ["0-16",
               "11-12"], ["0-16", "1-2"], ["0-16", "3-4"], ["0-16",
               "12-13"], ["0-16", "7-8"], ["0-16", "8-9"], ["0-16",
               "15-16"], ["0-16", "6-7"], ["0-16", "14-15"], ["0-16",
               "10-11"], ["0-16", "13-14"], ["0-16", "4-5"], ["0-17",
               "2-3"], ["0-17", "9-10"], ["0-17", "7-8"], ["0-17",
               "15-16"], ["0-17", "4-5"], ["0-17", "11-12"], ["0-17",
               "1-2"], ["0-17", "3-4"], ["0-17", "12-13"], ["0-17",
               "5-6"], ["0-17", "8-9"], ["0-17", "16-17"], ["0-17",
               "6-7"], ["0-17", "14-15"], ["0-17", "10-11"], ["0-17",
               "13-14"], ["0-17", "0-1"], ["20-25", "20-21"], ["20-25",
               "22-24"], ["20-25", "21-22"], ["20-25", "24-25"], ["24-31",
               "24-25"], ["24-31", "25-26"], ["24-31", "26-30"], ["24-31",
               "30-31"], ["65-70", "65-66"], ["65-70", "66-70"], ["65-75",
               "65-66"], ["65-75", "66-70"], ["65-75", "71-75"], ["65-75",
               "70-71"], ["55-65", "56-60"], ["55-65", "60-65"], ["55-65",
               "55-56"], ["50-60", "56-60"], ["50-60", "50-55"], ["50-60",
               "55-56"], ["21-inf", "25-35"], ["21-inf", "35-45"], ["21-inf",
               "45-55"], ["21-inf", "65-inf"], ["21-inf", "55-65"], ["21-inf",
               "21-25"], ["!16-18", "10-11"], ["!16-18", "82-83"], ["!16-18",
               "36-40"], ["!16-18", "91-92"], ["!16-18", "25-26"], ["!16-18",
               "55-56"], ["!16-18", "79-80"], ["!16-18", "50-55"], ["!16-18",
               "46-50"], ["!16-18", "21-22"], ["!16-18", "90-91"], ["!16-18",
               "83-84"], ["!16-18", "18-20"], ["!16-18", "89-90"], ["!16-18",
               "60-65"], ["!16-18", "86-87"], ["!16-18", "88-89"], ["!16-18",
               "56-60"], ["!16-18", "40-45"], ["!16-18", "13-14"], ["!16-18",
               "24-25"], ["!16-18", "20-21"], ["!16-18", "26-30"], ["!16-18",
               "95-inf"], ["!16-18", "71-75"], ["!16-18", "2-3"], ["!16-18",
               "70-71"], ["!16-18", "1-2"], ["!16-18", "87-88"], ["!16-18",
               "11-12"], ["!16-18", "84-85"], ["!16-18", "94-95"], ["!16-18",
               "45-46"], ["!16-18", "76-77"], ["!16-18", "31-35"], ["!16-18",
               "0-1"], ["!16-18", "92-93"], ["!16-18", "6-7"], ["!16-18",
               "14-15"], ["!16-18", "77-78"], ["!16-18", "9-10"], ["!16-18",
               "22-24"], ["!16-18", "5-6"], ["!16-18", "15-16"], ["!16-18",
               "4-5"], ["!16-18", "30-31"], ["!16-18", "81-82"], ["!16-18",
               "35-36"], ["!16-18", "7-8"], ["!16-18", "75-76"], ["!16-18",
               "80-81"], ["!16-18", "12-13"], ["!16-18", "93-94"], ["!16-18",
               "85-86"], ["!16-18", "8-9"], ["!16-18", "78-79"], ["!16-18",
               "66-70"], ["!16-18", "3-4"], ["!16-18", "65-66"], ["21-25",
               "21-22"], ["21-25", "22-24"], ["21-25", "24-25"], ["45-60",
               "46-50"], ["45-60", "55-56"], ["45-60", "56-60"], ["45-60",
               "45-46"], ["45-60", "50-55"], ["0-71", "10-11"], ["0-71",
               "60-65"], ["0-71", "36-40"], ["0-71", "25-26"], ["0-71",
               "18-20"], ["0-71", "50-55"], ["0-71", "46-50"], ["0-71",
               "21-22"], ["0-71", "16-17"], ["0-71", "17-18"], ["0-71",
               "56-60"], ["0-71", "9-10"], ["0-71", "13-14"], ["0-71",
               "15-16"], ["0-71", "20-21"], ["0-71", "26-30"], ["0-71",
               "2-3"], ["0-71", "0-1"], ["0-71", "11-12"], ["0-71", "24-25"],
               ["0-71", "1-2"], ["0-71", "45-46"], ["0-71", "5-6"], ["0-71",
               "31-35"], ["0-71", "55-56"], ["0-71", "6-7"], ["0-71", "14-15"],
               ["0-71", "40-45"], ["0-71", "22-24"], ["0-71", "7-8"], ["0-71",
               "4-5"], ["0-71", "30-31"], ["0-71", "35-36"], ["0-71", "12-13"],
               ["0-71", "8-9"], ["0-71", "70-71"], ["0-71", "66-70"], ["0-71",
               "3-4"], ["0-71", "65-66"]]]"""
    return PartitionGraph.from_json(txt)

def age2():
    return IntervalGraph(age_ranges)

def age3():
    return SetGraph(sets)

def marital_status():
    txt = """[["Living with someone as couple", "Other", "partner",
               "widowed/surviving civil partnership", "Civil Partnership but separated",
               "Civil Partnership", "Common Law", "divorced/separated",
               "married/living as couple/civil partnership",
               "Civil Partnership but dissolved", "Civil Partnership but widowed",
               "Separated", "Divorced", "Married", "Widowed",
               "divorced/disolved civil partnership", "Not re-married",
               "divorced/disolved/separated", "Single", "married/civil partnership",
               "Re-married", "single not divorced"], [["partner", "Common Law"],
               ["partner", "Living with someone as couple"],
               ["widowed/surviving civil partnership", "Civil Partnership but widowed"],
               ["widowed/surviving civil partnership", "Widowed"], ["Civil Partnership",
               "Civil Partnership but widowed"], ["Civil Partnership",
               "Civil Partnership but dissolved"], ["Civil Partnership",
               "Civil Partnership but separated"], ["divorced/separated", "Separated"],
               ["divorced/separated", "Divorced"],
               ["married/living as couple/civil partnership",
               "Living with someone as couple"],
               ["married/living as couple/civil partnership", "married/civil partnership"],
               ["Married", "Re-married"], ["Married", "Not re-married"],
               ["divorced/disolved civil partnership", "Civil Partnership but dissolved"],
               ["divorced/disolved civil partnership", "Divorced"],
               ["divorced/disolved/separated", "Civil Partnership but dissolved"],
               ["divorced/disolved/separated", "divorced/separated"],
               ["Single", "single not divorced"], ["Single", "Divorced"],
               ["married/civil partnership", "Civil Partnership"],
               ["married/civil partnership", "Married"]]]"""
    return PartitionGraph.from_json(txt)

def test(g):
    # fairly comprehensive test (although randomized)
    import random
    if hasattr(g, 'is_valid'):
        is_valid = g.is_valid
    else:
        is_valid = g.structure_is_valid
    assert is_valid()
    g.flatten()
    g.deepen()
    assert is_valid()
    assert g == g.__class__.from_json(g.to_json())
    samp = random.sample(g.nodes, g.num_nodes//4)
    for n in samp:
        g.discard(n)
    g.flatten()
    g.deepen()
    assert is_valid()
    samp = random.sample(g.nodes, g.num_nodes//2)
    g.prune(samp, aggressive=True)
    assert is_valid()
    g.flatten()
    g.deepen()
    assert is_valid()
    

if __name__ == "__main__":
    for g in [age(), age2(), age3(), marital_status()]:
        test(g)
