
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

import copy

import graph


class DirectedGraphError(Exception): pass


class DirectedGraph(graph.Graph):
    """
    A basic class for directed graphs.  Multiple edges between
    nodes are not supported, although self loops are.  Nodes,
    parent nodes and child nodes are contained in I{self.nodes},
    I{self.parents} and I{self.children}.  A node may be any
    hashable type.  An edge is a tuple (or list) of nodes, (tail, head).

    Example:
        >>> import digraph
        >>> edge = (1, 2)
        >>> g = digraph.DirectedGraph()
        >>> g.add_node(node1)
        >>> g.add_node(node2)
        >>> g.add_edge(edge)
        (1, 2)
        >>> g
        1 []
        2 [1]

    """
    def __init__(self):
        """
        Initialises an empty graph.  A graph consists of a set of
        nodes and parents and children dictionaries containing adjacency
        information.
        """
        #graph.Graph.__init__(self)
        self.nodes = set()
        self.parents = {}
        self.children = {}

    def __repr__(self):
        """
        User friendly graph representation.

        @rtype:  C{str}
        @return: C{str}
        """
        s = ''
        for node in self.nodes:
            s += str(node) + ' ' + repr([parent for parent in self.parents[node]]) + '\n'
        return s

    def get_edges(self):
        """
        @rtype:  C{list}
        @return: list of graph edges
        """
        return list(self.iterEdges())
    def set_edges(self, value):
        """
        @raise AttributeError: in all cases (read-only attribute)
        """
        raise AttributeError("use addEdge method")
    edges = property(get_edges, set_edges, doc="the graph edges")

    def get_number_of_edges(self):
        """
        @rtype:  C{int}
        @return: number of edges in graph
        """
        return sum(self.indegree(node) for node in self.iter_nodes())
    def set_number_of_edges(self, value):
        """
        @raise AttributeError: in all cases (read-only attribute)
        """
        raise AttributeError("cannot set number of edges")
    num_edges = property(get_number_of_edges, set_number_of_edges, doc="number of edges in graph")

    def indegree(self, node):
        """
        Returns the indegree (number of parents) of I{node}.
        
        @type  node: hashable type
        @param node: graph node
        @rtype:      C{int}
        @return:     node degree
        """
        return len(self.parents[node])

    def outdegree(self, node):
        """
        Returns the outdegree (number of children) of I{node}.
        
        @type  node: hashable type
        @param node: graph node
        @rtype:      C{int}
        @return:     node degree
        """
        return len(self.children[node])

    def is_root(self, node):
        """
        Tests whether I{node} has any parents.
        
        @type  node: hashable type
        @param node: graph node
        @rtype:      C{bool}
        @return:     True if node has no parents, otherwise False
        """
        return len(self.parents[node]) == 0

    def is_leaf(self, node):
        """
        Tests whether I{node} has any children.
        
        @type  node: hashable type
        @param node: graph node
        @rtype:      C{bool}
        @return:     True if node has no children, otherwise False
        """
        return len(self.children[node]) == 0

    def deepcopy(self):
        """
        A deepcopy of a graph has copies of the original node
        instances.

        @rtype:  same type as graph instance
        @return: graph deepcopy
        """
        copy_map = dict((node, copy.copy(node)) for node in self.nodes)
        acopy = self.__class__()
        for n in copy_map.values():
            acopy.addNode(n)
        for v, w in self.iter_edges():
            acopy.add_edge((copy_map[v], copy_map[w]))
        return acopy

    __deepcopy__ = deepcopy

    def add_node(self, node):
        """
        Adds a node to the graph.

        @type        node: hashable type
        @param       node: a node
        """
        super(DirectedGraph, self).add_node(node)
        if not node in self.parents:
            # if node has been previously added
            # we want a null operation
            self.parents[node] = set()
            self.children[node] = set()

    def del_node(self, node):
        """
        Removes a node and all incident edges.  Raises an error
        if the node is not in the graph.

        @type        node: hashable type
        @param       node: a node
        @raise ValueError: if node is not in graph
        """
        super(DirectedGraph, self).del_node(node)
        # remove edges
        for parent in list(self.parents[node]):
            self.children[parent].remove(node)
        for child in list(self.children[node]):
            self.parents[child].remove(node)
        del self.parents[node]
        del self.children[node]

    def discard_node(self, node):
        """
        Removes a node and all incident edges if the node
        is present.  Does not raise an error
        if the node is not in the graph.

        @type        node: hashable type
        @param       node: a node
        @raise ValueError: if node is not in graph
        """
        try:
            self.del_node(node)
        except ValueError:
            pass

    def add_edge(self, edge):
        """
        Adds an edge to the graph.
        
        An exception is raised if one or both edge nodes
        is not in the graph.

        @type  edge:       C{tuple} of nodes
        @param edge:       an edge, or pair of nodes
        @rtype:            C{tuple} of nodes
        @return:           a pair of nodes
        @raise ValueError: if node(s) not in graph
        """
        p, c = edge
        if not (self.has_node(p) and self.has_node(c)):
            raise ValueError('node(s) not in graph')
        self.children[p].add(c)
        self.parents[c].add(p)
        # return edge as tuple for the convenience of derived classes
        return p, c

    def del_edge(self, edge):
        """
        Removes an edge from the graph.
        
        An exception is raised if the edge
        is not in the graph.

        @type  edge:       C{tuple} of nodes
        @param edge:       a pair of nodes
        @rtype:            C{tuple} of nodes
        @return:           a pair of nodes
        @raise ValueError: if node(s) not in graph
        """
        p, c = edge
        if not (self.has_node(p) and self.has_node(c)):
            raise ValueError('node(s) not in graph')
        try:
            self.children[p].remove(c)
            self.parents[c].remove(p)
        except KeyError:
            raise ValueError('(%s, %s) is not in the graph' % edge)
        # return edge as tuple for the convenience of derived classes
        return p, c

    def discard_edge(self, edge):
        """
        Removes an edge from the graph if it is present.
        
        No exception is raised if one or both edge nodes
        is not in the graph, or if the edge is not present.

        @type  edge:       C{tuple} of nodes
        @param edge:       a pair of nodes
        @rtype:            C{tuple} of nodes
        @return:           a pair of nodes
        @raise ValueError: if node(s) not in graph
        """
        p, c = edge
        try:
            self.children[p].remove(c)
            self.parents[c].remove(p)
        except KeyError:
            pass
        # return edge as tuple for the convenience of derived classes
        return p, c

    def has_edge(self, edge):
        """
        Tests whether I{edge} is in graph.
        
        @type  edge: C{tuple} of nodes
        @param edge: a pair of nodes
        @rtype:      C{bool}
        @return:     True if edge is in graph, otherwise False
        """
        try:
            return edge[0] in self.parents[edge[1]]
        except:
            return False

    def del_edges(self):
        """
        Clears all edges (but not nodes) from the graph.
        """
        for node in self.iterNodes():
            self.parents[node] = set()
            self.children[node] = set()

    def is_subgraph(self, other):
        """
        Returns True if, and only if, all nodes in self and all edges
        in graph are in other.

        @type  other: L{DirectedGraph}
        @param other: a graph
        @rtype:       C{bool}
        @return:      True if all nodes and edges in graph are in I{other}, otherwise False
        """
        if not self.nodes <= set(other.nodes):
            return False
        for node in self.iter_nodes():
            if not self.children[node] <= set(other.children[node]):
                return False
        return True

    def __eq__(self, other):
        """
        Returns True if, and only if, graph is equal to other.

        @type  other: L{DirectedGraph}
        @param other: a graph
        @rtype:       C{bool}
        @return:      True if all nodes and edges in I{other} are in graph, otherwise False
        """
        if self.nodes == set(other.nodes) and self.num_edges == other.num_edges:
            for node in self.iter_nodes():
                if self.children[node] != set(other.children[node]):
                    return False
            return True
        return False

    def graph_sum(self, *others):
        """
        Returns a new graph containing the union of the edge sets
        of I{self} and the graphs in I{others}.  All graphs must have equal
        node sets.

        @type              others: iterable containing L{DirectedGraph} instances
        @param             others: directed graphs
        @rtype:                    same type as graph
        @return:                   sum of graph and graphs in I{others}
        @raise DirectedGraphError: if not all graphs have equal node sets
        """
        sum_ = self.copy()
        for other in others:
            if not sum_.nodes == set(other.nodes):
                raise DirectedGraphError('graph vertex sets must be equal')
            for edge in other.iter_edges():
                sum_.add_edge(edge)
        return sum_

    def graph_union(self, *others):
        """
        Returns a graph containing the union of the node sets and edge
        sets of I{self} and the graphs in I{others}.  All graphs must have
        distinct node sets (and edge sets).

        @type              others: iterable containing L{DirectedGraph} instances
        @param             others: directed graphs
        @rtype:                    same type as graph
        @return:                   union of graph and graphs in I{others}
        @raise DirectedGraphError: if any graphs share any nodes
        """
        union = self.copy()
        for other in others:
            if self.nodes.intersection(other.nodes):
                raise DirectedGraphError('graph vertex sets must be distinct')
            for node in other.iter_nodes():
                union.add_node(node)
            for edge in other.iter_edges():
                union.add_edge(edge)
        return union
        
    def graph_difference(self, *others):
        """
        Returns a graph containing the set difference of the edges sets
        of graph and the graphs in I{others}.  All graphs must have equal
        node sets.

        @type              others: iterable containing L{DirectedGraph} instances
        @param             others: directed graphs
        @rtype:                    same type as graph
        @return:                   difference of graph and graphs in I{others}
        @raise DirectedGraphError: if not all graphs have equal node sets
        """
        diff = self.copy()
        for other in others:
            if not diff.nodes == set(other.nodes):
                raise DirectedGraphError('graph vertex sets must be equal')
            for edge in other.iter_edges():
                diff.discard_edge(edge)
        return diff

    def weak_components(self):
        """
        Generates sets containing the sets of nodes in each weakly
        connected component (U{http://mathworld.wolfram.com/WeaklyConnectedComponent.html}).

        @rtype:  C{generator}
        @return: a generator sets of nodes for each weakly connected component
        """
        import traversals
        # use a bfs that ignores directions on edges
        unvisited = self.nodes.copy()
        adj = self.children.copy()
        for node in self.nodes:
            adj[node] |= self.parents[node]
        while unvisited:
            component = set(traversals.bfs(self, unvisited.pop(), pre=True, adj=adj))
            unvisited -= component
            yield component

    components = weak_components

    def strong_components(self):
        """
        Generates sets containing the sets of nodes in each strongly
        connected component (U{http://mathworld.wolfram.com/StronglyConnectedComponent.html}).

        @rtype:  C{generator}
        @return: a generator of sets of nodes for each strongly connected component
        """
        import traversals
        visited = set()
        order = list(traversals.dfs(self, post=True))
        top_order = reversed(order)
        for node in top_order:
            if not node in visited:
                component = set()
                for node in traversals.dfs(self, node, pre=True, adj=self.parents):
                    if not node in visited:
                        component.add(node)
                        visited.add(node)
                yield component

    def transposed(self):
        """
        Returns the transpose of the graph.  The
        transpose of a graph, I{G}, contains the
        same nodes as I{G}, but with the directions
        on the edges reversed.

        @rtype:  same type as graph
        @return: the transposed graph
        """
        transposed = self.__class__()
        for node in self.nodes:
            transposed.add_node(node)
        for edge in self.iter_edges():
            transposed.add_edge((edge[1], edge[0]))
        return transposed

    def induced_graph(self, nodes):
        """
        Returns the graph induced by the nodes in I{nodes}.  The
        induced graph contains all nodes in I{nodes} and exactly
        those edges in the original graph between nodes in I{nodes}.

        precondition: all nodes in self

        @type  nodes: iterable containing nodes
        @param nodes: nodes of induced graph
        @rtype:       same type as graph
        @return:      graph induced by I{nodes}
        """
        g = self.__class__()
        for node in nodes:
            g.add_node(node)
        for node in g.nodes:
            for c in self.children[node]:
                if g.has_node(c):
                    g.add_edge((node, c))
        return g
    
    def iter_edges(self):
        """
        Returns an iterator over the graph edges.

        @rtype:  C{generator}
        @return: a generator of edges.  The edges are
                 generated in an arbitrary order
        """
        for node in self.iter_nodes():
            for c in self.children[node]:
                yield node, c

    def to_forest(self):
        """
        Returns a L{Forest} of graphs.

        @rtype:  L{Forest}
        @return: a forest containing (weakly) disconnected graph components
        """
        from forest import Forest
        graphs = [self.induced_graph(component) for component in self.weak_components()]
        return Forest(graphs, 'iter_nodes')


class DirectedGraph2(DirectedGraph):
    """
    A directed graph which keeps track of its roots and leaves.
    """
    def __init__(self):
        """
        Creates an empty digraph.  The graph keeps a record of its
        root and leaf nodes.  i.e. Nodes with no parents / no children
        respectively.
        """
        DirectedGraph.__init__(self)
        # root and leaf nodes are updated when
        # nodes / edges are added / deleted
        self.root_nodes = set()
        self.leaf_nodes = set()

    def copy(self):
        """
        A copy of a graph shares the original node and edge instances.

        @rtype:  same type as graph
        @return: graph copy
        """
        acopy = super(DirectedGraph2, self).copy()
        acopy.root_nodes = self.root_nodes.copy()
        acopy.leaf_nodes = self.leaf_nodes.copy()
        return acopy
        
    def add_node(self, node):
        """
        Adds a node to the graph.

        @type        node: hashable type
        @param       node: a node
        """
        super(DirectedGraph2, self).add_node(node)
        # until edges are added a node is both root and leaf
        if self.outdegree(node) == 0:
            self.leaf_nodes.add(node)
        if self.indegree(node) == 0:
            self.root_nodes.add(node)

    def del_node(self, node):
        """
        Removes a node, but not incident edges.  Raises an error
        if the node is not in the graph.

        @type        node: hashable type
        @param       node: a node
        @raise ValueError: if node is not in graph
        """
        # get copy of parents and children to update root and leaf nodes
        parents = self.parents[node].copy()
        children = self.children[node].copy()
        # remove the node
        super(DirectedGraph2, self).del_node(node)
        # update root and leaf nodes
        self.root_nodes.discard(node)
        self.leaf_nodes.discard(node)
        for node in parents:
            if self.outdegree(node) == 0:
                self.leaf_nodes.add(node)
        for node in children:
            if self.indegree(node) == 0:
                self.root_nodes.add(node)

    def add_edge(self, edge):
        """
        Adds an edge to the graph.
        
        I{edge} is a tuple containing two nodes.  An exception
        is raised if one or both edge nodes is not in the graph.

        @type        edge: C{tuple} of nodes
        @param       edge: an pair of nodes
        @rtype:            C{tuple} of nodes
        @return:           a pair of nodes
        @raise ValueError: if node(s) not in graph
        """
        p, c = super(DirectedGraph2, self).add_edge(edge)
        self.root_nodes.discard(c)
        self.leaf_nodes.discard(p)
        return edge

    def del_edge(self, edge):
        """
        Removes an edge from the graph.
        
        I{edge} is a tuple containing two nodes.  An exception
        is raised if the edge is not in the graph.

        @type        edge: C{tuple} of nodes
        @param       edge: a pair of nodes
        @rtype:            C{tuple} of nodes
        @return:           pair of nodes
        @raise ValueError: if edge is not in graph
        """
        p, c = super(DirectedGraph2, self).del_edge(edge)
        if self.outdegree(p) == 0:
            self.leaf_nodes.add(p)
        if self.indegree(c) == 0:
            self.root_nodes.add(c)

