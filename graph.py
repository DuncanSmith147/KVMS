
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


class GraphError(Exception): pass


class Graph(object):
    """
    A basic class for simple graphs.  A graph consists of nodes and edges.  Nodes
    can be any hashable type, and edges are tuples of nodes.

    Methods contain basic necessary functionality, although this might
    not be considered complete for some applications.  e.g. Deleting a node
    does not delete incident edges.

    Subclasses can use alternative edge representations, such as adjacency lists, if
    get_number_of_edges(), add_edge(), del_edge(), has_edge(), del_edges(), and iter_edges() are
    suitably overridden.  is_subgraph() would also need to be overridden for the set
    operations on graphs.
    """
    def __init__(self):
        """
        Initialises an empty graph.  A graph consists of a set of
        nodes and a set of edges.
        """
        self.nodes = set()
        self.edges = set()

    def __contains__(self, item):
        return self.has_node(item) or self.has_edge(item)

    def accept(self, visitor):
        """
        @type  visitor: an arbitrary object that has a I{visit()} method
        @param visitor: an object containing algorithmic code
        @rtype:         whatever type is returned by I{visitor.visit(self)}
        @return:        return value of I{visitor.visit(self)}
        """
        return visitor.visit(self)

    def get_number_of_nodes(self):
        """
        @rtype:  C{int}
        @return: number of nodes in graph
        """
        return len(self.nodes)
    def set_number_of_nodes(self, value):
        """
        @raise AttributeError: in all cases (read-only attribute)
        """
        raise AttributeError("cannot set num_nodes")
    num_nodes = property(get_number_of_nodes, set_number_of_nodes, doc="number of nodes in graph")

    def get_number_of_edges(self):
        """
        @rtype:  C{int}
        @return: number of edges in graph
        """
        return len(self.edges)
    def set_number_of_edges(self, value):
        """
        @raise AttributeError: in all cases (read-only attribute)
        """
        raise AttributeError("cannot set num_edges")
    num_edges = property(get_number_of_edges, set_number_of_edges,
                        doc='number of edges in graph, requires iterating over '
                            'the graph edges, unless the edges are in self._edges')

    def __len__(self):
        """
        @rtype:  C{int}
        @return: number of nodes in graph
        @note:   implemented so that bool(g) returns False
                 for an empty graph (no nodes or edges),
                 otherwise True
        """
        return len(self.nodes)

    def copy(self):
        """
        A copy of a graph shares the original node and edge instances.

        @rtype:  same type as graph instance
        @return: graph copy
        """
        acopy = self.__class__()
        for node in self.iter_nodes():
            acopy.add_node(node)
        for edge in self.iter_edges():
            acopy.add_edge(edge)
        return acopy

    __copy__ = copy

    def deepcopy(self):
        """
        A deepcopy of a graph has copies of the original node
        and edge instances.

        @rtype:  same type as graph instance
        @return: graph deepcopy
        """
        acopy = self.__class__()
        for node in self.iter_nodes():
            acopy.add_node(copy.copy(node))
        for edge in self.iter_edges():
            acopy.add_edge(copy.copy(edge))
        return acopy

    __deepcopy__ = deepcopy

    def add_node(self, node):
        """
        Adds a node to the graph.

        @type        node: hashable type
        @param       node: a node
        """
        self.nodes.add(node)

    def del_node(self, node):
        """
        Removes a node, but not incident edges.  Raises an error
        if the node is not in the graph.

        @type        node: hashable type
        @param       node: a node
        @raise ValueError: if node is not in graph
        """
        try:
            self.nodes.remove(node)
        except KeyError:
            raise ValueError('cannot remove node that is not in graph')

    def discard_node(self, node):
        """
        Removes a node, but not incident edges.  Raises an error
        if the node is not in the graph.

        @type        node: hashable type
        @param       node: a node
        @raise ValueError: if node is not in graph
        """
        self.nodes.discard(node)

    def has_node(self, node):
        """
        Tests whether I{node} is in graph.

        @type  node: hashable type
        @param node: a node
        @rtype:      C{bool}
        @return:     True if node is in graph, otherwise False
        """
        try:
            return node in self.nodes
        except:
            return False

    def add_edge(self, edge):
        """
        Adds an edge instance to the graph.  Raises an error if either
        of the edge's nodes are not present in the graph's nodes.

        Must be overridden in subclasses that use alternative edge
        representations (such as adjacency lists).

        @type        edge: C{tuple} of nodes
        @param       edge: an edge
        @rtype:            C{tuple} of nodes
        @return:           corresponding pair of nodes; (head, tail) if edge is directed
        @raise ValueError: if node(s) not in graph
        """
        n1, n2 = edge
        if not (self.has_node(n1) and self.has_node(n2)):
            raise ValueError('node(s) not in graph')
        self.edges.add(edge)
        # return edge as tuple of nodes for the convenience of derived classes
        return n1, n2

    def del_edge(self, edge):
        """
        Removes an edge from the graph. Raises an error if either
        of the edge's nodes are not present in the graph's nodes,
        or if the edge is not present.

        Must be overridden in subclasses that use alternative edge
        representations (such as adjacency lists).

        @type  edge:       C{tuple} of nodes
        @param edge:       an edge
        @rtype:            C{tuple} of nodes
        @return:           corresponding pair of nodes; (head, tail) if edge is directed
        @raise ValueError: if node(s) not in graph
        """
        n1, n2 = edge
        if not (self.has_node(n1) and self.has_node(n2)):
            raise ValueError('node(s) not in graph')
        try:
            self.edges.remove(edge)
        except KeyError:
            raise ValueError('(%s, %s) is not in the graph' % edge)
        # return edge as tuple for the convenience of derived classes
        return n1, n2

    def discard_edge(self, edge):
        """
        Removes an edge from the graph if it is present. Does not raise
        an error if either of the edge's nodes are not present in the
        graph's nodes, or if the edge is not present.

        Must be overridden in subclasses that use alternative edge
        representations (such as adjacency lists).

        @type  edge:       C{tuple} of nodes
        @param edge:       an edge
        @rtype:            C{tuple} of nodes
        @return:           corresponding pair of nodes; (head, tail) if edge is directed
        @raise ValueError: if node(s) not in graph
        """
        self.edges.discard(edge)
        # return edge as tuple for the convenience of derived classes
        return edge[0], edge[1]

    def has_edge(self, edge):
        """
        Tests whether I{edge} is in graph.

        Must be overridden in subclasses that use alternative edge
        representations (such as adjacency lists).
        
        @type  edge: C{tuple} of nodes
        @param edge: an edge
        @rtype:      C{bool}
        @return:     True if edge is in graph, otherwise False
        """
        try:
            return edge in self.edges
        except:
            return False

    def del_edges(self):
        """
        Clears all edges (but not nodes) from the graph.
        """
        self.edges.clear()

    def is_subgraph(self, other):
        """
        Returns True if, and only if, all nodes in self and all edges
        in self are in other.

        @type  other: L{Graph}
        @param other: a graph
        @rtype:       C{bool}
        @return:      True if all nodes and edges in graph are in I{other}, otherwise False
        """
        return self.nodes <= other.nodes and self.edges <= other.edges

    def is_supergraph(self, other):
        """
        Returns True if, and only if, all nodes in other and all edges
        in other are in self.

        @type  other: L{Graph}
        @param other: a graph
        @rtype:       C{bool}
        @return:      True if all nodes and edges in I{other} are in graph, otherwise False
        """
        return other.is_subgraph(self)

    __le__ = is_subgraph
    __ge__ = is_supergraph

    def __lt__(self, other):
        """
        Returns True if, and only if, graph is a proper subgraph of other.

        @type  other: L{Graph}
        @param other: a graph
        @rtype:       C{bool}
        @return:      True if all nodes and edges in I{other} are in graph, otherwise False
        """
        return self.num_nodes + self.num_edges < other.num_nodes + other.num_edges and self <= other

    def __gt__(self, other):
        """
        Returns True if, and only if, other is a proper subgraph of graph.

        @type  other: L{Graph}
        @param other: a graph
        @rtype:       C{bool}
        @return:      True if all nodes and edges in I{other} are in graph, otherwise False
        """
        return self.num_nodes + self.num_edges > other.num_nodes + other.num_edges and self >= other

    def __eq__(self, other):
        """
        Returns True if, and only if, graph is equal to other.

        @type  other: L{Graph}
        @param other: a graph
        @rtype:       C{bool}
        @return:      True if all nodes and edges in I{other} are in graph, otherwise False
        """
        return self.num_nodes == other.num_nodes and self.num_edges == other.num_edges and self <= other

    def __ne__(self, other):
        """
        Returns True if, and only if, graph is not equal to other.

        @type  other: L{Graph}
        @param other: a graph
        @rtype:       C{bool}
        @return:      False if all nodes and edges in I{other} are in graph, otherwise True
        """
        return not self == other
    
    def iter_nodes(self):
        """
        Returns an iterator over the nodes.

        @rtype:  C{generator}
        @return: a generator of nodes.  The nodes are
                 generated in an arbitrary order
        """
        return iter(self.nodes)

    def iter_edges(self):
        """
        Returns an iterator over the edges.

        Must be overridden in subclasses that use alternative edge
        representations (such as adjacency lists).

        @rtype:  C{generator}
        @return: a generator of edges.  The edges are
                 generated in an arbitrary order
        """
        return iter(self.edges)


def testGraph():
    data = [('A', []),
            ('B', []),
            ('C', []),
            ('D', ['A', 'B', 'C']),
            ('E', ['C']),
            ('F', ['D', 'E']),
            ('G', [])]  # note: edges need only be contained in one data item
    g = Graph()
    for node, neighbours in data:
        g.add_node(node)
    for node, neighbours in data:
        for n in neighbours:
            g.add_edge((node, n))
    return g
