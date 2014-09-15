
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


"""Tree / graph traversal and other related algorithms"""

from __future__ import division

from collections import defaultdict


class CycleError(Exception): pass


def bfs(graph, root=None, pre=False, hops=False, edges=False, adj=None, sentinels=None, bool_func=None):
    """
    Returns a breadth first search generator for I{graph}.

    If I{root} is C{None} the search starts at an arbitrary
    node and continues until all graph nodes are visited.
    Depending on the boolean arguments the generator might
    yield all, or none of the nodes in visited order, the
    shortest distances (or hops) from the root node, and /
    or the visited graph edges.  Hops are yielded immediately
    after the relevant node.

    @type      graph: L{undirected<udgraph.UndirectedGraph>} or
                      L{directed<digraph.DirectedGraph>} graph
    @param     graph: graph
    @type       root: hashable type
    @param      root: root node for search.  If None the search
                      starts at an arbitrary node and continues
                      until all graph nodes are visited
    @type        pre: C{bool}
    @param       pre: if True, yields nodes in visited order
    @type       hops: C{bool}
    @param      hops: if True, yields the shortest distances from
                      the root to the generated node
    @type      edges: C{bool}
    @param     edges: if True, yields edges of BFS tree
    @type        adj: C{dict} or None
    @param       adj: mapping of nodes to sets of 'nearest reachable'
                      nodes.  If no argument is provided graph.neighbours
                      will be used for undirected graphs, or graph.children
                      for directed graphs
    @type  sentinels: a container of nodes, or None
    @param sentinels: sentinel nodes
    @type  bool_func: function, or None
    @param bool_func: a function that takes a node as argument and returns
                      I{True} if the node should by yielded, I{False} if
                      the node should be treated as a sentinel
    @rtype:           C{generator}
    @return:          a generator of nodes / edges in breadth first search order
    @note:            as calls to I{bool_func} might have side-effects it is ensured
                      that, if defined, it is called exactly once for each node.
                      I{bool_func} is never called with nodes in I{sentinels} as
                      argument
    """
    if adj is None:
        try:
            adj = graph.neighbours
        except AttributeError:
            adj = graph.children
    if root is None:
        roots = set(graph.nodes)
    else:
        roots = set([root])
    if sentinels:
        visited = set(sentinels)
    else:
        visited = set()
    def bfs(node):
        visited.add(node)
        if bool_func and not bool_func(node):
            raise StopIteration
        nodes = [(node, 0)]
        for node, hop in nodes:
            if pre:
                yield node
            if hops:
                yield hop
            for n in adj[node]:
                if n in visited:
                    continue
                elif bool_func and not bool_func(n):
                    continue
                nodes.append((n, hop + 1))
                if edges:
                    yield node, n
            visited.update(adj[node])
    for root in roots:
        if not root in visited:
            for item in bfs(root):
                yield item

def dfs(graph, root=None, pre=False, post=False, edges=False, adj=None, sentinels=None, bool_func=None):
    """
    Returns a depth first search generator for I{graph}.

    If I{root} is C{None} the search starts at an arbitrary
    node and continues until all graph nodes are visited.
    Depending on the boolean arguments the generator might
    yield all, or none of the nodes in visited order, finished
    order, and / or the visited graph edges.

    For example, if all the boolean arguments except I{edges}
    are True then each node would be yielded twice, their order
    corresponding to the DFS I{time-stamp} structure
    (U{http://www.cs.ust.hk/faculty/golin/COMP271Sp03/Notes/MyL08.pdf}).

    @type      graph: L{undirected<udgraph.UndirectedGraph>} or
                      L{directed<digraph.DirectedGraph>} graph
    @param     graph: graph
    @type       root: hashable type
    @param      root: root node for search.  If None the search
                      starts at an arbitrary node and continues
                      until all graph nodes are visited
    @type        pre: C{bool}
    @param       pre: if True, yields nodes in visited order
    @type       post: C{bool}
    @param      post: if True, yields nodes in finished order
    @type      edges: C{bool}
    @param     edges: if True, yields edges of DFS tree
    @type        adj: C{dict} or None
    @param       adj: mapping of nodes to sets of 'nearest reachable'
                      nodes.  If no argument is provided graph.neighbours
                      will be used for undirected graphs, or graph.children
                      for directed graphs
    @type  sentinels: a container of nodes, or None
    @param sentinels: sentinel nodes
    @type  bool_func: function, or None
    @param bool_func: a function that takes a node as argument and returns
                      I{True} if the node should by yielded, I{False} if
                      the node should be treated as a sentinel
    @rtype:           C{generator}
    @return:          a generator of nodes / edges in depth first search order
    @note:            as calls to bool_func might have side-effects it is ensured
                      that, if defined, it is called exactly once for each node.
                      I{bool_func} is never called with nodes in I{sentinels} as
                      argument
    """
    if adj is None:
        try:
            adj = graph.neighbours
        except AttributeError:
            adj = graph.children
    if root is None:
        roots = set(graph.nodes)
    else:
        roots = set([root])
    if sentinels:
        visited = set(sentinels)
    else:
        visited = set()
    def dfs(node, yield_it=None):
        visited.add(node)
        if yield_it is False:
            raise StopIteration
        elif yield_it is None:
            if bool_func and not bool_func(node):
                raise StopIteration
        if pre:
            yield node
        for adj_node in adj[node]:
            if not adj_node in visited:
                yield_it = None
                if edges:
                    if bool_func:
                        yield_it = bool_func(adj_node)
                        if yield_it:
                            yield node, adj_node
                    else:
                        yield node, adj_node
                for n in dfs(adj_node, yield_it):
                    yield n
        if post:
            yield node
    for root in roots:
        if not root in visited:
            for item in dfs(root):
                yield item

def topological_sort(graph):
    """
    Generates the nodes of the graph in a topological ordering.

    An alternative is the reverse order of the nodes in a
    postorder dfs.

    @type  graph:      L{directed graph<digraph.DirectedGraph>}
    @param graph:      a directed graph
    @rtype:            C{generator}
    @return:           a generator of nodes in topological order
    @raise CycleError: if the graph contains a cycle
    """
    try:
        queue = list(graph.root_nodes)
    except AttributeError:
        queue = [node for node in graph.nodes if graph.indegree(node) == 0]
    nodes = graph.nodes.difference(queue)
    indegrees = {}  # a dictionary to hold 'temporary' inDegrees
    for node in nodes:
        indegrees[node] = graph.indegree(node)
    for node in queue:
        yield node
        for child in graph.children[node]:
            indegrees[child] -= 1
            if indegrees[child] == 0:
                del indegrees[child]
                queue.append(child)
    if indegrees:
        raise CycleError('the graph contains a cycle')

def kruskal(graph, weighted_edges):
    """
    Returns a generator of the edges of a minimal spanning
    tree of the weighted undirected I{graph} with weights
    and edges in I{weighted_edges}.

    @type           graph: L{undirected<udgraph.UndirectedGraph>} or
                           L{directed<digraph.DirectedGraph>} graph
    @param          graph: graph
    @type  weighted_edges: iterable of C{tuple}s
    @param weighted_edges: an iterable of (weight, edge) tuples,
                           an edge being a tuple of nodes
    @rtype:                generator
    @return:               generator of minimal spanning tree
                           edges
    """
    from union_find import UnionFindTree
    weighted_edges = list(weighted_edges)
    weighted_edges.sort()
    uf = UnionFindTree()
    for node in graph.iterNodes():
        uf.add(node)
    for weight, edge in weighted_edges:
        if uf.union(*edge):
            yield edge
        if uf.size == 1:
            raise StopIteration

def reduction_closure(graph):
    # for a DAG
    # returns a pair of mappings of nodes to children
    # the first contains the edges that must be
    # removed from graph to produce its transitive reduction
    # the second contains the edges that are
    # contained in the transitive closure (including existing graph edges)
    # The transitive reduction is the unique (in the case of a DAG)
    # smallest subgraph (with the same node set) that has the same
    # transitive closure.
    # The transitive closure is the graph produced by adding edges
    # from each node to each of its descendants
    reduction = {}
    closure = {}
    for v in dfs(graph, post=True):
        descendants = set()
        to_remove = set()
        children = graph.children[v]
        for child in children:
            deleted = False
            # check for membership in other children
            for other in children.difference([child]) - to_remove:
                if child in closure[other]:
                    to_remove.add(child)
                    deleted = True
                    break
            if not deleted:
                descendants |= closure[child]
        descendants |= children
        reduction[v] = to_remove
        closure[v] = descendants
    return reduction, closure

def maintain_reduction_add_node(graph, node, func):
    # maintains a transitive reduction on node addition
    # func takes a pair of nodes v, w as arguments and
    # returns True if relation holds for (v,w), False if
    # relation holds for (w,v), and None otherwise
    leaves = set()
    roots = set()
    nodes = set(graph.nodes)
    visited = set()
    while nodes:
        x = nodes.pop()
        val = func(x, node)
        if val:
            leaves.add(x)
            search = dfs(graph, x, pre=True, adj=graph.parents, sentinels=visited) # unvisited ancestors
            next(search) # don't want to yield x
            for n in search:
                visited.add(n)
                nodes.discard(n) # might have been added to leaves and not visited
        elif val == False:
            roots.add(x)
            search = dfs(graph, x, pre=True, sentinels=visited) # unvisited descendants
            next(search)
            for n in search:
                visited.add(n)
                nodes.discard(n)
    leaves = leaves - visited
    roots = roots - visited
    # update graph
    graph.addNode(node)
    for leaf in leaves:
        graph.addEdge((leaf, node))
    for root in roots:
        graph.addEdge((node, root))
    for leaf in leaves:
        for root in roots:
            graph.discardEdge((leaf, root))

###############################


def test_add_node(n):
    # generate some random strings
    import random
    import digraph
    letters = 'abcdefghijklmnopqrstuvwxyz'
    strings = []
    for _ in range(n):
        strings.append(''.join(random.sample(letters, 6)))
    g = digraph.DirectedGraph()
    def func(v,w):
        if v[0] == w[0]:
            return None
        return v[0] < w[0]
    for s in strings:
        try:
            maintain_reduction_add_node(g, s, func)
        except:
            print
            print 'strings', strings
            print
            raise
    to_file(g, '/home/duncan/add_node.png')
    return g

def test_add_node2(n):
    # generate some random strings
    import random
    import digraph
    letters = 'abcdefghijklm'
    strings = []
    for _ in range(n):
        samp_size = random.randint(1, 10)
        strings.append(''.join(sorted(random.sample(letters, samp_size))))
    g = digraph.DirectedGraph()
    def func(v,w):
        if set(v).issubset(set(w)) and len(v) < len(w):
            return True
        elif set(v).issuperset(set(w)) and len(v) > len(w):
            return False
        return None
    for s in strings:
        try:
            maintain_reduction_add_node(g, s, func)
        except:
            print
            print 'strings', strings
            print
            raise
    to_file(g, '/home/duncan/add_node_2.png')
    return g

def to_file(g, filename, directed=True):
    import dot
    import digraph
    dot.graph2image(g, filename, directed, filename[-3:])
