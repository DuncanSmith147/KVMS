
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

import tempfile
import os


class DotElement(object):
    def __init__(self, **attribs):
        self.attribs = attribs # distinct namespace for dot attributes

    def get_attrib(self, attrib):
        """
        Returns the value of the dot attribute
        I{attrib}.

        @type  attrib: C{str}
        @param attrib: dot attribute
        @rtype:        C{str}
        @return:       attribute value
        """
        return self.attribs[attrib]

    def set_attrib(self, attrib, val):
        """
        Sets the value of the dot attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: dot attribute
        @type     val: C{str}
        @param    val: attribute value
        """
        self.attribs[attrib] = val

    def del_attrib(self, attrib):
        """
        Deletes the dot attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: dot attribute
        """
        del self.attribs[attrib]
        
    def has_attrib(self, attrib):
        """
        Tests whether the node has attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: dot attribute
        @rtype:        C{bool}
        @return:       True if I{attrib} is an
                       attribute of node instance,
                       otherwise False
        """
        return attrib in self.attribs


class DotNode(DotElement):
    """
    A class for a 'dot' graph node.  It contains a name
    and an arbitrary number of named attributes.
    """
    def __init__(self, name, **attribs):
        """
        A node must have a distinct (from any other
        nodes in the same graph) I{name}.  Attributes
        can be any valid dot attributes.

        @type  name: C{str}
        @param name: node name
        """
        self.name = name
        super(DotNode, self).__init__(**attribs)

    def _name_as_string(self):
        """
        Returns the name as an appropriate string;
        enclosed in quotes if the name contains spaces.

        @rtype:  C{str}
        @return: string representation of node name
        """
        return '"%s"' % self.name

    def __str__(self):
        """
        Returns a string representation of the node in a
        format suitable for constructing a dot file.

        @rtype:  C{str}
        @return: dot representation of node
        """
        s = self._name_as_string()
        if self.attribs:
            atts = ','.join(['%s=%s' % (key, (val or '""')) for key, val in self.attribs.iteritems()])
            s = s + ' [' + atts + ']'
        s = s + ';'
        return s

    def copy(self):
        """
        Returns a copy of the node.  A copy does not share the
        same I{attribs} dictionary.

        @rtype:  L{Node}
        @return: copy of node
        """
        return self.__class__(self.name, **self.attribs)

    __copy__ = copy


class DotEdge(DotElement):
    """
    A class for a 'dot' graph edge.  An edge contains
    the objects it connects, a boolean indicating if it is
    directed, and an arbitrary number of attributes.
    """
    def __init__(self, node1, node2, is_directed=False, **attribs):
        """
        @type        node1: L{DotNode} or L{DotSubgraph}
        @param       node1: dot node / subgraph
        @type        node2: L{DotNode} or L{DotSubgraph}
        @param       node2: dot node / subgraph
        @type  is_directed: C{bool}
        @param is_directed: C{bool}
        """
        self.node1 = node1
        self.node2 = node2
        self.is_directed = is_directed
        super(DotEdge, self).__init__(**attribs)

    def __str__(self):
        """
        Returns a string representation of the edge in a
        format suitable for constructing a dot file.

        @rtype:  C{str}
        @return: dot representation of edge
        """
        if self.is_directed:
            s = '%s -> %s' % (self.node1._name_as_string(), self.node2._name_as_string())
        else:
            s = '%s -- %s' % (self.node1._name_as_string(), self.node2._name_as_string())
        if self.attribs:
            atts = ','.join(['%s=%s' % (key, (val or '""')) for key, val in self.attribs.iteritems()])
            s = s + ' [' + atts + ']'
        s = s + ';'
        return s
    
    def copy(self):
        """
        Returns a copy of the edge.  A copy shares the same
        node instances, but not the same I{attribs} dictionary.

        @rtype:  L{Edge}
        @return: copy of edge
        """
        return self.__class__(self.node1, self.node2,
                              self.is_directed, **self.attribs)

    __copy__ = copy


class DotSubgraph(DotElement):
    """
    A class for a 'dot' subgraph.  A subgraph can
    contain nodes, edges and an arbitrary set of
    subgraph-level attributes.  A subgraph can
    contain other subgraphs.

    N.B.  Unattached, node, edge and graph attribute assignments
    are added to generated dot files in that particular order.
    They are added before any node, edge or subgraph assignments
    (and are thus always inherited by subgraphs).  They cannot be
    overridden by subsequent (unattached, node, edge and graph)
    assignments (except in subgraphs, where the same restriction
    applies).  Each subsequent node, edge and subgraph must override
    as necessary.

    Note:  DotSubgraphs generated from parsed dot files avoid
    complications by assigning attributes to all nodes and edges
    individually.
    """
    def __init__(self, name, **attribs):
        """
        A subgraph must have a distinct (from any other
        subgraphs in the same graph) I{name}.  Attributes
        can be any valid dot attributes.  The subgraph
        is initially empty (contains no nodes or edges).

        @type  name: C{str} or None
        @param name: node name
        """
        self.name = name
        super(DotSubgraph, self).__init__(**attribs)
        self.node_attribs = {}
        self.edge_attribs = {}
        self.nodes = []
        self.edges = []
        self.subgraphs = []
        self.subgraphs_inherit = True

    def _name_as_string(self):
        """
        Returns the name as an appropriate string;
        enclosed in quotes if the name contains spaces.

        @rtype:  C{str}
        @return: string representation of subgraph name
        """
        return '"%s"' % self.name

    def lines(self):
        """
        Returns a list of the lines in the string
        representation of the subgraph.

        @rtype:  C{list} of C{str}
        @return: representation of subgraph
        """
        if self.name:
            s = ['%s %s {' % ('subgraph', self._name_as_string())]
        else:
            s = ['%s {' % 'subgraph']
        if self.subgraphs_inherit:
            for key, val in self.attribs.iteritems():
                s.append('\t%s=%s;' % (key, (val or '""')))
        if self.node_attribs:
            s.append('\tnode [' + ','.join(['%s=%s' % (key, (val or '""'))
                                            for key, val in self.node_attribs.iteritems()]) + '];')
        if self.edge_attribs:
            s.append('\tedge [' + ','.join(['%s=%s' % (key, (val or '""'))
                                            for key, val in self.edge_attribs.iteritems()]) + '];')
        for subgraph in self.subgraphs:
            s.extend(['\t%s' % line for line in subgraph.lines()])
        if not self.subgraphs_inherit:
            for key, val in self.attribs.iteritems():
                s.append('\t%s=%s;' % (key, (val or '""')))
        for n in self.iter_nodes(False):
            s.append('\t%s' % str(n))
        for e in self.iter_edges(False):
            s.append('\t%s' % str(e))
        s.append('}')
        return s
    
    def __contains__(self, item):
        # does not recurse through subgraphs
        return item in self.nodes or item in self.edges or item in self.subgraphs

    def add_node(self, node):
        """
        Adds a node to the graph.

        @type  node: L{DotNode}
        @param node: a node
        """
        self.nodes.append(node)

    def del_node(self, node):
        """
        Removes a node, but not incident edges.  Raises an error
        if the node is not in the graph.

        @type        node: L{DotNode}
        @param       node: a node
        @raise ValueError: if node is not in graph
        """
        try:
            self.nodes.remove(node)
        except ValueError:
            raise ValueError('cannot remove node that is not in graph')

    def has_node(self, node):
        """
        Tests whether I{node} is in graph.

        @type  node: L{DotNode}
        @param node: a node
        @rtype:      C{bool}
        @return:     True if node is in graph, otherwise False
        """
        return node in self.nodes

    def add_edge(self, edge):
        """
        Adds an edge instance to the graph.

        @type  edge: L{DotEdge}
        @param edge: an edge
        """
        self.edges.append(edge)

    def del_edge(self, edge):
        """
        Removes an edge from the graph. Raises an error
        if the edge is not present.

        @type        edge: L{DotEdge}
        @param       edge: an edge
        @raise ValueError: if edge is not in graph
        """
        try:
            self.edges.remove(edge)
        except ValueError:
            raise ValueError('cannot remove edge that is not in graph')

    def del_edges(self):
        """
        Clears all edges (but not nodes) from the graph.
        """
        self.edges = []

    def has_edge(self, edge):
        """
        Tests whether I{edge} is in the graph.
        
        @type  edge: L{DotEdge}
        @param edge: an edge
        @rtype:      C{bool}
        @return:     True if edge is in graph, otherwise False
        """
        return edge in self.edges

    def add_subgraph(self, subgraph):
        """
        Adds a subgraph instance to the graph.

        @type  subgraph: L{DotSubgraph}
        @param subgraph: a subgraph
        """
        self.subgraphs.append(subgraph)

    def del_subgraph(self, subgraph):
        """
        Deletes I{subgraph} from the graph.

        @type    subgraph: L{DotSubgraph}
        @param   subgraph: a subgraph
        @raise ValueError: if subgraph is not in graph
        """
        try:
            self.subgraphs.remove(subgraph)
        except ValueError:
            raise ValueError('cannot remove subgraph that is not in graph')

    def has_subgraph(self, subgraph):
        """
        Tests whether I{subgraph} is in graph.
        
        @type  subgraph: L{DotSubgraph}
        @param subgraph: a subgraph
        @rtype:          C{bool}
        @return:         True if subgraph is in graph, otherwise False
        """
        return subgraph in self.subgraphs

    def get_node_attrib(self, attrib):
        """
        Returns the value of the node attribute
        I{attrib}.

        @type  attrib: C{str}
        @param attrib: node attribute
        @rtype:        C{str}
        @return:       attribute value
        """
        return self.node_attribs[attrib]

    def set_node_attrib(self, attrib, val):
        """
        Sets the value of the node attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: node attribute
        @type     val: C{str}
        @param    val: attribute value
        """
        self.node_attribs[attrib] = val

    def del_node_attrib(self, attrib):
        """
        Deletes the node attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: node attribute
        """
        del self.node_attribs[attrib]

    def has_node_attrib(self, attrib):
        """
        Tests whether the subgraph has node attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: node attribute
        @rtype:        C{bool}
        @return:       True if I{attrib} is a node
                       attribute of graph instance,
                       otherwise False
        """
        return attrib in self.node_attribs

    def get_edge_attrib(self, attrib):
        """
        Returns the value of the edge attribute
        I{attrib}.

        @type  attrib: C{str}
        @param attrib: edge attribute
        @rtype:        C{str}
        @return:       attribute value
        """
        return self.edge_attribs[attrib]

    def set_edge_attrib(self, attrib, val):
        """
        Sets the value of the edge attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: edge attribute
        @type     val: C{str}
        @param    val: attribute value
        """
        self.edge_attribs[attrib] = val

    def del_edge_attrib(self, attrib):
        """
        Deletes the edge attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: edge attribute
        """
        del self.edge_attribs[attrib]

    def has_edge_attrib(self, attrib):
        """
        Tests whether the subgraph has edge attribute I{attrib}.

        @type  attrib: C{str}
        @param attrib: edge attribute
        @rtype:        C{bool}
        @return:       True if I{attrib} is an edge
                       attribute of graph instance,
                       otherwise False
        """
        return attrib in self.edge_attribs

    def to_graph(self, cls):
        """
        Returns a corresponding graph instance
        of type I{cls}.  It uses node names as
        graph nodes and adds all nodes and edges
        in the instance (including those in subgraphs).

        @type  cls: C{type}
        @param cls: class
        @rtype:     cls
        @return:    graph of type cls
        """
        g = cls()
        for n in self.iter_nodes():
            g.addNode(n.name)
        for e in self.iter_edges():
            g.addEdge((e.node1.name, e.node2.name))
        return g

    def iter_nodes(self, recurse=True):
        """
        Returns an iterator over the nodes.  By default
        recurses through any subgraphs.

        @type  recurse: C{bool}
        @param recurse: if True, recurses through subgraphs
        @rtype:         C{generator}
        @return:        a generator of nodes.  The nodes are
                        generated in a (relatively) arbitrary order
        """
        for node in self.nodes:
            yield node
        if recurse:
            for subgraph in self.subgraphs:
                for node in subgraph.iter_nodes():
                    yield node

    def iter_edges(self, recurse=True):
        """
        Returns an iterator over the edges.  By default
        recurses through any subgraphs.

        @type  recurse: C{bool}
        @param recurse: if True, recurses through subgraphs
        @rtype:         C{generator}
        @return:        a generator of edges.  The edges are
                        generated in a (relatively) arbitrary order
        """
        for edge in self.edges:
            yield edge
        if recurse:
            for subgraph in self.subgraphs:
                for edge in subgraph.iter_edges(True):
                    yield edge


class DotGraph(DotSubgraph):
    """
    A class for a top level 'dot' graph.  It can contain nodes,
    edges and subgraphs.  It must have a I{graph_type} equal
    to 'graph' or 'digraph' for directed and undirected graphs
    respectively.  A graph is I{strict} if self-edges and multiple
    edges are disallowed.  If I{strict} is True, then they are
    subsequently ignored when the image is generated.
    """
    def __init__(self, graph_type, name, strict=False, **attribs):
        """
        @type  graph_type: C{str}
        @param graph_type: 'graph' or 'digraph'
        @type        name: C{str} or None
        @param       name: graph name
        @type      strict: C{bool}
        @param     strict: True if self-arcs and mutiple edges
                           are to be ignored
        """
        super(DotGraph, self).__init__(name, **attribs)
        self.graph_type = graph_type  # 'graph' or 'digraph'
        self.strict = strict

    def __str__(self):
        """
        Returns a string representation of the graph in dot
        format.

        @rtype:  C{str}
        @return: dot representation of graph
        """
        lines = self.lines()
        if self.strict:
            if self.name:
                lines[0] = '%s %s %s {' % ('strict', self.graph_type, self._name_as_string())
            else:
                lines[0] = '%s %s {' % ('strict', self.graph_type)
        else:
            if self.name:
                lines[0] = '%s %s {' % (self.graph_type, self.name)
            else:
                lines[0] = '%s {' % self.graph_type
        return '\n'.join(lines)

    def to_file(self, filename):
        """
        Writes the corresponding dot file to
        a file located at I{filename}.

        @type  filename: C{str}
        @param filename: path to file
        """
        f = open(filename, 'w')
        try:
            f.write(str(self))
        finally:
            f.close()

    def to_image(self, filename, prog='dot', format='dot'):
        """
        Creates an image and saves to file.  The program
        specified as I{prog} is used and the image format
        is that specified by I{format} (see graphviz
        documentation at U{http://www.graphviz.org/doc/}).

        @type  filename: C{str}
        @param filename: path to image file
        @type      prog: C{str}
        @param     prog: program
        @type    format: C{str}
        @param   format: image format
        """
        to_image(str(self), filename, prog, format)

            
def get_dot_path(prog='dot'):
    """
    Searches for the path to the program I{prog} and
    returns the path.  If not found a ValueError
    is raised.

    @type        prog: C{str}
    @param       prog: program
    @raise ValueError: if I{prog} cannot be found in
                       the usual places
    """
    paths = os.environ['PATH'].split(os.pathsep)
    paths = [path for path in paths if
             (os.path.exists(path + os.path.sep + prog)
              or os.path.exists(path + os.path.sep + prog + '.exe'))]
    if len(paths) == 1:
        return paths[0]
    else:
        paths = [path for path in paths if 'graphviz' in path.lower()]
        if not paths:
            # as a last option on Windows check current working directory for exe
            path = os.path.join(os.getcwd(), prog + '.exe')
            if os.path.exists(path):
                paths = [path]
            else:
                # or the parent directory (py2exe)
                path = os.path.join(os.path.split(os.getcwd())[0], prog + '.exe')
                if os.path.exists(path):
                    paths = [path]
        try:
            return paths[0]
        except IndexError:
            raise ValueError('%s program cannot be found' % prog)

def _parse_attrib(text):
    """
    Parses a string of the form 'key=val'and
    returns a key, value pair.

    @type  text: C{str}
    @param text: textual representation of key, value pair
    @rtype:      C{tuple}
    @return:     key, value pair
    """
    key, val = [s.strip() for s in text.split('=')]
    if '.' in val:
        try:
            val = float(val)
        except ValueError:
            pass
    else:
        try:
            val = int(val)
        except ValueError:
            pass
    return key, val

def _parse_attribs(text):
    """
    Parses a string of the form '[key1=val1,key2=val2]'
    and returns a dictionary.

    @type  text: C{str}
    @param text: textual representation of key, value pairs
    @rtype:      C{dict}
    @return:     dictionary of key, value pairs
    """
    attribs = {}
    split_it = 1
    current = []
    for ch in text:
        if ch == '"':
            split_it = 1-split_it
        elif split_it and ch == ',':
            key, val = _parse_attrib(''.join(current))
            attribs[key] = val
            current = []
            continue
        current.append(ch)
    key, val = _parse_attrib(''.join(current))
    attribs[key] = val
    return attribs

def _parse_node(text, defaults):
    """
    Parses a line of dot code containing
    node data and returns a L{DotNode} instance.

    @type      text: C{str}
    @param     text: textual representation of dot node
    @type  defaults: C{dict}
    @param defaults: mapping of (default) attribute keys to values
    @rtype:          L{DotNode}
    @return:         dot node
    """
    data = text.split('[')
    name = data[0].strip().strip('"')
    if len(data) == 1:
        # no attributes set
        return DotNode(name)
    else:
        text = data[1].strip().strip('] ')
        defaults.update(_parse_attribs(text))
        return DotNode(name, **defaults)

def _parse_edges(text, nodes, defaults):
    """
    Parses a line of dot code containing
    edge data and returns a L{DotEdge} instance.
    The edge nodes must be contained in I{nodes},
    an iterable containing all nodes added to the graph.

    @type      text: C{str}
    @param     text: textual representation of dot edge
    @type     nodes: C{iterable}
    @param    nodes: iterable containing nodes
    @type  defaults: C{dict}
    @param defaults: mapping of (default) attribute keys to values
    @rtype:          L{DotEdge}
    @return:         dot edge
    """
    data = text.split('[')
    edge_data = data[0]
    node_names = [s.strip(' >"') for s in edge_data.split('-') if s]
    nodes_dict = dict((n.name, n) for n in nodes)
    nodes_list = []
    new_nodes = []
    for name in node_names:
        try:
            nodes_list.append(nodes_dict[name])
        except KeyError:
            n = DotNode(name)
            nodes_list.append(n)
            new_nodes.append(n)
    if len(data) == 1:
        # no attributes set
        return[DotEdge(nodes_list[i], nodes_list[i+1], **defaults) for i in range(len(node_names)-1)], new_nodes
    else:
        text = data[1].strip().strip('] ')
        defaults.update(_parse_attribs(text))
        return [DotEdge(nodes_list[i], nodes_list[i+1], **defaults) for i in range(len(node_names)-1)], new_nodes

def _parse_subgraph(lines, subgraph, node_defaults, edge_defaults, graph_defaults):
    """
    Parses the dot code contained in the iterator I{lines}
    and creates a corresponding DotSubgraph instance.

    @type           lines: C{iterable}
    @param          lines: iterable containing valid lines of dot code
    @type        subgraph: L{DotSubgraph}
    @param       subgraph: dot subgraph
    @type   node_defaults: C{dict}
    @param  node_defaults: mapping of node attribute keys to values
    @type   edge_defaults: C{dict}
    @param  edge_defaults: mapping of edge attribute keys to values
    @type  graph_defaults: C{dict}
    @param graph_defaults: mapping of graph attribute keys to values
    @rtype:                L{DotSubgraph}
    @return:               dot subgraph supplied as argument, updated with
                           information in I{lines}
    """
    brackets = 0
    for line in lines:
        if line == '{':
            brackets += 1
            continue
        elif line == '}':
            brackets -= 1
            if brackets:
                continue
            else:
                break
        words = line.split()
        if words[0].lower() == 'subgraph':
            if len(words) == 2:
                name = words[1]
            else:
                name = None
            subgraph.subgraphs.append(_parse_subgraph(lines,
                                                      DotSubgraph(name),
                                                      node_defaults.copy(),
                                                      edge_defaults.copy(),
                                                      graph_defaults.copy()))
        elif '=' in line and not '[' in line and not line.startswith('"'):
            # attribute
            key, val = _parse_attrib(line)
            graph_defaults[key] = val
        elif len(words) > 1 and words[1] in ['--', '->']:
            edges, new_nodes = _parse_edges(line, subgraph.iter_nodes(), edge_defaults.copy())
            for node in new_nodes:
                node.attribs = node_defaults.copy()
                subgraph.nodes.append(node)
            subgraph.edges.extend(edges)
        else:
            if words[0].lower() == 'node':
                node_defaults.update(_parse_attribs(words[1].strip('[] ')))
            elif words[0].lower() == 'edge':
                edge_defaults.update(_parse_attribs(words[1].strip('[] ')))
            elif words[0].lower() == 'graph':
                graph_defaults.update(_parse_attribs(words[1].strip('[] ')))
            else:
                subgraph.nodes.append(_parse_node(line, node_defaults.copy()))
    subgraph.attribs = graph_defaults
    subgraph.subgraphs_inherit = False
    return subgraph

def parse_dot(text):
    """
    Parses the dot code contained in I{text} and creates
    a corresponding DotGraph instance.  It will not parse
    arbitrary dot files, but will parse those generated by
    the L{DotGraph} class.  Comments enclosed in C-style /* */
    delimiters are permissible as long as the delimiters each
    occupy a single line.  (This can be used to store additional
    data that should be ignored by the parser / Graphviz.)

    @type  text: C{str}
    @param text: valid dot code
    @rtype:      L{DotGraph}
    @return:     dot graph
    """
    def parse(s):
        s = iter(s)
        current = []
        for ch in s:
            if ch in ['{', '}', ';']:
                yield ''.join(current).strip()
                if not ch == ';':
                    yield ch
                current = []
                continue
            if ch == '\n':
                for ch in s:
                    if ch not in ['\n', '\t', ' ']:
                        break
                if ch in ['{', '}']:
                    yield ch
                else:
                    current.append(ch)
            else:
                current.append(ch)
    lines = parse(text)
    line = lines.next()
    words = line.split()
    strict = False
    if words[0] == 'strict':
        strict = True
        graph_type = words[1]
        if len(words) == 3:
            name = words[2]
        else:
            name = None
    else:
        graph_type = words[0]
        if len(words) == 2:
            name = words[1]
        else:
            name = None
    return _parse_subgraph(lines, DotGraph(graph_type, name, strict), {}, {}, {})

def from_file(filename):
    """
    Creates a GraphNode instance from the dot
    file at location I{filename}.

    @type  filename: C{str}
    @param filename: path to file
    @rtype:          L{DotGraph}
    @return:         dot graph
    """
    f = open(filename, 'r')
    try:
        text = f.read()
        return parse_dot(text)
    finally:
        f.close()
    
def _execute(command):
    """
    Executes I{command}.

    @type  command: C{str}
    @param command: command (invoking e.g. dot)
    """
    w, r = os.popen4(command)
    output = r.read()
    w.close()
    r.close()
    if output:
        print output
        
def to_image(text, filename, prog='dot', format='dot'):
    """
    Creates an image from the dot code contained in I{text}
    and saves to file.  The program specified as I{prog} is
    used and the image format is that specified by I{format}
    (see graphviz documentation at U{http://www.graphviz.org/doc/}).

    @type      text: C{str}
    @param     text: valid dot code
    @type  filename: C{str}
    @param filename: path to image file
    @type      prog: C{str}
    @param     prog: program
    @type    format: C{str}
    @param   format: image format
    """
    # prog can be a series of commands
    # like 'unflatten -l 3 | dot'
    temp_path = tempfile.mktemp()
    f = open(temp_path, 'w')
    try:
        f.write(text)
        f.close()
        progs = prog.split('|')
        progs[0] = progs[0] + ' %s ' % temp_path
        prog = '|'.join(progs)
        _execute('%s -T%s -o %s' % (prog, format, filename))
    finally:
        f.close()
        os.remove(temp_path)

#############utility function for simple graphs#############

def graph2image(graph, filename, is_directed=True, prog='dot', format='png'):
    node_map = {}
    dot_g = DotGraph(['graph', 'digraph'][is_directed], 'g', True)
    dot_g.set_attrib('bgcolor', 'transparent')
    #dot_g.setAttrib('size', '"12,12"')
    dot_g.set_attrib('ratio', 'compress')
    for node in graph.iter_nodes():
        dot_n = DotNode(str(id(node)), label='"%s"' % str(node), fillcolor='white', style='filled')
        node_map[node] = dot_n
        dot_g.add_node(dot_n)
    for edge in graph.iter_edges():
        dot_e = DotEdge(node_map[edge[0]], node_map[edge[1]], is_directed)
        dot_g.add_edge(dot_e)
    dot_g.to_image(filename, prog=prog, format=format)


####################a couple of quick tests##################
def test():
    a, b, c, d, e, f = [DotNode('a', label=100, style='filled', fillcolor='white'), DotNode('b', style='filled', fillcolor='white'),
                        DotNode('c', style='filled', fillcolor='white'), DotNode('d', style='filled', fillcolor='white'),
                        DotNode('e', style='filled', fillcolor='white'), DotNode('f', style='filled', fillcolor='white')]
    sub = DotSubgraph('cluster1', style='filled', color='grey')
    subsub = DotSubgraph('cluster2', style='filled', color='yellow')
    subsub.add_node(a)
    sub.add_subgraph(subsub)
    sub.add_edge(DotEdge(c, d, True, color='orange'))
    sub.add_node(c)
    sub.add_node(d)
    g = DotGraph('digraph', 'g', True)
    g.add_subgraph(sub)
    g.add_node(b)
    g.add_node(e)
    g.add_node(f)
    g.add_edge(DotEdge(a, b, True, color='blue'))
    g.add_edge(DotEdge(e, f, True))
    g.add_edge(DotEdge(f, d, True, label='"0.04"', arrowhead='none'))
    return g
    
def test2():
    a, b, c, d, e, f = [DotNode('a', label=100, style='filled', fillcolor='white'), DotNode('b', style='filled', fillcolor='white'),
                        DotNode('c', label=''), DotNode('d', style='filled', fillcolor='white'),
                        DotNode('e', style='filled', fillcolor='white'), DotNode('f', style='filled', fillcolor='white')]
    sub = DotSubgraph('cluster1', style='filled', color='grey')
    subsub = DotSubgraph('cluster2', style='filled', color='yellow')
    subsub.add_node(a)
    sub.add_subgraph(subsub)
    sub.add_edge(DotEdge(c, d, False, color='orange'))
    sub.add_node(c)
    sub.add_node(d)
    g = DotGraph('graph', 'g', True)
    sub.set_attrib('label', '"default"')
    g.add_subgraph(sub)
    g.set_attrib('color', 'white')
    g.add_node(b)
    g.add_node(e)
    g.add_node(f)
    g.add_edge(DotEdge(a, b, False, color='blue'))
    g.add_edge(DotEdge(e, f, False))
    g.add_edge(DotEdge(f, d, False, label='"0.04"'))
    g.add_edge(DotEdge(f, d, False, label='"0.06"'))
    return g

def test3():
    n123456 = DotNode('n1+n2+n3+n4+n5+n6', style='filled', fillcolor='white')
    n123 = DotNode('n1+n2+n3', style='filled', fillcolor='white')
    n1245 = DotNode('n1+n2+n4+n5', style='filled', fillcolor='white')
    n2356 = DotNode('n2+n3+n5+n6', style='filled', fillcolor='white')
    n1346 = DotNode('n1+n3+n4+n6', style='filled', fillcolor='white')
    n456 = DotNode('n4+n5+n6', style='filled', fillcolor='white')
    n12 = DotNode('n1+n2', style='filled', fillcolor='white')
    n23 = DotNode('n2+n3', style='filled', fillcolor='white')
    n25 = DotNode('n2+n5', style='filled', fillcolor='white')
    n13 = DotNode('n1+n3', style='filled', fillcolor='white')
    n14 = DotNode('n1+n4', style='filled', fillcolor='white')
    n45 = DotNode('n4+n5', style='filled', fillcolor='white')
    n36 = DotNode('n3+n6', style='filled', fillcolor='white')
    n56 = DotNode('n5+n6', style='filled', fillcolor='white')
    n46 = DotNode('n4+n6', style='filled', fillcolor='white')
    n1 = DotNode('n1', style='filled', fillcolor='white')
    n2 = DotNode('n2', style='filled', fillcolor='white')
    n3 = DotNode('n3', style='filled', fillcolor='white')
    n4 = DotNode('n4', style='filled', fillcolor='white')
    n5 = DotNode('n5', style='filled', fillcolor='white')
    n6 = DotNode('n6', style='filled', fillcolor='white')
    nodes = [n123456, n123, n1245, n2356, n1346, n456, n12, n23,
             n25, n13, n14, n45, n36, n56, n46, n1, n2, n3, n4, n5, n6]
    # update attribs
    for node in nodes:
        node.attribs['shape'] = 'box'
    g = DotGraph('digraph', 'g', True)#, bb='"5,5"', ranksep=0.7, size='"4,4"')
    for node in nodes:
        g.add_node(node)
    edges = [[n123456, n123], [n123456, n456], [n123456, n1245], [n123456, n36],
             [n123456, n2356], [n123456, n14], [n123456, n1346], [n123456, n25],
             [n123, n12], [n123, n3], [n123, n23], [n123, n1], [n123, n13], [n123, n2],
             [n1245, n12], [n1245, n45], [n1245, n25], [n1245, n14],
             [n2356, n23], [n2356, n56], [n2356, n25], [n2356, n36],
             [n1346, n13], [n1346, n46], [n1346, n14], [n1346, n36],
             [n456, n45], [n456, n6], [n456, n56], [n456, n4], [n456, n46], [n456, n5],
             [n12, n1], [n12, n2], [n23, n2], [n23, n3],
             [n25, n2], [n25, n5], [n13, n1], [n13, n3],
             [n14, n1], [n14, n4], [n45, n4], [n45, n5],
             [n36, n3], [n36, n6], [n46, n4], [n46, n6],
             [n56, n5], [n56, n6]]
    for edge in edges:
        g.add_edge(DotEdge(edge[0], edge[1], True))
    return g
    
    
    


