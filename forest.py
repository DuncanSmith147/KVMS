
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


class Forest(object):
    """
    A Forest is a collection of disconnected components (of some graph / tree).
    """
    def __init__(self, components, attr):
        """
        A forest contains a list of I{components} and a mapping of element
        ids to components.  Thus the component to which an element
        belongs can be efficiently identified.  An element would typically be,
        for example, a graph node.  The mapping of element ids to components
        is the dictionary I{self.owners}.

        @type  components: C{list}
        @param components: a list containing (weakly) disconnected components
        @type        attr: C{string}
        @param       attr: getattr(component, attr)() returns iterable
                           containing elements of component (e.g. nodes)
        @invariant:        all elements have are unique and hashable
        @note:             changes that are made to components will not be
                           reflected in I{self.owners}
        """
        self.components = components
        self.owners = {}
        for component in components:
            for element in getattr(component, attr)():
                self.owners[element] = component

    def __len__(self):
        """
        The length of a forest is the number of components it contains.
        It is not guaranteed that a component is not empty, e.g. a graph
        containing zero nodes.
        """
        return len(self.components)

    def __getitem__(self, index):
        return self.components[index]



