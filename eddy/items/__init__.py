# -*- coding: utf-8 -*-

##########################################################################
#                                                                        #
#  Eddy: an editor for the Graphol ontology language.                    #
#  Copyright (C) 2015 Daniele Pantaleone <danielepantaleone@me.com>      #
#                                                                        #
#  This program is free software: you can redistribute it and/or modify  #
#  it under the terms of the GNU General Public License as published by  #
#  the Free Software Foundation, either version 3 of the License, or     #
#  (at your option) any later version.                                   #
#                                                                        #
#  This program is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          #
#  GNU General Public License for more details.                          #
#                                                                        #
#  You should have received a copy of the GNU General Public License     #
#  along with this program. If not, see <http://www.gnu.org/licenses/>.  #
#                                                                        #
##########################################################################
#                                                                        #
#  Graphol is developed by members of the DASI-lab group of the          #
#  Dipartimento di Ingegneria Informatica, Automatica e Gestionale       #
#  A.Ruberti at Sapienza University of Rome: http://www.dis.uniroma1.it/ #
#                                                                        #
#     - Domenico Lembo <lembo@dis.uniroma1.it>                           #
#     - Valerio Santarelli <santarelli@dis.uniroma1.it>                  #
#     - Domenico Fabio Savo <savo@dis.uniroma1.it>                       #
#     - Marco Console <console@dis.uniroma1.it>                          #
#                                                                        #
##########################################################################


from abc import ABCMeta, abstractmethod
from PyQt5.QtWidgets import QGraphicsItem, QGraphicsTextItem
from eddy.datatypes import Item


class AbstractItem(QGraphicsItem):
    """
    Base class for all the diagram elements.
    """
    __metaclass__ = ABCMeta

    item = 0  # an integer identifying this node as unique item type
    name = 'item' # a lowercase word which identifies this object
    prefix = 'i' # a prefix character to be prepended to the unique id
    xmlname = 'item' # a lowercase word used to identify this node in XML related documents

    def __init__(self, scene, id=None, description='', url='', **kwargs):
        """
        Initialize the item.
        :param scene: the scene where this item is being added.
        :param id: the id of this item.
        :param description: the description of this item.
        :param url: the url this item is referencing.
        """
        self._id = id or scene.guid.next(self.prefix)
        self._description = description
        self._url = url

        super().__init__(**kwargs)

    ####################################################################################################################
    #                                                                                                                  #
    #   PROPERTIES                                                                                                     #
    #                                                                                                                  #
    ####################################################################################################################

    @property
    def description(self):
        """
        Returns the description of the node.
        :rtype: str
        """
        return self._description.strip()

    @description.setter
    def description(self, value):
        """
        Set the description of the node.
        :param value: the description of the node.
        """
        self._description = value.strip()

    @property
    def edge(self):
        """
        Tells whether the current element is an Edge.
        :return: True if the item is an edge, False otherwise.
        :rtype: bool
        """
        return Item.InclusionEdge <= self.item <= Item.InstanceOfEdge

    @property
    def id(self):
        """
        Returns the node id.
        :rtype: str
        """
        return self._id

    @id.setter
    def id(self, value):
        """
        Set the node id.
        :param value: the new node id.
        """
        self._id = value

    @property
    def node(self):
        """
        Tells whether the current element is a Node.
        :return: True if the item is a node, False otherwise.
        :rtype: bool
        """
        return Item.ConceptNode <= self.item <= Item.PropertyAssertionNode

    @property
    def url(self):
        """
        Returns the url attached to the  node.
        :rtype: str
        """
        return self._url.strip()

    @url.setter
    def url(self, value):
        """
        Set the url attached to the node.
        :param value: the url to attach to the node.
        """
        self._url = value.strip()

    ####################################################################################################################
    #                                                                                                                  #
    #   INTERFACE                                                                                                      #
    #                                                                                                                  #
    ####################################################################################################################

    @abstractmethod
    def contextMenu(self, *args, **kwargs):
        """
        Returns the basic node context menu.
        :rtype: QMenu
        """
        pass

    @abstractmethod
    def copy(self, scene):
        """
        Create a copy of the current item.
        :param scene: a reference to the scene where this item is being copied from.
        """
        pass

    def isItem(self, *args):
        """
        Tells whether the current item is one of the given types.
        :param args: positional arguments specifying item types to match.
        :return: True if the item matches a given type, False otherwise.
        :rtype: bool
        """
        return self.item in args

    ####################################################################################################################
    #                                                                                                                  #
    #   IMPORT / EXPORT                                                                                                #
    #                                                                                                                  #
    ####################################################################################################################

    @abstractmethod
    def toGraphol(self, document):
        """
        Export the current item in Graphol format.
        :param document: the XML document where this item will be inserted.
        :rtype: QDomElement
        """
        pass

    @classmethod
    @abstractmethod
    def fromGraphol(cls, scene, E):
        """
        Create a new item instance by parsing a Graphol document item entry.
        :param scene: the scene where the element will be inserted.
        :param E: the Graphol document element entry.
        :rtype: AbstractItem
        """
        pass

    ####################################################################################################################
    #                                                                                                                  #
    #   DRAWING                                                                                                        #
    #                                                                                                                  #
    ####################################################################################################################

    @classmethod
    @abstractmethod
    def image(cls, **kwargs):
        """
        Returns an image suitable for the palette.
        :rtype: QPixmap
        """
        pass

    ####################################################################################################################
    #                                                                                                                  #
    #   GEOMETRY                                                                                                       #
    #                                                                                                                  #
    ####################################################################################################################

    @abstractmethod
    def painterPath(self):
        """
        Returns the current shape as QPainterPath (used for collision detection).
        :rtype: QPainterPath
        """
        pass

    ####################################################################################################################
    #                                                                                                                  #
    #   REPRESENTATION                                                                                                 #
    #                                                                                                                  #
    ####################################################################################################################

    def __repr__(self):
        """
        Object representaton.
        """
        return '{}:{}'.format(self.__class__.__name__, self.id)


class LabelItem(QGraphicsTextItem):
    """
    Base class for all the diagram labels: this class is mostly needed to improve performances.
    By using the LabelItem interface we can check the item type using the isItem() method instead of using isinstance().
    """
    __metaclass__ = ABCMeta

    item = 0 # an integer identifying this label as unique item type

    def __init__(self, parent=None):
        """
        Initialize the Label item.
        :param parent: the parent item
        """
        super().__init__(parent)

    ####################################################################################################################
    #                                                                                                                  #
    #   PROPERTIES                                                                                                     #
    #                                                                                                                  #
    ####################################################################################################################

    @property
    def edge(self):
        """
        Tells whether the current element is an Edge.
        :return: True if the item is an edge, False otherwise.
        :rtype: bool
        """
        return False

    @property
    def node(self):
        """
        Tells whether the current element is a Node.
        :return: True if the item is a node, False otherwise.
        :rtype: bool
        """
        return False

    ####################################################################################################################
    #                                                                                                                  #
    #   INTERFACE                                                                                                      #
    #                                                                                                                  #
    ####################################################################################################################

    def isItem(self, *args):
        """
        Tells whether the current item is one of the given types.
        :param args: positional arguments specifying item types to match.
        :return: True if the item matches a given type, False otherwise.
        :rtype: bool
        """
        return self.item in args

    ####################################################################################################################
    #                                                                                                                  #
    #   REPRESENTATION                                                                                                 #
    #                                                                                                                  #
    ####################################################################################################################

    def __repr__(self):
        """
        Object representaton.
        """
        return '{}:{}'.format(self.__class__.__name__, self.id)


from eddy.items.nodes import AttributeNode, ConceptNode, ComplementNode, DatatypeRestrictionNode, DisjointUnionNode
from eddy.items.nodes import DomainRestrictionNode, EnumerationNode, IndividualNode, IntersectionNode
from eddy.items.nodes import PropertyAssertionNode, RangeRestrictionNode, RoleNode, RoleChainNode
from eddy.items.nodes import RoleInverseNode, UnionNode, ValueDomainNode, ValueRestrictionNode
from eddy.items.edges import InputEdge, InclusionEdge, InstanceOfEdge


__mapping__ = {
    ConceptNode.xmlname: ConceptNode,
    ValueDomainNode.xmlname: ValueDomainNode,
    RoleNode.xmlname: RoleNode,
    IndividualNode.xmlname: IndividualNode,
    AttributeNode.xmlname: AttributeNode,
    ValueRestrictionNode.xmlname: ValueRestrictionNode,
    DomainRestrictionNode.xmlname: DomainRestrictionNode,
    RangeRestrictionNode.xmlname: RangeRestrictionNode,
    UnionNode.xmlname: UnionNode,
    EnumerationNode.xmlname: EnumerationNode,
    ComplementNode.xmlname: ComplementNode,
    RoleChainNode.xmlname: RoleChainNode,
    IntersectionNode.xmlname: IntersectionNode,
    RoleInverseNode.xmlname: RoleInverseNode,
    DatatypeRestrictionNode.xmlname: DatatypeRestrictionNode,
    DisjointUnionNode.xmlname: DisjointUnionNode,
    PropertyAssertionNode.xmlname: PropertyAssertionNode,
    InclusionEdge.xmlname: InclusionEdge,
    InputEdge.xmlname: InputEdge,
    InstanceOfEdge.xmlname: InstanceOfEdge,
}