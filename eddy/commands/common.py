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


from PyQt5.QtWidgets import QUndoCommand

from eddy.datatypes import Item


class CommandItemsMultiAdd(QUndoCommand):
    """
    This command is used to add a collection of items to the graphic scene.
    """
    def __init__(self, scene, collection):
        """
        Initialize the command.
        :param scene: the scene where this command is being performed.
        :param collection: a collection of items to add to the scene.
        """
        self.scene = scene
        self.collection = collection
        self.selected = scene.selectedItems()

        if len(collection) == 1:
            super().__init__('add {} {}'.format(collection[0].name, 'node' if collection[0].node else 'edge'))
        else:
            super().__init__('add {} items'.format(len(collection)))

    def redo(self):
        """redo the command"""
        self.scene.clearSelection()
        for item in self.collection:
            self.scene.addItem(item)
            item.setSelected(True)
        # emit updated signal
        self.scene.updated.emit()

    def undo(self):
        """undo the command"""
        self.scene.clearSelection()
        for item in self.collection:
            self.scene.removeItem(item)
        for item in self.selected:
            item.setSelected(True)
        # emit updated signal
        self.scene.updated.emit()


class CommandItemsMultiRemove(QUndoCommand):
    """
    This command is used to remove multiple items from the scene.
    The selection of the items involved in the multi remove needs to be handled somewhere else.
    """
    def __init__(self, scene, collection):
        """
        Initialize the command.
        :param scene: the graphic scene where this command is being performed.
        :param collection: a collection of items to remove.
        """
        self.scene = scene
        self.nodes = {item for item in collection if item.node}
        self.edges = {item for item in collection if item.edge}

        # compute the new inputs order for role chain and property assertion nodes
        # which are not being removed but whose other endpoint is being detached.
        self.inputs = {n: {
            'undo': n.inputs[:],
            'redo': n.inputs[:],
        } for edge in self.edges \
            if edge.isItem(Item.InputEdge) \
                for n in {edge.source, edge.target} \
                    if n.isItem(Item.RoleChainNode, Item.PropertyAssertionNode) and \
                        n not in self.nodes}

        for node in self.inputs:
            for edge in node.edges:
                if edge.isItem(Item.InputEdge) and edge in self.edges and edge.target is node:
                    self.inputs[node]['redo'].remove(edge.id)

        if len(collection) == 1:
            super().__init__('remove {} {}'.format(collection[0].name, 'node' if collection[0].node else 'edge'))
        else:
            super().__init__('remove {} items'.format(len(collection)))

    def redo(self):
        """redo the command"""
        # remove the edges
        for edge in self.edges:
            edge.source.removeEdge(edge)
            edge.target.removeEdge(edge)
            self.scene.removeItem(edge)
        # remove the nodes
        for node in self.nodes:
            self.scene.removeItem(node)
        # update node inputs
        for node in self.inputs:
            node.inputs = self.inputs[node]['redo'][:]
            for edge in node.edges:
                edge.updateEdge()
        # emit updated signal
        self.scene.updated.emit()

    def undo(self):
        """undo the command"""
        # add back the nodes
        for node in self.nodes:
            self.scene.addItem(node)
        # add back the edges
        for edge in self.edges:
            edge.source.addEdge(edge)
            edge.target.addEdge(edge)
            self.scene.addItem(edge)
        # update node inputs
        for node in self.inputs:
            node.inputs = self.inputs[node]['undo'][:]
            for edge in node.edges:
                edge.updateEdge()
        # emit updated signal
        self.scene.updated.emit()


class CommandComposeAxiom(QUndoCommand):
    """
    This command is used to compose axioms.
    """
    def __init__(self, name, scene, source, nodes, edges):
        """
        Initialize the command.
        :param name: the name of the undo command.
        :param scene: the graphic scene where this command is being performed.
        :param source: the source node of the composition
        :param nodes: a set of nodes to be used in the composition.
        :param edges: a set of edges to be used in the composition.
        """
        super().__init__(name)
        self.scene = scene
        self.source = source
        self.nodes = nodes
        self.edges = edges

    def redo(self):
        """redo the command"""
        # add items to the scene
        for item in self.nodes | self.edges:
            self.scene.addItem(item)
        # map edges over source and target nodes
        for edge in self.edges:
            edge.source.addEdge(edge)
            edge.target.addEdge(edge)
            edge.updateEdge()
        # emit updated signal
        self.scene.updated.emit()

    def undo(self):
        """undo the command"""
        # remove edge mappings from source and target nodes
        for edge in self.edges:
            edge.source.removeEdge(edge)
            edge.target.removeEdge(edge)
        # remove items from the scene
        for item in self.nodes | self.edges:
            self.scene.removeItem(item)
        # emit updated signal
        self.scene.updated.emit()


class CommandDecomposeAxiom(QUndoCommand):
    """
    This command is used to decompose axioms.
    """
    def __init__(self, name, scene, source, items):
        """
        Initialize the command.
        :param name: the name of the undo command.
        :param scene: the graphic scene where this command is being performed.
        :param source: the source node of the decomposition.
        :param items: a set of items to be removed from the composition.
        """
        super().__init__(name)
        self.items = items
        self.scene = scene
        self.source = source

    def redo(self):
        """redo the command"""
        for item in self.items:
            if item.edge:
                item.source.removeEdge(item)
                item.target.removeEdge(item)
                self.scene.removeItem(item)
        for item in self.items:
            if item.node:
                self.scene.removeItem(item)
        self.scene.updated.emit()

    def undo(self):
        """undo the command"""
        for item in self.items:
            if item.node:
                self.scene.addItem(item)
        for item in self.items:
            if item.edge:
                item.source.addEdge(item)
                item.target.addEdge(item)
                self.scene.addItem(item)
        self.scene.updated.emit()


class CommandRefactor(QUndoCommand):
    """
    This command is used to perform refactoring by applying multiple QUndoCommand.
    """
    def __init__(self, name, scene, commands):
        """
        Initialize the command.
        :param name: the name of the undo command.
        :param scene: the scene where the command is being performed.
        :param commands: a collection of QUndoCommand to be performed.
        """
        super().__init__(name)
        self.scene = scene
        self.commands = commands

    def redo(self):
        """redo the command"""
        for command in self.commands:
            command.redo()
        self.scene.updated.emit()

    def undo(self):
        """undo the command"""
        for command in self.commands:
            command.undo()
        self.scene.updated.emit()