# -*- coding: utf-8 -*-

##########################################################################
#                                                                        #
#  Eddy: a graphical editor for the specification of Graphol ontologies  #
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
#  #####################                          #####################  #
#                                                                        #
#  Graphol is developed by members of the DASI-lab group of the          #
#  Dipartimento di Ingegneria Informatica, Automatica e Gestionale       #
#  A.Ruberti at Sapienza University of Rome: http://www.dis.uniroma1.it  #
#                                                                        #
#     - Domenico Lembo <lembo@dis.uniroma1.it>                           #
#     - Valerio Santarelli <santarelli@dis.uniroma1.it>                  #
#     - Domenico Fabio Savo <savo@dis.uniroma1.it>                       #
#     - Daniele Pantaleone <pantaleone@dis.uniroma1.it>                  #
#     - Marco Console <console@dis.uniroma1.it>                          #
#                                                                        #
##########################################################################

import ast

from PyQt5 import QtCore,QtGui

from eddy.core.datatypes.owl import OWLStandardIRIPrefixPairsDict
from eddy.core.commands.diagram import CommandDiagramAdd
from eddy.core.commands.nodes import CommandNodeSetMeta
from eddy.core.datatypes.graphol import Item, Identity, Special
from eddy.core.functions.owl import OWLText
from eddy.core.functions.path import expandPath
from eddy.core.functions.signals import connect, disconnect
from eddy.core.output import getLogger
from eddy.core.items.common import AbstractItem
from eddy.core.items.nodes.common.base import AbstractNode
from eddy.core.commands.labels import GenerateNewLabel, CommandLabelChange

from eddy.ui.resolvers import PredicateBooleanConflictResolver
from eddy.ui.resolvers import PredicateDocumentationConflictResolver
from jnius import autoclass, cast, detach

LOGGER = getLogger()


# PROJECT INDEX
K_DIAGRAM = 'diagrams'
K_EDGE = 'edges'
K_ITEMS = 'items'
K_META = 'meta'
K_NODE = 'nodes'
K_PREDICATE = 'predicates'
K_TYPE = 'types'

# PROJECT MERGE
K_CURRENT = 'current'
K_FINAL = 'final'
K_IMPORTING = 'importing'
K_REDO = 'redo'
K_UNDO = 'undo'
K_ITEM = 'item'
K_NAME = 'name'
K_PROPERTY = 'property'

# PREDICATES META KEYS
K_DESCRIPTION = 'description'

K_FUNCTIONAL = 'functional'
K_ASYMMETRIC = 'asymmetric'
K_INVERSE_FUNCTIONAL = 'inverseFunctional'
K_IRREFLEXIVE = 'irreflexive'
K_REFLEXIVE = 'reflexive'
K_SYMMETRIC = 'symmetric'
K_TRANSITIVE = 'transitive'


# noinspection PyTypeChecker
class Project(QtCore.QObject):
    """
    Extension of QtCore.QObject which implements a Graphol project.
    Additionally to built-in signals, this class emits:

    * sgnDiagramAdded: whenever a Diagram is added to the Project.
    * sgnDiagramRemoved: whenever a Diagram is removed from the Project.
    * sgnItemAdded: whenever an item is added to the Project.
    * sgnItemRemoved: whenever an item is removed from the Project.
    * sgnMetaAdded: whenever predicate metadata are added to the Project.
    * sgnMetaRemoved: whenever predicate metadata are removed from the Project.
    * sgnUpdated: whenever the Project is updated in any of its parts.
    """
    sgnDiagramAdded = QtCore.pyqtSignal('QGraphicsScene')
    sgnDiagramRemoved = QtCore.pyqtSignal('QGraphicsScene')
    sgnItemAdded = QtCore.pyqtSignal('QGraphicsScene', 'QGraphicsItem')
    sgnItemRemoved = QtCore.pyqtSignal('QGraphicsScene', 'QGraphicsItem')
    sgnMetaAdded = QtCore.pyqtSignal(Item, str)
    sgnMetaRemoved = QtCore.pyqtSignal(Item, str)
    sgnUpdated = QtCore.pyqtSignal()

    sgnIRIPrefixesEntryModified = QtCore.pyqtSignal(str,str,str,str)
    sgnIRIPrefixEntryAdded = QtCore.pyqtSignal(str,str,str)
    sgnIRIPrefixEntryRemoved = QtCore.pyqtSignal(str,str,str)
    sgnIRIPrefixesEntryIgnored = QtCore.pyqtSignal(str,str,str)

    sgnIRINodeEntryAdded = QtCore.pyqtSignal(str,str,str)
    sgnIRINodeEntryRemoved = QtCore.pyqtSignal(str,str,str)
    sgnIRINodeEntryIgnored = QtCore.pyqtSignal(str,str,str)

    sgnIRIVersionEntryAdded = QtCore.pyqtSignal(str,str,str)
    sgnIRIVersionEntryRemoved = QtCore.pyqtSignal(str,str,str)
    sgnIRIVersionEntryIgnored = QtCore.pyqtSignal(str,str,str)

    sgnIRIPrefixNodeDictionaryUpdated = QtCore.pyqtSignal()

    def __init__(self, **kwargs):
        """
        Initialize the graphol project.
        """
        super().__init__(kwargs.get('session'))
        self.index = ProjectIndex()
        #self.iri = kwargs.get('iri', 'NULL')
        self.name = kwargs.get('name')
        self.path = expandPath(kwargs.get('path'))
        #self.prefix = kwargs.get('prefix', 'NULL')
        self.profile = kwargs.get('profile')
        self.profile.setParent(self)
        self.version = kwargs.get('version', '1.0')

        ###  variables controlled by reasoners  ###
        self.ontology_OWL = None
        self.axioms_to_nodes_edges_mapping = None

        self.unsatisfiable_classes = []
        self.explanations_for_unsatisfiable_classes = []
        self.unsatisfiable_attributes = []
        self.explanations_for_unsatisfiable_attributes = []
        self.unsatisfiable_roles = []
        self.explanations_for_unsatisfiable_roles = []

        self.inconsistent_ontology = None
        self.explanations_for_inconsistent_ontology = []

        self.uc_as_input_for_explanation_explorer = None
        self.nodes_of_unsatisfiable_entities = []
        self.nodes_or_edges_of_axioms_to_display_in_widget = []
        self.nodes_or_edges_of_explanations_to_display_in_widget = []

        self.converted_nodes = dict()

        self.brush_blue = QtGui.QBrush(QtGui.QColor(43, 63, 173, 160))
        self.brush_light_red = QtGui.QBrush(QtGui.QColor(250, 150, 150, 100))
        self.brush_orange = QtGui.QBrush(QtGui.QColor(255, 165, 0, 160))
        ############  variables for IRI-prefixes management #############

        self.IRI_prefixes_nodes_dict = kwargs.get('IRI_prefixes_nodes_dict')
        self.init_IRI_prefixes_nodes_dict_with_std_data()

        connect(self.sgnItemAdded, self.add_item_to_IRI_prefixes_nodes_dict)
        connect(self.sgnItemRemoved, self.remove_item_from_IRI_prefixes_nodes_dict)
        connect(self.sgnIRIPrefixNodeDictionaryUpdated, self.regenerate_all_labels)

    @property
    def iri(self):

        #print('Project >> iri')

        return_list = []

        for iri in self.IRI_prefixes_nodes_dict:
            properties = self.IRI_prefixes_nodes_dict[iri][2]
            #print('iri',iri,'properties',properties)
            if 'Project_IRI' in properties:
                return_list.append(iri)

        #print('Project >> iri >> return_list',return_list)

        if len(return_list) == 0:
            return None
        elif len(return_list) == 1:
            return return_list[0]
        else:
            LOGGER.critical('Multiple project IRIs found'+return_list)
            return return_list[0]

        #print('Project >> iri END')

    @property
    def prefix(self):

        project_prefixes = self.prefixes

        if len(project_prefixes) == 0:
            return None
        else:
            return sorted(list(project_prefixes))[0]

    @property
    def prefixes(self):

        project_iri = self.iri

        if project_iri is None:
            return None

        project_prefixes = self.IRI_prefixes_nodes_dict[project_iri][0]

        return project_prefixes

    """
    @property
    def version(self):

        project_iri = self.iri
        version = self.IRI_prefixes_nodes_dict[project_iri][3]

        return version
    """

    def get_full_IRI(self,iri,version,remaining_characters):

        if (version is None) or (version == ''):
            return iri + '#' + remaining_characters
        else:
            return iri + '/' + version + '#' + remaining_characters

    def print_dictionary(self, dictionary):

        LOGGER.info('<<<<<<<<<          print_dictionary        >>>>>>>>')
        LOGGER.info('size of the dictionary - '+str(len(dictionary.keys())))

        LOGGER.info('Project_IRI - '+str(self.iri))
        LOGGER.info('Project_prefix(es) - '+str(self.prefixes))
        LOGGER.info('Project_prefix - '+str(self.prefix))

        for iri in dictionary.keys():
            prefixes = dictionary[iri][0]
            nodes = dictionary[iri][1]
            properties = dictionary[iri][2]
            #version = dictionary[iri][3]

            #print(iri, '-', prefixes, '-', nodes, '-', properties, '-', version)
            LOGGER.info(str(iri)+ ' - '+ str(prefixes)+ ' - '+ str(nodes)+ ' - '+ str(properties))
        LOGGER.info('********************')

        for p in self.nodes():
            LOGGER.info(str(p.text()))

        LOGGER.info('********************')

        for n in self.nodes():
            #if (n.Type is Item.AttributeNode) or (n.Type is Item.ConceptNode) or (n.Type is Item.IndividualNode) or (n.Type is Item.RoleNode):
            if (('AttributeNode' in str(type(n))) or ('ConceptNode' in str(type(n))) or (
                            'IndividualNode' in str(type(n))) or ('RoleNode' in str(type(n)))):
                if n.Type is Item.IndividualNode:
                    LOGGER.info(str(n.type())+ ','+ str(n.id)+ ','+ str(n.IRI(self))+ ','+ str(n.prefix(self))+ ','+ str(n.remaining_characters)+ ','+ str(n.identity()))
                else:
                    LOGGER.info(str(n.type())+ ','+ str(n.id)+ ','+ str(n.IRI(self))+ ','+ str(n.prefix(self))+ ','+ str(n.remaining_characters))

        LOGGER.info('<<<<<<<<<          print_dictionary (END)       >>>>>>>>')

    def copy_IRI_prefixes_nodes_dictionaries(self, from_dict, to_dict):

        # dict[key] = [set(),set()]
        for iri in from_dict.keys():
            prefixes = from_dict[iri][0]
            nodes = from_dict[iri][1]
            properties = from_dict[iri][2]
            #version = from_dict[iri][3]

            values = []
            to_prefixes = set()
            to_nodes = set()
            to_properties = set()
            #to_version = None

            to_prefixes = to_prefixes.union(prefixes)
            to_nodes = to_nodes.union(nodes)
            to_properties = to_properties.union(properties)
            #to_version = version

            values.append(to_prefixes)
            values.append(to_nodes)
            values.append(to_properties)
            #values.append(to_version)

            to_dict[iri] = values

        return to_dict

    def init_IRI_prefixes_nodes_dict_with_std_data(self):

        for std_iri in OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict.keys():

            if std_iri in self.IRI_prefixes_nodes_dict.keys():
                continue

            std_prefix = OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict[std_iri]
            prefixes = set()
            prefixes.add(std_prefix)
            nodes = set()
            properties = set()
            properties.add('Standard_IRI')

            values = []
            values.append(prefixes)
            values.append(nodes)
            values.append(properties)

            self.IRI_prefixes_nodes_dict[std_iri] = values

    @QtCore.pyqtSlot('QGraphicsScene', 'QGraphicsItem')
    def add_item_to_IRI_prefixes_nodes_dict(self, diagram, item):

        #print('>>>     add_item_to_IRI_prefixes_nodes_dict         ', item)

        #if item.type() in {Item.AttributeNode, Item.ConceptNode, Item.IndividualNode, Item.RoleNode}:
        if (('AttributeNode' in str(type(item))) or ('ConceptNode' in str(type(item))) or (
                    'IndividualNode' in str(type(item))) or ('RoleNode' in str(type(item)))):
            #print('item.type() in {Item.AttributeNode, Item.ConceptNode, Item.IndividualNode, Item.RoleNode}:')

            node = item

            # print('2nd if statement')

            if (node.type() is Item.IndividualNode) and (node.identity() is Identity.Value):

                # print('if           (node.type() is Item.IndividualNode) and (item.identity() is Identity.Value):')
                if (node.IRI(self) is None):
                    prefix = str(node.datatype.value)[0:str(node.datatype.value).index(':')]

                    std_iri_prefix = ['http://www.w3.org/1999/02/22-rdf-syntax-ns', 'rdf',
                                      'http://www.w3.org/2000/01/rdf-schema', 'rdfs',
                                      'http://www.w3.org/2001/XMLSchema', 'xsd',
                                      'http://www.w3.org/2002/07/owl', 'owl']

                    ind_prefix = std_iri_prefix.index(prefix)
                    ind_iri = ind_prefix - 1
                    corr_iri = std_iri_prefix[ind_iri]
                else:
                    corr_iri = None
            else:

                # print('else          (node.type() is Item.IndividualNode) and (item.identity() is Identity.Value):')

                if (node.IRI(self) is None):

                    # print('if       (node.IRI(self) is None):')

                    if (node.type() is not (Item.IndividualNode)) and (node.special() is not None):

                        # print('if       (node.type() is not (Item.IndividualNode)) and (node.special() is not None):')

                        corr_iri = 'http://www.w3.org/2002/07/owl'
                    else:

                        # print('else       (node.type() is not (Item.IndividualNode)) and (node.special() is not None):')

                        corr_iri = self.iri
                else:

                    # print('else       (node.IRI(self) is None):')

                    corr_iri = None

            # print('corr_iri',corr_iri)

            if corr_iri is not None:
                # print('self.IRI_prefixes_nodes_dict[corr_iri][1] (before addition)',self.IRI_prefixes_nodes_dict[corr_iri][1])
                self.IRI_prefixes_nodes_dict[corr_iri][1].add(node)
                # print('self.IRI_prefixes_nodes_dict[corr_iri][1] (after addition)',self.IRI_prefixes_nodes_dict[corr_iri][1])
                self.sgnIRIPrefixNodeDictionaryUpdated.emit()

            #print('self.IRI_prefixes_nodes_dict',self.IRI_prefixes_nodes_dict)

        #print('>>>     add_item_to_IRI_prefixes_nodes_dict       END', item)

    @QtCore.pyqtSlot('QGraphicsScene', 'QGraphicsItem')
    def remove_item_from_IRI_prefixes_nodes_dict(self, diagram, node):

        print('>>>     remove_item_from_IRI_prefixes_nodes_dict        ',node)
        #remove the node in all the indices of the dictionary
        if (('AttributeNode' in str(type(node))) or ('ConceptNode' in str(type(node))) or (
                    'IndividualNode' in str(type(node))) or ('RoleNode' in str(type(node)))):
            corr_iris = []

            for IRI_in_dict in self.IRI_prefixes_nodes_dict.keys():
                if node in self.IRI_prefixes_nodes_dict[IRI_in_dict][1]:
                    corr_iris.append(IRI_in_dict)

            if len(corr_iris) == 1:
                self.IRI_prefixes_nodes_dict[corr_iris[0]][1].remove(node)
                self.sgnIRIPrefixNodeDictionaryUpdated.emit()
            elif len(corr_iris) == 0:
                LOGGER.warning('node is not present in the dictionary')
            else:
                LOGGER.critical('multiple IRIs found for node')

        print('>>>     remove_item_from_IRI_prefixes_nodes_dict    END    ',node)

    def get_iri_for_prefix(self,prefix_inp):

        if prefix_inp is None:
            return None

        return_list = []

        for iri in self.IRI_prefixes_nodes_dict.keys():
            prefixes = self.IRI_prefixes_nodes_dict[iri][0]
            if prefix_inp in prefixes:
                return_list.append(iri)

        if len(return_list) == 1:
            return return_list[0]
        elif len(return_list) == 0:
            return None
        else:
            LOGGER.critical('prefix mapped to multiple IRI(s)')
            return return_list

    def get_prefix_for_iri(self,inp_iri):

        prefixes = self.IRI_prefixes_nodes_dict[inp_iri][0]
        sorted_lst_prefixes = sorted(list(prefixes))

        return sorted_lst_prefixes[0]
    """
    #not used as of now
    def AddORremoveORmodifyVersionforIRI(self, iri_inp, version_to, dictionary, **kwargs):

        task = kwargs.get('task', None)

        if iri_inp in dictionary.keys():
            version_from = dictionary[iri_inp][3]
            if task == 'add':
                if version_to is None:
                    self.sgnIRIVersionEntryIgnored.emit(iri_inp, version_to,'Nothing to add')
                    return None
                else:
                    if version_from is None:
                        dictionary[iri_inp][3] = version_to
                        self.sgnIRIVersionEntryAdded.emit(iri_inp, version_to, 'Version added')
                        return dictionary
                    else:
                        if version_from == version_to:
                            self.sgnIRIVersionEntryIgnored.emit(iri_inp, version_to, 'Current and new value for the versions are the same. Nothing to change')
                            return None
                        else:
                            self.sgnIRIVersionEntryIgnored.emit(iri_inp, version_to, 'Another value present. (Remove_old_val + Add_new_val) or Modify_old_val.')
                            return None
            elif task == 'remove':
                if version_from is None:
                    self.sgnIRIVersionEntryIgnored.emit(iri_inp, version_from, 'Nothing to remove')
                    return None
                else:
                    dictionary[iri_inp][3] = None
                    self.sgnIRIVersionEntryRemoved.emit(iri_inp, version_from, 'Version added')
                    return dictionary
            elif task == 'modify':
                if version_from is None:
                    if version_to is None:
                        self.sgnIRIVersionEntryIgnored.emit(iri_inp, version_from, 'Nothing to change')
                        return None
                    else:
                        dictionary[iri_inp][3] = version_to
                        self.sgnIRIVersionEntryRemoved.emit(iri_inp, version_from, 'Version modified')
                        return dictionary
                else:
                    if version_to is None:
                        dictionary[iri_inp][3] = version_to
                        self.sgnIRIVersionEntryRemoved.emit(iri_inp, version_from, 'Version modified')
                        return dictionary
                    else:
                        if version_to == version_from:
                            self.sgnIRIVersionEntryIgnored.emit(iri_inp, version_from, 'Nothing to change')
                            return None
                        else:
                            dictionary[iri_inp][3] = version_to
                            self.sgnIRIVersionEntryRemoved.emit(iri_inp, version_from, 'Version modified')
                            return dictionary
            else:
                self.sgnIRIVersionEntryIgnored.emit(iri_inp, version_from, 'Programming error/ contact developer')
                LOGGER.critical('Programming error/ contact developer   >>>  AddORremoveORmodifyVersionforIRI')
                return None
        else:
            self.sgnIRIVersionEntryIgnored.emit(iri_inp, version_to, 'IRI not present in dictionary')
            return None
    """
    def modifyIRIPrefixesEntry(self,iri_from_val,prefixes_from_val,iri_to_val,prefixes_to_val, dictionary):

        None_1 = (iri_from_val is None)
        None_2 = (prefixes_from_val is None)
        None_3 = (iri_to_val is None)
        None_4 = (prefixes_to_val is None)

        ENTRY_ADD_OK_var = set()
        ENTRY_REMOVE_OK_var = set()
        ENTRY_IGNORE_var = set()

        @QtCore.pyqtSlot(str, str, str)
        def entry_ADD_ok(iri, prefix, message):

            ENTRY_ADD_OK_var.add(True)
            print('modifyIRIPrefixesEntry   >>>     entry_ADD_ok(self): ', iri, ',', prefix, ',', message)

        @QtCore.pyqtSlot(str, str, str)
        def entry_REMOVE_OK(iri, prefix, message):

            ENTRY_REMOVE_OK_var.add(True)
            print('modifyIRIPrefixesEntry   >>>     entry_REMOVE_ok(self): ', iri, ',', prefix, ',', message)

        @QtCore.pyqtSlot(str, str, str)
        def entry_NOT_OK(iri, prefix, message):

            ENTRY_IGNORE_var.add(True)
            print('modifyIRIPrefixesEntry   >>>     entry_NOT_OK(self): ', iri, ',', prefix, ',', message)

        connect(self.sgnIRIPrefixEntryAdded, entry_ADD_ok)
        connect(self.sgnIRIPrefixEntryRemoved, entry_REMOVE_OK)
        connect(self.sgnIRIPrefixesEntryIgnored, entry_NOT_OK)

        def modify_iri(from_iri, to_iri, dictionary):

            if from_iri == to_iri:
                return 'Nothing to modify in IRI'

            #msg needed

            temp_prefixes = dictionary[from_iri][0]
            temp_nodes = dictionary[from_iri][1]
            temp_properties = dictionary[from_iri][2]

            dictionary[from_iri][0] = set()
            dictionary[from_iri][1] = set()
            dictionary[from_iri][2] = set()

            #self.removeIRIPrefixEntry(dictionary,from_iri,None)
            self.addORremoveIRIPrefixEntry(dictionary, from_iri, None, 'remove_entry',remove_project_iri=True)

            if (False in ENTRY_REMOVE_OK_var) or (True in ENTRY_IGNORE_var):
                return str('Error could not modify IRI from '+from_iri+' to '+to_iri)

            #self.addIRIPrefixEntry(dictionary,to_iri,None)
            self.addORremoveIRIPrefixEntry(dictionary, to_iri, None, 'add_entry')

            if (False in ENTRY_ADD_OK_var) or (True in ENTRY_IGNORE_var):
                return str('Error could not modify IRI from '+from_iri+' to '+to_iri)

            if to_iri in dictionary.keys():
                dictionary[to_iri][0] = dictionary[to_iri][0].union(temp_prefixes)
                dictionary[to_iri][1] = dictionary[to_iri][1].union(temp_nodes)
                dictionary[to_iri][2] = dictionary[to_iri][2].union(temp_properties)
            else:
                dictionary[to_iri][0] = temp_prefixes
                dictionary[to_iri][1] = temp_nodes
                dictionary[to_iri][2] = temp_properties
            return 'Success'

        def modify_prefixes(iri_inp, from_prefixes, to_prefixes, dictionary):

            if (to_prefixes.issubset(from_prefixes) and from_prefixes.issubset(to_prefixes)):
                return 'Nothing to modify in prefixes'

            iri_keys = []
            msg = None

            if iri_inp is None:
                iri_keys.extend(dictionary.keys())
            else:
                iri_keys.append(iri_inp)

            for i in iri_keys:
                prefixes = dictionary[i][0]
                equal = (prefixes.issubset(from_prefixes) and from_prefixes.issubset(prefixes))
                if equal:
                    #dictionary[i][1] = set()
                    for p in from_prefixes:
                        #self.removeIRIPrefixEntry(dictionary, i, p)
                        self.addORremoveIRIPrefixEntry(dictionary, i, p, 'remove_entry', remove_project_prefixes=True)
                        if (False in ENTRY_REMOVE_OK_var) or (True in ENTRY_IGNORE_var):
                            return 'Error could not modify prefixes from '+str(from_prefixes)+' to '+str(to_prefixes)

                    for p in to_prefixes:
                        #self.addIRIPrefixEntry(dictionary,i,p)
                        self.addORremoveIRIPrefixEntry(dictionary, i, p, 'add_entry')
                        if (False in ENTRY_ADD_OK_var) or (True in ENTRY_IGNORE_var):
                            return 'Error could not modify prefixes from '+str(from_prefixes)+' to '+str(to_prefixes)
                else:
                    pass
            return 'Success'

        msg_1=None
        msg_2=None

        #Case1
        if (not None_1) and (None_2) and (not None_3) and (None_4):
            print('modifyIRIPrefixesEntry   >>>    Case1')
            msg_1 = modify_iri(iri_from_val,iri_to_val,dictionary)

        #Case2
        elif (not None_1) and (None_2) and (not None_3) and (not None_4):
            print('modifyIRIPrefixesEntry   >>>    Case2')
            msg_1 = modify_iri(iri_from_val, iri_to_val,dictionary)
            if 'Error' not in msg_1:
                msg_2 = modify_prefixes(iri_to_val,prefixes_from_val,prefixes_to_val,dictionary)

        #Case3
        elif (None_1) and (not None_2) and (None_3) and (not None_4):
            print('modifyIRIPrefixesEntry   >>>    Case3')
            for iri_key in dictionary.keys():
                msg_2 = modify_prefixes(iri_key, prefixes_from_val, prefixes_to_val, dictionary)
                if 'Error' in msg_2:
                    break

        #Case4
        elif (not None_1) and (not None_2) and (not None_3) and (None_4):
            print('modifyIRIPrefixesEntry   >>>    Case4')
            msg_1 = modify_iri(iri_from_val, iri_to_val,dictionary)
            if 'Error' not in msg_1:
                msg_2 = modify_prefixes(iri_to_val,prefixes_from_val,set(),dictionary)

        #Case5
        elif (not None_1) and (not None_2) and (None_3) and (not None_4):
            print('modifyIRIPrefixesEntry   >>>    Case5')
            msg_2 = modify_prefixes(iri_from_val, prefixes_from_val, prefixes_to_val, dictionary)

        #Case6
        elif (not None_1) and (not None_2) and (not None_3) and (not None_4):
            print('modifyIRIPrefixesEntry   >>>    Case6')
            msg_1 = modify_iri(iri_from_val, iri_to_val, dictionary)
            print('msg_1',msg_1)
            if 'Error' not in msg_1:
                msg_2 = modify_prefixes(iri_to_val, prefixes_from_val, prefixes_to_val, dictionary)
                print('msg_2', msg_2)
        #None of the cases
        else:
            LOGGER.critical('Case not dealt with/ Design fault; please contact programmer')
            self.sgnIRIPrefixesEntryIgnored.emit('', '', 'Case not dealt with/ Design fault; please contact programmer')
            return None

        error_C1 = (msg_1 is not None) and ('Error' in msg_1)
        error_C2 = (msg_2 is not None) and ('Error' in msg_2)

        if error_C1 or error_C2:
            msg_error = ''
            if error_C1:
                msg_error = msg_error + msg_1
            if error_C2:
                msg_error = msg_error + ' ; ' + msg_2

            msg_iris = 'From: '+ iri_from_val +' To: '+ iri_to_val
            msg_prefixes = 'From: ' + str(prefixes_from_val) + ' To: ' + str(prefixes_to_val)

            self.sgnIRIPrefixesEntryIgnored.emit(msg_iris,msg_prefixes,msg_error)
            return None
        else:
            self.sgnIRIPrefixesEntryModified.emit(iri_from_val,str(prefixes_from_val),iri_to_val,str(prefixes_to_val))
            return dictionary

    def addORremoveIRIPrefixEntry(self, dictionary, IRI_inp, Prefix_inp, inp, **kwargs):

        remove_project_iri = kwargs.get('remove_project_iri',False)
        remove_project_prefixes = kwargs.get('remove_project_prefixes', False)

        ### cannot add standart prefixes ###

        if (Prefix_inp is not None) and (Prefix_inp in {'rdf', 'rdfs', 'xsd', 'owl'}):
            self.sgnIRIPrefixesEntryIgnored.emit(IRI_inp, Prefix_inp, 'Cannot add/remove standard prefix(es)')
            return None
        ### cannot add standart IRI ###
        if (IRI_inp is not None) and (IRI_inp in OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict.keys()):
            self.sgnIRIPrefixesEntryIgnored.emit(IRI_inp, Prefix_inp, 'Cannot add/remove standard IRI(s)')
            return None

        if inp == 'add_entry':
            self.addIRIPrefixEntry(dictionary,IRI_inp,Prefix_inp)
        elif inp == 'remove_entry':
            self.removeIRIPrefixEntry(dictionary,IRI_inp,Prefix_inp,remove_project_iri=remove_project_iri,\
                                      remove_project_prefixes=remove_project_prefixes)
        else:
            LOGGER.error('PROJECT >> addORremoveIRIPrefixEntry >> invalid command')

    def addIRIPrefixEntry(self, dictionary, iri_inp, prefix_inp):

        print('addIRIPrefixEntry    >>>')
        #if [prefix_inp-IRI'] exists => addition is not possible
        #if [prefix_inp-IRI_inp] exists => addition not needed (duplicate entry)

        #corr_iri_of_prefix_inp = self.get_iri_for_prefix(prefix_inp)

        corr_iri_of_prefix_inp_in_dict = []

        for i in dictionary.keys():
            if dictionary[i][0] == prefix_inp:
                corr_iri_of_prefix_inp_in_dict.append(i)


        if (len(corr_iri_of_prefix_inp_in_dict) > 0):
            if(iri_inp not in corr_iri_of_prefix_inp_in_dict):
                self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, prefix_inp, str('prefix already mapped with IRI-' + corr_iri_of_prefix_inp_in_dict))
                return None
            else:
                self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, prefix_inp, '[IRI] entry already exists in table')
                return None

        if (prefix_inp is None) and (iri_inp in dictionary.keys()):
            self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, None, '[IRI] entry already exists in table')
            return None

        #C1 prefix_inp !None
        #C2 prefix_inp None
        #CA key already present
        #CB key is a new one

        if iri_inp in dictionary.keys():
            if prefix_inp is not None:
                #Case A1
                print('Case A1')
                dictionary[iri_inp][0].add(prefix_inp)
                self.sgnIRIPrefixEntryAdded.emit(iri_inp, prefix_inp, 'prefix added to existing IRI')
                return dictionary
            else:
                #Case A2
                print('Case A2')
                self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, None, 'Nothing to add')
                return None
        else:
            if prefix_inp is not None:
                #Case B1
                print('Case B1')
                prefixes = set()
                prefixes.add(prefix_inp)
                nodes = set()
                properties = set()
                #version = iri_version

                entry = []
                entry.append(prefixes)
                entry.append(nodes)
                entry.append(properties)
                #entry.append(version)

                dictionary[iri_inp] = entry
                self.sgnIRIPrefixEntryAdded.emit(iri_inp, prefix_inp, 'prefix added to new IRI')
                return dictionary
            else:
                # Case B2
                print('Case B2')
                prefixes = set()
                nodes = set()
                properties = set()
                #version = iri_version

                entry = []
                entry.append(prefixes)
                entry.append(nodes)
                entry.append(properties)
                #entry.append(version)

                dictionary[iri_inp] = entry
                self.sgnIRIPrefixEntryAdded.emit(iri_inp, None, 'new IRI added')
                return dictionary

    def addIRINodeEntry(self, dictionary, iri_inp, node_inp):

        temp = set()

        for iri in dictionary.keys():
            nodes = dictionary[iri][1]
            if node_inp in nodes:
                temp.add(iri)

        # check if node is already present
        if len(temp) > 0:
            if (len(temp) == 1) and (iri_inp in temp):
                self.sgnIRINodeEntryIgnored.emit(iri_inp, str(node_inp), 'Same entry already present in the table')
                return None
            else:
                self.sgnIRINodeEntryIgnored.emit(iri_inp, str(node_inp), 'Node mapped to another IRI-'+temp)
                return None

        msg = ''

        if iri_inp in dictionary.keys():
            pass
        else:
            dict_2 = self.copy_IRI_prefixes_nodes_dictionaries(dictionary, dict())
            res = self.addIRIPrefixEntry(dict_2, iri_inp, None)
            if res is None:
                self.sgnIRINodeEntryIgnored.emit(iri_inp, str(node_inp), 'Failed to add IRI to the table.')
                return None
            else:
                self.addIRIPrefixEntry(dictionary, iri_inp, None)
                msg = str('IRI added-'+iri_inp+'; ')

        dictionary[iri_inp][1].add(node_inp)
        msg = msg + str('node mapped to IRI-'+iri_inp)

        self.sgnIRINodeEntryAdded.emit(iri_inp, str(node_inp), msg)
        return dictionary

    def removeIRIPrefixEntry(self, dictionary, iri_inp, prefix_inp, **kwargs):

        remove_project_IRI = kwargs.get('remove_project_IRI',False)
        remove_project_prefixes = kwargs.get('remove_project_prefixes',False)

        # iri_inp does not exist => deletion not possible as key is absent
        #prefix_inp is None
            # nodes are mapped to IRI_inp =>  deletion not possible as nodes are linked to the IRI
        # prefix_inp is not None
            # [prefix_inp-iri_inp] does not exist => prefix not mapped with this IRI

        print('removeIRIPrefixEntry >>>',iri_inp, ' - ',prefix_inp)

        if iri_inp not in dictionary.keys():
            self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, prefix_inp, 'IRI is not present in the table')
            return None
        """
        if iri_inp in OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict.keys():
            self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, prefix_inp, 'cannot remove standard IRI')
            return None

        if prefix_inp in OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict.values():
            self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, prefix_inp, 'cannot remove a standard prefix')
            return None
        """

        if prefix_inp is None:
            if len(dictionary[iri_inp][1]) > 0:# attempt to delete entire IRI record
                self.sgnIRIPrefixesEntryIgnored.emit(iri_inp,prefix_inp,'Nodes are mapped to this IRI, deletion not possible. However it can be modified')
                return None
            if remove_project_IRI is False:
                if 'Project_IRI' in dictionary[iri_inp][2]:
                #if iri_inp == self.iri:
                    self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, prefix_inp,'cannot remove project IRI; project IRI can only be modified')
                    return None
            # remove iri_inp i.e. a key
            dictionary.pop(iri_inp)
            self.sgnIRIPrefixEntryRemoved.emit(iri_inp, None, 'IRI removed from table')
            return dictionary
        else:
            if prefix_inp not in dictionary[iri_inp][0]:
                self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, prefix_inp, 'prefix not mapped with this IRI')
                return None
            if remove_project_prefixes is False:
                if ('Project_IRI' in dictionary[iri_inp][2]) and (len(dictionary[iri_inp][0]) == 1):
                #if (prefix_inp == self.prefix) and (len(dictionary[iri_inp][0]) == 1):
                    self.sgnIRIPrefixesEntryIgnored.emit(iri_inp, prefix_inp, 'cannot remove the only project prefix, however it can me modified')
                    return None
            # remove prefix_inp from prefixes of iri_inp
            dictionary[iri_inp][0].remove(prefix_inp)
            self.sgnIRIPrefixEntryRemoved.emit(iri_inp, prefix_inp, 'Prefix is no longer mapped to this IRI')
            return dictionary

    def removeIRINodeEntry(self, dictionary, iri_inp, node_inp):

        nodes = dictionary[iri_inp][1]

        if node_inp not in nodes:
            self.sgnIRINodeEntryIgnored.emit(iri_inp, str(node_inp), str('Node not mapped to IRI'+iri_inp+', deletion not possible'))
            return None

        dictionary[iri_inp][1].remove(node_inp)
        print('node_inp',node_inp)
        self.sgnIRINodeEntryRemoved.emit(iri_inp, str(node_inp), str('Node no longer mapped to IRI'+iri_inp))
        return dictionary

    @QtCore.pyqtSlot()
    def regenerate_all_labels(self):

        for node in self.nodes():
            #if node.type() in {Item.AttributeNode, Item.ConceptNode, Item.IndividualNode, Item.RoleNode}:
            if (('AttributeNode' in str(type(node))) or ('ConceptNode' in str(type(node))) or (
                            'IndividualNode' in str(type(node))) or ('RoleNode' in str(type(node)))):
                new_label = GenerateNewLabel(self, node).return_label()
                CommandLabelChange(node.diagram, node, None, new_label).redo()

    def colour_items_in_case_of_unsatisfiability_or_inconsistent_ontology(self):

        for node_or_edge in self.nodes_or_edges_of_explanations_to_display_in_widget:

            node_or_edge.selection.setBrush(self.brush_light_red)
            node_or_edge.setCacheMode(AbstractItem.NoCache)
            node_or_edge.setCacheMode(AbstractItem.DeviceCoordinateCache)
            node_or_edge.update(node_or_edge.boundingRect())

        for node_or_edge in self.nodes_or_edges_of_axioms_to_display_in_widget:

            node_or_edge.selection.setBrush(self.brush_blue)
            node_or_edge.setCacheMode(AbstractItem.NoCache)
            node_or_edge.setCacheMode(AbstractItem.DeviceCoordinateCache)
            node_or_edge.update(node_or_edge.boundingRect())

        print('self.nodes_of_unsatisfiable_entities',self.nodes_of_unsatisfiable_entities)

        for node_or_str in self.nodes_of_unsatisfiable_entities:

            if str(type(node_or_str)) != '<class \'str\'>':

                node_or_str.selection.setBrush(self.brush_orange)
                # node.updateNode(valid=False)
                node_or_str.setCacheMode(node_or_str.NoCache)
                node_or_str.setCacheMode(node_or_str.DeviceCoordinateCache)

                # SCHEDULE REPAINT
                node_or_str.update(node_or_str.boundingRect())

        for d in self.diagrams():
            self.diagram(d.name).sgnUpdated.emit()

    def getOWLtermfornode(self, node):

        # looks up the dict for the raw term and then..
        # returns the string portion without the IRI and special characters

        for diag, val_in_diag in self.converted_nodes.items():
            for nd in val_in_diag:
                if (val_in_diag[nd] is not None) and (nd == node.id):
                    if str(type(val_in_diag[nd])) == '<class \'list\'>':

                        return_list = []
                        for ele in val_in_diag[nd]:
                            java_class = str(val_in_diag[nd])[2:str(val_in_diag[nd]).index(' ')]
                            cast(autoclass(java_class), ele)
                            detach()

                            return_list.append(ele.toString())

                        return return_list
                    else:
                        java_class = str(val_in_diag[nd])[1:str(val_in_diag[nd]).index(' ')]
                        cast(autoclass(java_class), val_in_diag[nd])
                        detach()

                        return val_in_diag[nd].toString()

        return None

    #############################################
    #   PROPERTIES
    #################################

    @property
    def session(self):
        """
        Returns the reference to the active session (alias for Project.parent()).
        :rtype: Session
        """
        return self.parent()

    #############################################
    #   INTERFACE
    #################################

    def addDiagram(self, diagram):
        """
        Add the given diagram to the Project, together with all its items.
        :type diagram: Diagram
        """
        if self.index.addDiagram(diagram):
            self.sgnDiagramAdded.emit(diagram)
            for item in diagram.items():
                if item.isNode() or item.isEdge():
                    diagram.sgnItemAdded.emit(diagram, item)

    def diagram_from_its_name(self, d_name):
        """
        Retrieves a diagram given its id.
        :type did: str
        :rtype: Diagram
        """
        diags = self.diagrams()

        for d in diags:
            if d.name == d_name:
                return d
        return None

    def diagram(self, did):
        """
        Returns the diagram matching the given id or None if no diagram is found.
        :type did: str
        :rtype: Diagram
        """
        return self.index.diagram(did)

    def diagrams(self):
        """
        Returns a collection with all the diagrams in this Project.
        :rtype: set
        """
        return self.index.diagrams()

    def edge(self, diagram, eid):
        """
        Returns the edge matching the given id or None if no edge is found.
        :type diagram: Diagram
        :type eid: str
        :rtype: AbstractEdge
        """
        return self.index.edge(diagram, eid)

    def edges(self, diagram=None):
        """
        Returns a collection with all the edges in the given diagram.
        If no diagram is supplied a collection with all the edges in the Project will be returned.
        :type diagram: Diagram
        :rtype: set
        """
        return self.index.edges(diagram)

    def isEmpty(self):
        """
        Returns True if the Project contains no element, False otherwise.
        :rtype: bool
        """
        return self.index.isEmpty()

    def item(self, diagram, iid):
        """
        Returns the item matching the given id or None if no item is found.
        :type diagram: Diagram
        :type iid: str
        :rtype: AbstractItem
        """
        return self.index.item(diagram, iid)

    def itemNum(self, item, diagram=None):
        """
        Returns the number of items of the given type which belongs to the given diagram.
        If no diagram is supplied, the counting is extended to the whole Project.
        :type item: Item
        :type diagram: Diagram
        :rtype: int
        """
        return self.index.itemNum(item, diagram)

    def items(self, diagram=None):
        """
        Returns a collection with all the items in the given diagram.
        If no diagram is supplied a collection with all the items in the Project will be returned.
        :type diagram: Diagram
        :rtype: set
        """
        return self.index.items(diagram)

    def meta(self, item, name):
        """
        Returns metadata for the given predicate, expressed as pair (item, name).
        :type item: Item
        :type name: str
        :rtype: dict
        """
        return self.index.meta(item, name)

    def metas(self, *types):
        """
        Returns a collection of pairs 'item', 'name' for all the predicates with metadata.
        :type types: list
        :rtype: list
        """
        return self.index.metas(*types)

    def node(self, diagram, nid):
        """
        Returns the node matching the given id or None if no node is found.
        :type diagram: Diagram
        :type nid: str
        :rtype: AbstractNode
        """
        return self.index.node(diagram, nid)

    def nodes(self, diagram=None):
        """
        Returns a collection with all the nodes in the given diagram.
        If no diagram is supplied a collection with all the nodes in the Project will be returned.
        :type diagram: Diagram
        :rtype: set
        """
        return self.index.nodes(diagram)

    def predicateNum(self, item, diagram=None):
        """
        Returns the number of predicates of the given type which are defined in the given diagram.
        If no diagram is supplied, the counting is extended to the whole Project.
        :type item: Item
        :type diagram: Diagram
        :rtype: int
        """
        return self.index.predicateNum(item, diagram)

    def predicates(self, item=None, name=None, diagram=None):
        """
        Returns a collection of predicate nodes belonging to the given diagram.
        If no diagram is supplied the lookup is performed across the whole Project.
        :type item: Item
        :type name: str
        :type diagram: Diagram
        :rtype: set
        """
        return self.index.predicates(item, name, diagram)

    def removeDiagram(self, diagram):
        """
        Remove the given diagram from the project index, together with all its items.
        :type diagram: Diagram
        """
        if self.index.removeDiagram(diagram):
            for item in self.items(diagram):
                diagram.sgnItemRemoved.emit(diagram, item)
            self.sgnDiagramRemoved.emit(diagram)

    def setMeta(self, item, name, meta):
        """
        Set metadata for the given predicate type/name combination.
        :type item: Item
        :type name: str
        :type meta: dict
        """
        if self.index.setMeta(item, name, meta):
            self.sgnMetaAdded.emit(item, name)

    def unsetMeta(self, item, name):
        """
        Remove metadata for the given predicate type/name combination.
        :type item: Item
        :type name: str
        """
        if self.index.unsetMeta(item, name):
            self.sgnMetaRemoved.emit(item, name)

    #############################################
    #   SLOTS
    #################################

    @QtCore.pyqtSlot('QGraphicsScene', 'QGraphicsItem')
    def doAddItem(self, diagram, item):
        """
        Executed whenever an item is added to a diagram belonging to this Project.
        :type diagram: Diagram
        :type item: AbstractItem
        """
        if self.index.addItem(diagram, item):
            self.sgnItemAdded.emit(diagram, item)

    @QtCore.pyqtSlot('QGraphicsScene', 'QGraphicsItem')
    def doRemoveItem(self, diagram, item):
        """
        Executed whenever an item is removed from a diagram belonging to this project.
        This slot will remove the given element from the project index.
        :type diagram: Diagram
        :type item: AbstractItem
        """
        if self.index.removeItem(diagram, item):
            self.sgnItemRemoved.emit(diagram, item)


class ProjectIndex(dict):
    """
    Extends built-in dict and implements the Project index.
    """
    def __init__(self):
        """
        Initialize the Project Index.
        """
        super().__init__(self)
        self[K_DIAGRAM] = dict()
        self[K_EDGE] = dict()
        self[K_ITEMS] = dict()
        self[K_NODE] = dict()
        self[K_PREDICATE] = dict()
        self[K_TYPE] = dict()

    def addDiagram(self, diagram):
        """
        Add the given diagram to the Project index.
        :type diagram: Diagram
        :rtype: bool
        """
        if diagram.name not in self[K_DIAGRAM]:
            self[K_DIAGRAM][diagram.name] = diagram
            return True
        return False

    def addItem(self, diagram, item):
        """
        Add the given item to the Project index.
        :type diagram: Diagram
        :type item: AbstractItem
        :rtype: bool
        """
        i = item.type()
        if diagram.name not in self[K_ITEMS]:
            self[K_ITEMS][diagram.name] = dict()
        if item.id not in self[K_ITEMS][diagram.name]:
            self[K_ITEMS][diagram.name][item.id] = item
            if diagram.name not in self[K_TYPE]:
                self[K_TYPE][diagram.name] = dict()
            if i not in self[K_TYPE][diagram.name]:
                self[K_TYPE][diagram.name][i] = set()
            self[K_TYPE][diagram.name][i] |= {item}
            if item.isNode():
                if diagram.name not in self[K_NODE]:
                    self[K_NODE][diagram.name] = dict()
                self[K_NODE][diagram.name][item.id] = item
                if item.isPredicate():
                    k = OWLText(item.text())
                    if i not in self[K_PREDICATE]:
                        self[K_PREDICATE][i] = dict()
                    if k not in self[K_PREDICATE][i]:
                        self[K_PREDICATE][i][k] = {K_NODE: dict()}
                    if diagram.name not in self[K_PREDICATE][i][k][K_NODE]:
                        self[K_PREDICATE][i][k][K_NODE][diagram.name] = set()
                    self[K_PREDICATE][i][k][K_NODE][diagram.name] |= {item}
            if item.isEdge():
                if diagram.name not in self[K_EDGE]:
                    self[K_EDGE][diagram.name] = dict()
                self[K_EDGE][diagram.name][item.id] = item
            return True
        return False

    def diagram(self, did):
        """
        Retrieves a diagram given its id.
        :type did: str
        :rtype: Diagram
        """
        try:
            return self[K_DIAGRAM][did]
        except KeyError:
            return None

    def diagrams(self):
        """
        Returns a collection with all the diagrams in this Project Index.
        :rtype: set
        """
        return set(self[K_DIAGRAM].values())

    def edge(self, diagram, eid):
        """
        Retrieves an edge given it's id and the diagram the edge belongs to.
        :type diagram: Diagram
        :type eid: str
        :rtype: AbstractEdge
        """
        try:
            return self[K_EDGE][diagram.name][eid]
        except KeyError:
            return None

    def edges(self, diagram=None):
        """
        Returns a collection with all the edges in the given diagram.
        If no diagram is supplied a collection with all the edges in the Project Index will be returned.
        :type diagram: Diagram
        :rtype: set
        """
        try:
            if not diagram:
                return set.union(*(set(self[K_EDGE][i].values()) for i in self[K_EDGE]))
            return set(self[K_EDGE][diagram.name].values())
        except (KeyError, TypeError):
            return set()

    def isEmpty(self):
        """
        Returns True if the Project Index contains no element, False otherwise.
        :rtype: bool
        """
        for i in self[K_ITEMS]:
            for _ in self[K_ITEMS][i]:
                return False
        return True

    def item(self, diagram, iid):
        """
        Retrieves an item given it's id and the diagram the edge belongs to.
        :type diagram: Diagram
        :type iid: str
        :rtype: AbstractItem
        """
        try:
            return self[K_ITEMS][diagram.name][iid]
        except KeyError:
            return None

    def itemNum(self, item, diagram=None):
        """
        Count the number of items of the given type which belongs to the given diagram.
        If no diagram is supplied, the counting is extended to the whole Project.
        :type item: Item
        :type diagram: Diagram
        :rtype: int
        """
        try:
            subdict = self[K_TYPE]
            if not diagram:
                return len(set.union(*(subdict[i][item] for i in subdict if item in subdict[i])))
            return len(subdict[diagram.name][item])
        except (KeyError, TypeError):
            return 0

    def items(self, diagram=None):
        """
        Returns a collection with all the items in the given diagram.
        If no diagram is supplied a collection with all the items in the Project Index will be returned.
        :type diagram: Diagram
        :rtype: set
        """
        try:
            if not diagram:
                return set.union(*(set(self[K_ITEMS][i].values()) for i in self[K_ITEMS]))
            return set(self[K_ITEMS][diagram.name].values())
        except (KeyError, TypeError):
            return set()

    def meta(self, item, name):
        """
        Retrieves metadata for the given predicate, expressed as pair (item, name).
        :type item: Item
        :type name: str
        :rtype: dict
        """
        try:
            name = OWLText(name)
            return self[K_PREDICATE][item][name][K_META]
        except KeyError:
            return dict()

    def metas(self, *types):
        """
        Retrieves a collection of pairs 'item', 'name' for all the predicates with metadata.
        :type types: list
        :rtype: list
        """
        filter_ = lambda x: not types or x in types
        return [(k1, k2) for k1 in self[K_PREDICATE] \
                            for k2 in self[K_PREDICATE][k1] \
                                if filter_(k1) and K_META in self[K_PREDICATE][k1][k2]]

    def node(self, diagram, nid):
        """
        Retrieves the node matching the given id or None if no node is found.
        :type diagram: Diagram
        :type nid: str
        :rtype: AbstractNode
        """
        try:
            return self[K_EDGE][diagram.name][nid]
        except KeyError:
            return None

    def nodes(self, diagram=None):
        """
        Returns a collection with all the nodes in the given diagram.
        If no diagram is supplied a collection with all the nodes in the Project Index will be returned.
        :type diagram: Diagram
        :rtype: set
        """
        try:
            if not diagram:
                return set.union(*(set(self[K_NODE][i].values()) for i in self[K_NODE]))
            return set(self[K_NODE][diagram.name].values())
        except (KeyError, TypeError):
            return set()

    def predicateNum(self, item, diagram=None):
        """
        Count the number of predicates of the given type which are defined in the given diagram.
        If no diagram is supplied, the counting is extended to the whole Project Index.
        :type item: Item
        :type diagram: Diagram
        :rtype: int
        """
        try:
            subdict = self[K_PREDICATE]
            if not diagram:
                return len(subdict[item])
            return len({i for i in subdict[item] if diagram.name in subdict[item][i][K_NODE]})
        except (KeyError, TypeError):
            return 0
    
    def predicates(self, item=None, name=None, diagram=None):
        """
        Returns a collection of predicate nodes belonging to the given diagram.
        If no diagram is supplied the lookup is performed across the whole Project Index.
        :type item: Item
        :type name: str
        :type diagram: Diagram
        :rtype: set
        """
        try:

            if not item and not name:
                collection = set()
                if not diagram:
                    for i in self[K_PREDICATE]:
                        for j in self[K_PREDICATE][i]:
                            collection.update(*self[K_PREDICATE][i][j][K_NODE].values())
                else:
                    for i in self[K_PREDICATE]:
                        for j in self[K_PREDICATE][i]:
                            collection.update(self[K_PREDICATE][i][j][K_NODE][diagram.name])
                return collection

            if item and not name:
                collection = set()
                if not diagram:
                    for i in self[K_PREDICATE][item]:
                        collection.update(*self[K_PREDICATE][item][i][K_NODE].values())
                else:
                    for i in self[K_PREDICATE][item]:
                        collection.update(self[K_PREDICATE][item][i][K_NODE][diagram.name])
                return collection

            if not item and name:
                collection = set()
                name = OWLText(name)
                if not diagram:
                    for i in self[K_PREDICATE]:
                        collection.update(*self[K_PREDICATE][i][name][K_NODE].values())
                else:
                    for i in self[K_PREDICATE]:
                        collection.update(self[K_PREDICATE][i][name][K_NODE][diagram.name])
                return collection

            if item and name:
                name = OWLText(name)
                if not diagram:
                    return set.union(*self[K_PREDICATE][item][name][K_NODE].values())
                return self[K_PREDICATE][item][name][K_NODE][diagram.name]

        except KeyError:
            return set()
        
    def removeDiagram(self, diagram):
        """
        Remove the given diagram from the Project index.
        :type diagram: Diagram
        :rtype: bool
        """
        if diagram.name in self[K_DIAGRAM]:
            del self[K_DIAGRAM][diagram.name]
            return True
        return False

    def removeItem(self, diagram, item):
        """
        Remove the given item from the Project index.
        :type diagram: Diagram
        :type item: AbstractItem
        :rtype: bool
        """
        i = item.type()
        if diagram.name in self[K_ITEMS]:
            if item.id in self[K_ITEMS][diagram.name]:
                del self[K_ITEMS][diagram.name][item.id]
                if not self[K_ITEMS][diagram.name]:
                    del self[K_ITEMS][diagram.name]
            if diagram.name in self[K_TYPE]:
                if i in self[K_TYPE][diagram.name]:
                    self[K_TYPE][diagram.name][i] -= {item}
                    if not self[K_TYPE][diagram.name][i]:
                        del self[K_TYPE][diagram.name][i]
                        if not self[K_TYPE][diagram.name]:
                            del self[K_TYPE][diagram.name]
            if item.isNode():
                if diagram.name in self[K_NODE]:
                    if item.id in self[K_NODE][diagram.name]:
                        del self[K_NODE][diagram.name][item.id]
                        if not self[K_NODE][diagram.name]:
                            del self[K_NODE][diagram.name]
                if item.isPredicate():
                    k = OWLText(item.text())
                    if i in self[K_PREDICATE]:
                        if k in self[K_PREDICATE][i]:
                            if diagram.name in self[K_PREDICATE][i][k][K_NODE]:
                                self[K_PREDICATE][i][k][K_NODE][diagram.name] -= {item}
                                if not self[K_PREDICATE][i][k][K_NODE][diagram.name]:
                                    del self[K_PREDICATE][i][k][K_NODE][diagram.name]
                                    if not self[K_PREDICATE][i][k][K_NODE]:
                                        del self[K_PREDICATE][i][k]
                                        if not self[K_PREDICATE][i]:
                                            del self[K_PREDICATE][i]
            if item.isEdge():
                if diagram.name in self[K_EDGE]:
                    if item.id in self[K_EDGE][diagram.name]:
                        del self[K_EDGE][diagram.name][item.id]
                        if not self[K_EDGE][diagram.name]:
                            del self[K_EDGE][diagram.name]
            return True
        return False
                
    def setMeta(self, item, name, meta):
        """
        Set metadata for the given predicate type/name combination.
        :type item: Item
        :type name: str
        :type meta: dict
        :rtype: bool
        """
        try:
            name = OWLText(name)
            self[K_PREDICATE][item][name][K_META] = meta
        except KeyError:
            return False
        else:
            return True

    def unsetMeta(self, item, name):
        """
        Unset metadata for the given predicate type/name combination.
        :type item: Item
        :type name: str
        :rtype: bool
        """
        name = OWLText(name)
        if item in self[K_PREDICATE]:
            if name in self[K_PREDICATE][item]:
                if K_META in self[K_PREDICATE][item][name]:
                    del self[K_PREDICATE][item][name][K_META]
                    return True
        return False


class ProjectMergeWorker(QtCore.QObject):
    """
    Extends QObject with facilities to merge the content of 2 distinct projects.
    """
    def __init__(self, project, other, session):
        """
        Initialize the project merge worker.
        :type project: Project
        :type other: Project
        :type session: Session
        """
        super().__init__(session)
        self.commands = list()
        self.project = project
        self.other = other

    #############################################
    #   PROPERTIES
    #################################

    @property
    def session(self):
        """
        Returns the reference to the active session (alias for ProjectMergeWorker.parent()).
        :rtype: Session
        """
        return self.parent()

    #############################################
    #   INTERFACE
    #################################

    def mergeDiagrams(self):
        """
        Perform the merge of the diagrams by importing all the diagrams in the 'other' project in the loaded one.
        """
        for diagram in self.other.diagrams():
            # We may be in the situation in which we are importing a diagram with name 'X'
            # even though we already have a diagram 'X' in our project. Because we do not
            # want to overwrite diagrams, we perform a rename of the diagram being imported,
            # to be sure to have a unique diagram name, in the current project namespace.
            occurrence = 1
            name = diagram.name
            while self.project.diagram(diagram.name):
                diagram.name = '{0}_{1}'.format(name, occurrence)
                occurrence += 1
            ## SWITCH SIGNAL SLOTS
            disconnect(diagram.sgnItemAdded, self.other.doAddItem)
            disconnect(diagram.sgnItemRemoved, self.other.doRemoveItem)
            connect(diagram.sgnItemAdded, self.project.doAddItem)
            connect(diagram.sgnItemRemoved, self.project.doRemoveItem)
            ## MERGE THE DIAGRAM IN THE CURRENT PROJECT
            self.commands.append(CommandDiagramAdd(diagram, self.project))

    def mergeMeta(self):
        """
        Perform the merge of predicates metadata.
        """
        conflicts = dict()
        resolutions = dict()

        for item, name in self.other.metas():
            if not self.project.predicates(item, name):
                ## NO PREDICATE => NO CONFLICT
                undo = self.project.meta(item, name).copy()
                redo = self.other.meta(item, name).copy()
                self.commands.append(CommandNodeSetMeta(self.project, item, name, undo, redo))
            else:
                ## CHECK FOR POSSIBLE CONFLICTS
                metac = self.project.meta(item, name)
                metai = self.other.meta(item, name)
                if metac != metai:
                    if item not in conflicts:
                        conflicts[item] = dict()
                    conflicts[item][name] = {K_CURRENT: metac.copy(), K_IMPORTING: metai.copy()}
                    if item not in resolutions:
                        resolutions[item] = dict()
                    resolutions[item][name] = metac.copy()

        ## RESOLVE CONFLICTS
        aconflicts = []
        for item in conflicts:
            for name in conflicts[item]:
                metac = conflicts[item][name][K_CURRENT]
                metai = conflicts[item][name][K_IMPORTING]
                ## RESOLVE DOCUMENTATION CONFLICTS
                docc = metac.get(K_DESCRIPTION, '')
                doci = metai.get(K_DESCRIPTION, '')
                if docc != doci:
                    resolver = PredicateDocumentationConflictResolver(item, name, docc, doci)
                    if resolver.exec_() == PredicateDocumentationConflictResolver.Rejected:
                        raise ProjectStopImportingError
                    resolutions[item][name][K_DESCRIPTION] = resolver.result()
                ## COLLECT ASSERTIONS CONFLICTS FOR ATTRIBUTES
                if item is Item.AttributeNode:
                    vc = metac.get(K_FUNCTIONAL, False)
                    vi = metai.get(K_FUNCTIONAL, False)
                    if vc != vi:
                        aconflicts.append({
                            K_ITEM: item,
                            K_NAME: name,
                            K_PROPERTY: K_FUNCTIONAL,
                            K_CURRENT: vc,
                            K_IMPORTING: vi
                        })
                ## COLLECT ASSERTIONS CONFLICTS FOR ROLES
                if item is Item.RoleNode:
                    for k in (K_ASYMMETRIC, K_INVERSE_FUNCTIONAL, K_IRREFLEXIVE, K_REFLEXIVE, K_SYMMETRIC, K_TRANSITIVE):
                        vc = metac.get(k, False)
                        vi = metai.get(k, False)
                        if vc != vi:
                            aconflicts.append({
                                K_ITEM: item,
                                K_NAME: name,
                                K_PROPERTY: k,
                                K_CURRENT: vc,
                                K_IMPORTING: vi
                            })

        ## RESOLVE BOOLEAN PROPERTIES CONFLICTS
        if aconflicts:
            resolver = PredicateBooleanConflictResolver(aconflicts)
            if resolver.exec_() == PredicateBooleanConflictResolver.Rejected:
                raise ProjectStopImportingError
            for e in resolver.results():
                resolutions[e[K_ITEM]][e[K_NAME]][e[K_PROPERTY]] = e[K_FINAL]

        ## GENERATE UNDOCOMMANDS FOR RESOLUTIONS
        for item in resolutions:
            for name in resolutions[item]:
                undo = self.project.meta(item, name)
                redo = resolutions[item][name]
                self.commands.append(CommandNodeSetMeta(self.project, item, name, undo, redo))

    def mergeFinished(self):
        """
        Completes the merge by executing the commands in the buffer on the undostack.
        """
        if self.commands:
            self.session.undostack.beginMacro('import project "{0}" into "{1}"'.format(self.other.name, self.project.name))
            for command in self.commands:
                self.session.undostack.push(command)
            self.session.undostack.endMacro()

    def run(self):
        """
        Perform the merge of the 2 projects.
        """
        try:
            LOGGER.info('Performing project import: %s <- %s...', self.project.name, self.other.name)
            self.mergeDiagrams()
            self.mergeMeta()
        except ProjectStopImportingError:
            pass
        else:
            self.mergeFinished()


class ProjectNotFoundError(RuntimeError):
    """
    Raised whenever we are not able to find a project given its path.
    """
    pass


class ProjectNotValidError(RuntimeError):
    """
    Raised whenever a found project has an invalid structure.
    """
    pass


class ProjectStopLoadingError(RuntimeError):
    """
    Used to signal that a project loading needs to be interrupted.
    """
    pass


class ProjectStopImportingError(RuntimeError):
    """
    Used to signal that a project import needs to be interrupted.
    """
    pass


class ProjectVersionError(RuntimeError):
    """
    Raised whenever we have a project version mismatch.
    """
    pass