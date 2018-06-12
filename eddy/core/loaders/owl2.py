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

from PyQt5 import QtCore, QtGui
from eddy.core.loaders.common import AbstractOntologyLoader
from eddy.core.output import getLogger
from eddy.core.datatypes.system import File
from jnius import autoclass, cast, detach

from eddy.core.clipboard import Clipboard
from eddy.core.datatypes.graphol import Item, Identity
from eddy.core.functions.misc import snap
from eddy.core.items.factory import ItemFactory
from eddy.core.diagram import Diagram
from eddy.core.commands.nodes_2 import CommandProjetSetIRIPrefixesNodesDict
from eddy.core.commands.project import CommandProjectDisconnectSpecificSignals, CommandProjectConnectSpecificSignals
from eddy.core.commands.edges import CommandEdgeAdd
from eddy.core.commands.nodes import CommandNodeAdd
from eddy.core.datatypes.owl import OWLStandardIRIPrefixPairsDict

LOGGER = getLogger()


class OWL2OntologyLoader(AbstractOntologyLoader):
    """
    Extends AbstractOntologyLoader with facilities to load ontologies from OWL file format.
    """
    def __init__(self, path, project, session):
        """
        Initialize the GraphML importer.
        :type path: str
        :type project: Project
        :type session: Session
        """
        super().__init__(path, project, session)

        self.DefaultPrefixManager = autoclass('org.semanticweb.owlapi.util.DefaultPrefixManager')
        self.IRI = autoclass('org.semanticweb.owlapi.model.IRI')
        self.OWLClassExpression = autoclass('org.semanticweb.owlapi.model.OWLClassExpression')
        self.OWLIndividual = autoclass('org.semanticweb.owlapi.model.OWLIndividual')
        self.OWLAxiom = autoclass('org.semanticweb.owlapi.model.OWLAxiom')
        self.AxiomType = autoclass('org.semanticweb.owlapi.model.AxiomType')
        self.OWLOntology = autoclass('org.semanticweb.owlapi.model.OWLOntology')
        self.PrefixManager = autoclass('org.semanticweb.owlapi.model.PrefixManager')
        self.OWLManager = autoclass('org.semanticweb.owlapi.apibinding.OWLManager')
        self.OWLOntologyID = autoclass('org.semanticweb.owlapi.model.OWLOntologyID')

        self.FunctionalSyntaxDocumentFormat = autoclass('org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat')
        self.ManchesterSyntaxDocumentFormat = autoclass('org.semanticweb.owlapi.formats.ManchesterSyntaxDocumentFormat')

        self.OWLAnnotationValue = autoclass('org.semanticweb.owlapi.model.OWLAnnotationValue')
        self.OWLFacet = autoclass('org.semanticweb.owlapi.vocab.OWLFacet')
        self.OWL2Datatype = autoclass('org.semanticweb.owlapi.vocab.OWL2Datatype')

        self.OWLOntologyDocumentTarget = autoclass('org.semanticweb.owlapi.io.OWLOntologyDocumentTarget')
        self.StringDocumentTarget = autoclass('org.semanticweb.owlapi.io.StringDocumentTarget')
        self.TurtleDocumentFormat = autoclass('org.semanticweb.owlapi.formats.TurtleDocumentFormat')
        self.RDFXMLDocumentFormat = autoclass('org.semanticweb.owlapi.formats.RDFXMLDocumentFormat')

        self.File = autoclass('java.io.File')
        self.Set = autoclass('java.util.Set')
        self.LinkedList = autoclass('java.util.LinkedList')
        self.List = autoclass('java.util.List')
        self.HashSet = autoclass('java.util.HashSet')

        self.project = project
        self.syntax = None

        self.df = None
        self.man = None
        self.ontology = None
        self.pm = None

        self.axiom_graph_dict = dict()
        self.axiom_metadata_dict = dict()
        self.diag_entity_axioms_dict = dict()

        self.occupied_positions_xy = [[0,0]]

    def process_Class_Expression_Axiom(self,axiom):
        ### https://www.w3.org/TR/owl2-quick-reference/
        # Class Expression Axioms
        #
        #   SubClassOf(C1 C2)
        #   EquivalentClasses(C1 … Cn)
        #   DisjointClasses(C1 C2)
        #   DisjointClasses(C1 … Cn)
        #   DisjointUnionOf(CN C1 … Cn)
        ###
        pass

    def process_Object_Property_Axiom(self,axiom):
        ###
        # Object_Property_Axioms
        #
        #   SubObjectPropertyOf(P1 P2)
        #   SubObjectPropertyOf(ObjectPropertyChain(P1 … Pn) P)
        #   ObjectPropertyDomain(P C)
        #   ObjectPropertyRange(P C)
        #   EquivalentObjectProperties(P1 … Pn)
        #   DisjointObjectProperties(P1 P2)
        #   DisjointObjectProperties(P1 … Pn)
        #   InverseObjectProperties(P1 P2)
        #   FunctionalObjectProperty(P)
        #   InverseFunctionalObjectProperty(P)
        #   ReflexiveObjectProperty(P)
        #   IrreflexiveObjectProperty(P)
        #   SymmetricObjectProperty(P)
        #   AsymmetricObjectProperty(P)
        #   TransitiveObjectProperty(P)
        pass

    def process_Data_Property_Axiom(self,axiom):
        ###
        #   Data_Property_Axioms
        #
        #   SubDataPropertyOf(R1 R2)
        #   DataPropertyDomain(R C)
        #   DataPropertyRange(R D)
        #   EquivalentDataProperties(R1 … Rn)
        #   DisjointDataProperties(R1 R2)
        #   DisjointDataProperties(R1 … Rn)
        #   FunctionalDataProperty(R)

        pass

    def process_Datatype_Definitions(self):
        ###
        #   DatatypeDefinition(DN D)
        pass

    def process_Assertion(self,axiom):

        diagram = list(self.project.diagrams())[0]

        print('process_Assertion_axiom >>>')

        ###
        #   Assertions
        #
        #   SameIndividual(a1 … an)
        #   DifferentIndividuals(a1 a2)
        #   DifferentIndividuals(a1 … an)
        #   ClassAssertion(C a)
        print('axiom.getAxiomType().toString()',axiom.getAxiomType().toString())
        if axiom.getAxiomType().toString() == self.AxiomType.CLASS_ASSERTION.toString():

            class_expression = axiom.getClassExpression()
            cast(self.OWLClassExpression,class_expression)
            print('class_expression.toString()',class_expression.toString())
            nodeA = diagram.factory.create(Item.ConceptNode)

            full_iriA = class_expression.toString()[1:len(class_expression.toString()) - 1]
            resA = self.project.get_iri_and_rc_from_full_iri(full_iriA)
            iriA = resA[0]
            rcA = resA[1]

            nodeA.setText(full_iriA)
            nodeA.remaining_characters = rcA

            individual = axiom.getIndividual()
            cast(self.OWLIndividual,individual)
            print('individual.toString()', individual.toString())
            nodeB = diagram.factory.create(Item.IndividualNode)

            full_iriB = individual.toString()[1:len(individual.toString()) - 1]
            resB = self.project.get_iri_and_rc_from_full_iri(full_iriB)
            iriB = resB[0]
            rcB = resB[1]

            nodeB.setText(full_iriB)
            nodeB.remaining_characters = rcB

            last_position_xy = self.occupied_positions_xy[-1]
            if last_position_xy[0] > 2000:
                new_xA = 0
                new_yA = last_position_xy[1] + 100
            else:
                new_xA = last_position_xy[0] + 300
                new_yA = last_position_xy[1]
            self.occupied_positions_xy.append([new_xA, new_yA])

            last_position_xy = self.occupied_positions_xy[-1]
            if last_position_xy[0] > 2000:
                new_xB = 0
                new_yB = last_position_xy[1] + 100
            else:
                new_xB = last_position_xy[0] + 300
                new_yB = last_position_xy[1]
            self.occupied_positions_xy.append([new_xB, new_yB])

            snapToGrid = self.session.action('toggle_grid').isChecked()
            nodeA.setPos(snap(QtCore.QPoint(new_xA, new_yA), Diagram.GridSize, snapToGrid))
            nodeB.setPos(snap(QtCore.QPoint(new_xB, new_yB), Diagram.GridSize, snapToGrid))

            edge = diagram.factory.create(Item.MembershipEdge, source=nodeB)

            nodeB.updateNode(selected=False)
            currentNode = nodeA
            insertEdge = False

            if currentNode:
                currentNode.updateNode(selected=False)
                pvr = diagram.project.profile.checkEdge(nodeB, edge, nodeA)
                if pvr.isValid():
                    edge.target = currentNode
                    insertEdge = True

            # We temporarily remove the item from the diagram and we perform the
            # insertion using the undo command that will also emit the sgnItemAdded
            # signal hence all the widgets will be notified of the edge insertion.
            # We do this because while creating the edge we need to display it so the
            # user knows what he is connecting, but we don't want to truly insert
            # it till it's necessary (when the mouse is released and the validation
            # confirms that the generated expression is a valid graphol expression).
            diagram.removeItem(edge)

            if insertEdge:
                return ['ClassAssertion',[nodeA,nodeB],[iriA,iriB],[edge]]
        else:
            return None
        #   ObjectPropertyAssertion( PN a1 a2 )
        #   DataPropertyAssertion( R a v )
        #   NegativeObjectPropertyAssertion(P a1 a2 )
        #   NegativeDataPropertyAssertion(R a v )
        print('process_Assertion_axiom >>>')

    def process_Key(self,axiom):
        ###
        #  HasKey(C (P1 … Pm) (R1 … Rn) )
        pass

    def process_Declaration(self,axiom):
        ###
        #   Declarations
        #
        #   create a new entity; place it in the diagram.

        flag = True
        print('')
        print('axiom.toString()',axiom.toString())

        entity = axiom.getEntity()

        diagram = list(self.project.diagrams())[0]

        #   Declaration( Class( CN ) )
        if entity.isOWLClass():
            node = diagram.factory.create(Item.ConceptNode)
        # Declaration( ObjectProperty( PN ) )
        elif entity.isOWLObjectProperty():
            node = diagram.factory.create(Item.RoleNode)
        # Declaration( DataProperty( R ) )
        elif entity.isOWLDataProperty():
            node = diagram.factory.create(Item.AttributeNode)
        #   Declaration( NamedIndividual( aN ) )
        elif entity.isOWLNamedIndividual():
            node = diagram.factory.create(Item.IndividualNode)
        #   Declaration( Datatype( DN ) )
        #elif entity.isOWLDatatype():
        #    pass
        #   Declaration( AnnotationProperty( A ) )
        #elif entity.isOWLAnnotationProperty():
        #    pass
        #elif entity.isTopEntity():
        #    pass
        #elif entity.isBottomEntity():
        #    pass
        else:
            flag = False

        print('entity.toString()',entity.toString())
        print('entity.getIRI().toString()', entity.getIRI().toString())
        print('axiom.getEntity().getEntityType()', entity.getEntityType().toString())

        if flag is False:
            return None

        full_iri = entity.toString()[1:len(entity.toString()) - 1]
        res = self.project.get_iri_and_rc_from_full_iri(full_iri)
        iri = res[0]
        rc = res[1]

        node.setText(full_iri)
        node.remaining_characters = rc

        """
        else:
            node.setText(entity.toString())
            node.remaining_characters = entity.toString()[entity.toString().rindex(':')+1:len(entity.toString())]

            prefix = entity.toString()[0:entity.toString().index(':')]
            print('prefix',prefix)

            for std_iri in OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict.keys():
                std_prefix = OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict[std_iri]
                if std_prefix == prefix:
                    iri = std_iri
                    break

            print('node.identity()',node.identity())
        """
        print('node.text()', node.text())
        print('node.remaining_characters',node.remaining_characters)

        last_position_xy = self.occupied_positions_xy[-1]
        if last_position_xy[0] >2000:
            new_x = 0
            new_y = last_position_xy[1] + 100
        else:
            new_x = last_position_xy[0] + 300
            new_y = last_position_xy[1]
        self.occupied_positions_xy.append([new_x, new_y])

        snapToGrid = self.session.action('toggle_grid').isChecked()
        node.setPos(snap(QtCore.QPoint(new_x, new_y), Diagram.GridSize, snapToGrid))

        return ['Declaration',node,iri]

    def process_Annotation(self):
        ###
        #   Annotations
        #
        #   AnnotationAssertion(A s t)
        #   AXIOM(Annotation(A t) …)
        #   AXIOM(Annotation(A t) … )
        #   Annotation(Annotation(A t) … A1 t1)
        pass

    def process_Annotation_Axiom(self,axiom):
        ###
        # Annotation Axioms
        #
        #   SubAnnotationPropertyOf(A1 A2)
        #   AnnotationPropertyDomain(A U)
        #   AnnotationPropertyRange(A U)
        pass

    def process_Ontology(self):
        ###
        #   Ontologies
        #
        #   Ontology([ON [U]] Import(ON1)... Annotation(A t) ...)
        #   Prefix(p=U)
        pass

    def Process_annotated_axiom(self):

        self.process_Annotation()
        self.process_Annotation_Axiom()

        self.process_Ontologie()

    def Process_annotation_axiom(self):

        self.process_Annotation()
        self.process_Annotation_Axiom()

        self.process_Ontology()

    def Process_bottom_entity(self):
        pass

    def Process_top_entity(self):
        pass

    def Process_logical_axiom(self,axiom_to_process):

        print('Process_logical_axiom >>>')



        #print('axiom.getAxiomType().getName()',axiom_to_process.getAxiomType().getName())
        #print('axiom.getAxiomType().getIndex()', axiom_to_process.getAxiomType().getIndex())
        #print('axiom.getAxiomType().toString()', axiom_to_process.getAxiomType().toString())

        #print('axiom.toString()', axiom_to_process.toString())

        self.process_Class_Expression_Axiom(axiom_to_process)
        self.process_Object_Property_Axiom(axiom_to_process)
        self.process_Data_Property_Axiom(axiom_to_process)

        #self.process_Datatype_Definitions()

        op=self.process_Assertion(axiom_to_process)

        #self.process_Keys()

        print('Process_logical_axiom >>> END')

        return op

    def Process_other_type_of_axiom(self):

        self.process_Datatype_Definition()

        self.process_Key()

        self.process_Annotation()
        self.process_Annotation_Axiom()

        self.process_Ontology()

    def fetch_ontology_from_file(self,filename_inp):

        try:
            file = self.File(filename_inp)
            #print('file.exists()',file.exists())
            self.ontology = self.man.loadOntologyFromOntologyDocument(file);
            cast(self.OWLOntology, self.ontology)
        except():
            print('error reading file')

    def convert_axioms_to_nodes_and_edges_or_metadata(self):

        commands = []

        declaration_axioms = self.ontology.getAxioms(self.AxiomType.DECLARATION)
        cast(self.Set, declaration_axioms)
        declaration_axioms_itr = declaration_axioms.iterator()
        while declaration_axioms_itr.hasNext():

            decl_axiom_to_process = declaration_axioms_itr.next()
            #cast(self.OWLAxiom, decl_axiom_to_process)
            op = self.process_Declaration(decl_axiom_to_process)
            if op is not None:
                commands.append(op)

        axioms_in_ontology = self.ontology.getAxioms()
        cast(self.Set,axioms_in_ontology)
        axioms_in_ontology_itr = axioms_in_ontology.iterator()
        while axioms_in_ontology_itr.hasNext():

            axiom_to_process = axioms_in_ontology_itr.next()
            cast(self.OWLAxiom, axiom_to_process)

            flag = False

            if axiom_to_process.isAnnotated():
                #self.process_annotated_axiom(axiom_to_process)
                flag = True

            if axiom_to_process.isAnnotationAxiom():
                #self.process_annotation_axiom(axiom_to_process)
                flag = True

            if axiom_to_process.isBottomEntity():
                #self.process_bottom_entity(axiom_to_process)
                flag = True

            if axiom_to_process.isTopEntity():
                #self.process_top_entity(axiom_to_process)
                flag = True

            if axiom_to_process.isLogicalAxiom():
                op=self.Process_logical_axiom(axiom_to_process)
                if op is not None:
                    commands.append(op)
                flag = True

            if flag is False:
                #self.process_other_type_of_axiom(axiom_to_process)
                pass

        return commands

    def place_graphs_in_a_diagram(self):

        pass

    @classmethod
    def filetype(cls):
        """
        Returns the type of the file that will be used for the import.
        :return: File
        """
        return File.Owl

    def run(self):

        self.man = self.OWLManager.createOWLOntologyManager()
        self.df = self.man.getOWLDataFactory()
        self.pm = self.DefaultPrefixManager()

        all_commands = []

        print('self.path',self.path)
        self.fetch_ontology_from_file(self.path)
        #self.fetch_ontology_from_file('C://Users/Ashwin/Desktop/Pizza.owl')

        all_commands.extend(self.convert_axioms_to_nodes_and_edges_or_metadata())

        print('len(all_commands)',len(all_commands))

        Duplicate_dict_1 = self.project.copy_IRI_prefixes_nodes_dictionaries(self.project.IRI_prefixes_nodes_dict,
                                                                             dict())
        Duplicate_dict_2 = self.project.copy_IRI_prefixes_nodes_dictionaries(self.project.IRI_prefixes_nodes_dict,
                                                                             dict())

        diagram = list(self.project.diagrams())[0]

        commands_session = []

        all_iris = set()

        for cmd in all_commands:
            print('cmd',cmd)
            type = cmd[0]
            if type == 'Declaration':
                node = cmd[1]
                iri = cmd[2]

                all_iris.add(iri)

                Duplicate_dict_1 = self.project.addIRINodeEntry(Duplicate_dict_1, iri, node)

                commands_session.append(CommandNodeAdd(diagram, node))

            elif type =='ClassAssertion':
                nodes = cmd[1]
                iris = cmd[2]
                edges = cmd[3]

                all_iris.union(iri)

                for c in range(0,len(nodes)):

                    nd = nodes[c]
                    iri = iris[c]

                    Duplicate_dict_1 = self.project.addIRINodeEntry(Duplicate_dict_1, iri, nd)

                    commands_session.append(CommandNodeAdd(diagram, nd))

                for e in edges:
                    commands_session.append(CommandEdgeAdd(diagram, e))
                    e.updateEdge()

                    diagram.clearSelection()
                    diagram.project.profile.reset()

        commands_session.insert(0, CommandProjetSetIRIPrefixesNodesDict(self.project, Duplicate_dict_2, Duplicate_dict_1,
                                             list(all_iris), None))
        commands_session.append(CommandProjetSetIRIPrefixesNodesDict(self.project, Duplicate_dict_2, Duplicate_dict_1,
                                             list(all_iris), None))

        commands_session.insert(0, CommandProjectDisconnectSpecificSignals(self.project))

        commands_session.append(CommandProjectConnectSpecificSignals(self.project))

        if any(commands_session):
            self.session.undostack.beginMacro('')
            for c in commands_session:
                if c:
                    self.session.undostack.push(c)
            self.session.undostack.endMacro()

        self.place_graphs_in_a_diagram()


