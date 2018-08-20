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

from PyQt5 import QtCore, QtGui, QtWidgets
from eddy.core.loaders.common import AbstractOntologyLoader
from eddy.core.output import getLogger
from eddy.core.datatypes.system import File
from jnius import autoclass, cast, detach

from eddy.core.common import HasThreadingSystem, HasWidgetSystem
from eddy.core.clipboard import Clipboard
from eddy.core.datatypes.graphol import Item, Identity
from eddy.core.functions.misc import snap
from eddy.core.items.factory import ItemFactory
from eddy.core.diagram import Diagram
from eddy.core.commands.nodes_2 import CommandProjetSetIRIPrefixesNodesDict
from eddy.core.commands.project import CommandProjectDisconnectSpecificSignals, CommandProjectConnectSpecificSignals
from eddy.core.commands.edges import CommandEdgeAdd, CommandEdgesBreakpointsAdd, CommandEdgesAdd
from eddy.core.commands.nodes import CommandNodeAdd, CommandNodesAdd, CommandNodeSetMeta, CommandNodesSetMeta
from eddy.core.project import K_SYMMETRIC, K_TRANSITIVE, K_FUNCTIONAL, K_INVERSE_FUNCTIONAL, K_ASYMMETRIC, K_IRREFLEXIVE, K_REFLEXIVE
from eddy.core.commands.diagram import CommandDiagramResize
from eddy.core.datatypes.owl import OWLStandardIRIPrefixPairsDict
from eddy.core.worker import AbstractWorker
from eddy.core.exporters.owl2 import OWLOntologyExporterDialog
from eddy.core.functions.path import expandPath, openPath
import sys, math, time

from eddy import APPNAME, BUG_TRACKER, ORGANIZATION
from eddy.core.common import HasThreadingSystem, HasWidgetSystem
from eddy.core.datatypes.graphol import Item, Identity, Special, Restriction
from eddy.core.datatypes.owl import Datatype, Facet, OWLAxiom, OWLSyntax
from eddy.core.datatypes.qt import Font
from eddy.core.datatypes.system import File
from eddy.core.diagram import DiagramMalformedError
from eddy.core.exporters.common import AbstractOntologyExporter
from eddy.core.functions.fsystem import fwrite, fremove
from eddy.core.functions.misc import first, clamp, isEmpty, rtfStripFontAttributes
from eddy.core.functions.misc import rstrip, postfix, format_exception
from eddy.core.functions.owl import OWLFunctionalSyntaxDocumentFilter
from eddy.core.functions.owl import OWLManchesterSyntaxDocumentFilter
from eddy.core.functions.owl import RDFXMLDocumentFilter
from eddy.core.functions.owl import TurtleDocumentFilter
from eddy.core.functions.owl import OWLShortIRI, OWLAnnotationText
from eddy.core.functions.path import expandPath, openPath
from eddy.core.functions.signals import connect
from eddy.core.output import getLogger
from eddy.core.project import K_DESCRIPTION
from eddy.core.worker import AbstractWorker
from eddy.ui.DiagramsSelectionDialog import DiagramsSelectionDialog
from eddy.ui.fields import ComboBox, CheckBox
from eddy.ui.progress import BusyProgressDialog
from eddy.ui.syntax import SyntaxValidationWorker

from eddy import APPNAME, BUG_TRACKER, ORGANIZATION

LOGGER = getLogger()


class OWL2OntologyLoader(AbstractOntologyLoader, HasThreadingSystem):
    # class OWLOntologyImporterWorker(AbstractWorker):
    """
    Extends AbstractWorker with facilities to load ontologies from OWL file format.
    """
    sgnCompleted = QtCore.pyqtSignal()
    sgnErrored = QtCore.pyqtSignal(Exception)
    sgnProgress = QtCore.pyqtSignal(int, int)
    sgnStarted = QtCore.pyqtSignal()

    def __init__(self, path, project, session):
        # def __init__(self, session, project, path, **kwargs):
        """
        Initialize the GraphML importer.
        :type path: str
        :type project: Project
        """
        super().__init__(path, project, session)
        # super().__init__()

        # self.project = project
        # self.session = session
        # self.path = path

        print('self.project', self.project)
        print('self.session', self.session)

        self.DefaultPrefixManager = autoclass('org.semanticweb.owlapi.util.DefaultPrefixManager')
        self.IRI = autoclass('org.semanticweb.owlapi.model.IRI')
        self.Imports = autoclass('org.semanticweb.owlapi.model.parameters.Imports')

        self.OWLClass = autoclass('org.semanticweb.owlapi.model.OWLClass')
        self.OWLDataProperty = autoclass('org.semanticweb.owlapi.model.OWLDataProperty')
        self.OWLObjectProperty = autoclass('org.semanticweb.owlapi.model.OWLObjectProperty')
        self.OWLDataPropertyExpression = autoclass('org.semanticweb.owlapi.model.OWLDataPropertyExpression')
        self.OWLObjectPropertyExpression = autoclass('org.semanticweb.owlapi.model.OWLObjectPropertyExpression')
        self.OWLClassExpression = autoclass('org.semanticweb.owlapi.model.OWLClassExpression')
        self.OWLDataRange = autoclass('org.semanticweb.owlapi.model.OWLDataRange')
        self.OWLLiteral = autoclass('org.semanticweb.owlapi.model.OWLLiteral')
        self.OWLIndividual = autoclass('org.semanticweb.owlapi.model.OWLIndividual')
        self.OWLFacet = autoclass('org.semanticweb.owlapi.vocab.OWLFacet')
        self.OWL2Datatype = autoclass('org.semanticweb.owlapi.vocab.OWL2Datatype')

        self.OWLAxiom = autoclass('org.semanticweb.owlapi.model.OWLAxiom')
        self.AxiomType = autoclass('org.semanticweb.owlapi.model.AxiomType')
        self.OWLOntology = autoclass('org.semanticweb.owlapi.model.OWLOntology')
        self.PrefixManager = autoclass('org.semanticweb.owlapi.model.PrefixManager')
        self.OWLManager = autoclass('org.semanticweb.owlapi.apibinding.OWLManager')
        self.OWLOntologyID = autoclass('org.semanticweb.owlapi.model.OWLOntologyID')

        self.ClassExpressionType = autoclass('org.semanticweb.owlapi.model.ClassExpressionType')
        self.DataRangeType = autoclass('org.semanticweb.owlapi.model.DataRangeType')

        self.OWLObjectUnionOf = autoclass('org.semanticweb.owlapi.model.OWLObjectUnionOf')
        self.OWLObjectIntersectionOf = autoclass('org.semanticweb.owlapi.model.OWLObjectIntersectionOf')
        self.OWLObjectComplementOf = autoclass('org.semanticweb.owlapi.model.OWLObjectComplementOf')
        self.OWLObjectOneOf = autoclass('org.semanticweb.owlapi.model.OWLObjectOneOf')
        self.OWLObjectHasSelf = autoclass('org.semanticweb.owlapi.model.OWLObjectHasSelf')

        self.OWLObjectSomeValuesFrom = autoclass('org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom')
        self.OWLObjectAllValuesFrom = autoclass('org.semanticweb.owlapi.model.OWLObjectAllValuesFrom')
        self.OWLObjectHasValue = autoclass('org.semanticweb.owlapi.model.OWLObjectHasValue')
        self.OWLObjectMinCardinality = autoclass('org.semanticweb.owlapi.model.OWLObjectMinCardinality')
        self.OWLObjectMaxCardinality = autoclass('org.semanticweb.owlapi.model.OWLObjectMaxCardinality')
        self.OWLObjectExactCardinality = autoclass('org.semanticweb.owlapi.model.OWLObjectExactCardinality')

        self.OWLDataSomeValuesFrom = autoclass('org.semanticweb.owlapi.model.OWLDataSomeValuesFrom')
        self.OWLDataAllValuesFrom = autoclass('org.semanticweb.owlapi.model.OWLDataAllValuesFrom')
        self.OWLDataHasValue = autoclass('org.semanticweb.owlapi.model.OWLDataHasValue')
        self.OWLDataMinCardinality = autoclass('org.semanticweb.owlapi.model.OWLDataMinCardinality')
        self.OWLDataMaxCardinality = autoclass('org.semanticweb.owlapi.model.OWLDataMaxCardinality')
        self.OWLDataExactCardinality = autoclass('org.semanticweb.owlapi.model.OWLDataExactCardinality')

        self.FunctionalSyntaxDocumentFormat = autoclass('org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat')
        self.ManchesterSyntaxDocumentFormat = autoclass('org.semanticweb.owlapi.formats.ManchesterSyntaxDocumentFormat')

        self.OWLAnnotationValue = autoclass('org.semanticweb.owlapi.model.OWLAnnotationValue')

        self.OWLOntologyDocumentTarget = autoclass('org.semanticweb.owlapi.io.OWLOntologyDocumentTarget')
        self.StringDocumentTarget = autoclass('org.semanticweb.owlapi.io.StringDocumentTarget')
        self.TurtleDocumentFormat = autoclass('org.semanticweb.owlapi.formats.TurtleDocumentFormat')
        self.RDFXMLDocumentFormat = autoclass('org.semanticweb.owlapi.formats.RDFXMLDocumentFormat')

        self.File = autoclass('java.io.File')
        self.Set = autoclass('java.util.Set')
        self.LinkedList = autoclass('java.util.LinkedList')
        self.List = autoclass('java.util.List')
        self.HashSet = autoclass('java.util.HashSet')
        self.Object = autoclass('java.lang.Object')
        self.HashSet = autoclass('java.util.HashSet')

        self.syntax = None

        self.df = None
        self.man = None
        self.ontology = None
        self.pm = None
        self.num = 0
        self.max = 100

        self.axiom_graph_dict = dict()
        self.axiom_metadata_dict = dict()
        self.diag_entity_axioms_dict = dict()

        self.occupied_positions_xy = dict()
        self.occupied_rectangles = []

        self.diagram_to_place = list(self.project.diagrams())[0]
        # self.diagram_to_place = kwargs.get('diagram_to_place',list(self.project.diagrams())[0])
        print('self.diagram_to_place', self.diagram_to_place)

        # self.axioms_types_to_import = kwargs.get('axioms_types_to_import',[])
        # self.import_do = kwargs.get('import_do',False)

        self.exclusion_list = ['RoleNode:', 'AttributeNode:', 'DomainRestrictionNode:', 'RangeRestrictionNode:',
                               'ValueDomainNode:', 'IndividualNode:', 'EnumerationNode:', 'RoleInverseNode:', 'RoleChainNode:',
                               'PropertyAssertionNode:']

        self.disjoint_class_expressions_for_all_DisjointClassesAxioms = []
        self.disjoint_class_expressionsNos_for_all_DisjointClassesAxioms_to_be_ignored = []

        self.ClassExpressions_unique = []
        self.ClassExpressions_to_nodes_mapping = []

        self.AttributesANDRoles_to_DomainANDRange_mapping = dict()
        self.attributes_for_classexpressions = dict()
        self.Entities_with_individuals = dict()

    def step(self, num, increase=0):
        """
        Increments the progress by the given step and emits the progress signal.
        :type num: int
        :type increase: int
        """
        self.max += increase
        self.num += num
        self.num = clamp(self.num, minval=0, maxval=self.max)
        self.sgnProgress.emit(self.num, self.max)

    def process_Datatype_Definitions(self):
        ###
        #   DatatypeDefinition(DN D)
        pass

    def process_Key(self, axiom):
        ###
        #  HasKey(C (P1 … Pm) (R1 … Rn) )
        pass

    def Process_Declaration(self, axiom, commands):
        ###
        #   Declarations
        #
        #   create a new entity; place it in the diagram.

        print('axiom.toString()', axiom.toString())

        entity = axiom.getEntity()
        iri_to_append = entity.getIRI()

        print('entity.toString()', entity.toString())
        print('iri_to_append', iri_to_append)
        print('axiom.getEntity().getEntityType()', entity.getEntityType().toString())

        node = None

        # if iri_to_append in commands:
        ind = self.check_if_iri_is_in_commands(iri_to_append, commands)
        if ind != -1:
            # ind = commands.index(iri_to_append)
            node = commands[ind - 1]
        else:
            #   Declaration( Class( CN ) )
            if entity.isOWLClass():
                node = self.diagram_to_place.factory.create(Item.ConceptNode)
            # Declaration( ObjectProperty( PN ) )
            elif entity.isOWLObjectProperty():
                node = self.diagram_to_place.factory.create(Item.RoleNode)
            # Declaration( DataProperty( R ) )
            elif entity.isOWLDataProperty():
                node = self.diagram_to_place.factory.create(Item.AttributeNode)
            # Declaration( NamedIndividual( aN ) )
            elif entity.isOWLNamedIndividual():
                node = self.diagram_to_place.factory.create(Item.IndividualNode)
            # Declaration( Datatype( DN ) )
            # elif entity.isOWLDatatype():
            #    pass
            #   Declaration( AnnotationProperty( A ) )
            # elif entity.isOWLAnnotationProperty():
            #    pass
            # elif entity.isTopEntity():
            #    pass
            # elif entity.isBottomEntity():
            #    pass
            else:
                pass

        if node is None:
            return None
        else:
            return [node, iri_to_append]

    def Process_annotated_and_import_axiom(self, axiom):

        def process_Annotation(self, axiom):
            ###
            #   Annotations
            #
            #   AnnotationAssertion(A s t)
            #   AXIOM(Annotation(A t) …)
            #   AXIOM(Annotation(A t) … )
            #   Annotation(Annotation(A t) … A1 t1)
            pass

        def process_Annotation_Axiom(self, axiom):
            ###
            # Annotation Axioms
            #
            #   SubAnnotationPropertyOf(A1 A2)
            #   AnnotationPropertyDomain(A U)
            #   AnnotationPropertyRange(A U)
            pass

        def process_Ontology(self, axiom):
            ###
            #   Ontologies
            #
            #   Ontology([ON [U]] Import(ON1)... Annotation(A t) ...)
            #   Prefix(p=U)
            pass

        process_Annotation(axiom)
        process_Annotation_Axiom(axiom)

        process_Ontology(axiom)

    def Process_annotation_axiom(self):

        self.process_Annotation()
        self.process_Annotation_Axiom()

        self.process_Ontology()

    def Process_bottom_entity(self):
        pass

    def Process_top_entity(self):
        pass

    def Process_logical_axiom(self, axiom_to_process):

        print('Process_logical_axiom >>>')

        # print('axiom.getAxiomType().getName()',axiom_to_process.getAxiomType().getName())
        # print('axiom.getAxiomType().getIndex()', axiom_to_process.getAxiomType().getIndex())
        # print('axiom.getAxiomType().toString()', axiom_to_process.getAxiomType().toString())

        # print('axiom.toString()', axiom_to_process.toString())

        ## empty as of now ##
        def process_Class_Expression_Axiom(self, axiom):
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

        def process_Object_Property_Axiom(self, axiom):
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

        def process_Data_Property_Axiom(self, axiom):
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

        ## ##

        def process_Assertion(self, axiom):

            diagram = list(self.project.diagrams())[0]

            print('process_Assertion_axiom >>>')

            ###
            #   Assertions
            #
            #   SameIndividual(a1 … an)
            #   DifferentIndividuals(a1 a2)
            #   DifferentIndividuals(a1 … an)
            #   ClassAssertion(C a)
            print('axiom.getAxiomType().toString()', axiom.getAxiomType().toString())
            if axiom.getAxiomType().toString() == self.AxiomType.CLASS_ASSERTION.toString():

                class_expression = axiom.getClassExpression()
                cast(self.OWLClassExpression, class_expression)
                print('class_expression.toString()', class_expression.toString())
                nodeA = diagram.factory.create(Item.ConceptNode)

                full_iriA = class_expression.toString()[1:len(class_expression.toString()) - 1]
                resA = self.project.get_iri_and_rc_from_full_iri(full_iriA)
                iriA = resA[0]
                rcA = resA[1]

                nodeA.setText(full_iriA)
                nodeA.remaining_characters = rcA

                individual = axiom.getIndividual()
                cast(self.OWLIndividual, individual)
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
                    return ['ClassAssertion', [nodeA, nodeB], [iriA, iriB], [edge]]
            else:
                return None
            # ObjectPropertyAssertion( PN a1 a2 )
            #   DataPropertyAssertion( R a v )
            #   NegativeObjectPropertyAssertion(P a1 a2 )
            #   NegativeDataPropertyAssertion(R a v )
            print('process_Assertion_axiom >>>')

        process_Class_Expression_Axiom(axiom_to_process)
        process_Object_Property_Axiom(axiom_to_process)
        process_Data_Property_Axiom(axiom_to_process)

        # self.process_Datatype_Definitions()

        op = process_Assertion(axiom_to_process)

        # self.process_Keys()

        print('Process_logical_axiom >>> END')

        return op

    def Process_other_type_of_axiom(self):

        self.process_Datatype_Definition()

        self.process_Key()

        self.process_Annotation()
        self.process_Annotation_Axiom()

        self.process_Ontology()

    def check_if_iri_is_in_commands(self, iri_inp, commands):

        ind = -1

        for i, c in enumerate(commands):
            if str(type(c)) == '<class \'jnius.reflect.org.semanticweb.owlapi.model.IRI\'>':
                if iri_inp.toString() == c.toString():
                    return i

        return ind

    def get_node_from_iri(self, iri_inp, dict):

        for str_node in dict.keys():
            iri = dict[str_node][1]
            if iri_inp.toString() == iri.toString():
                return dict[str_node][0]

        return None

    def Process_ClassDescription(self, axiom):

        pass

    def create_and_return_dummy_node(self, text, iri_node_str_dict):

        return_result = []

        dummy_node = self.diagram_to_place.factory.create(Item.ConceptNode)
        # dummy_node.setText('dummy:d')
        # dummy_node.remaining_characters = 'd'
        return_result.append(dummy_node)
        # iri = self.IRI.create(text)
        iri = self.IRI.create('http://d#Node of unknown type')
        return_result.append(iri)
        return_result.append(0)

        iri_node_str_dict[str(dummy_node)] = [dummy_node, iri]

        return return_result

    def check_if_all_class_expressions_are_disjoint(self, all_operands):

        flag = False

        for ax_no, disj_ces in enumerate(self.disjoint_class_expressions_for_all_DisjointClassesAxioms):

            present = set()
            # check if all operators are present.
            for opr in all_operands:

                opr_present = set()

                for dis_ce in disj_ces:
                    # print('opr.equals(dis_ce)',opr.equals(dis_ce))
                    if dis_ce.compareTo(opr) == 0:
                        opr_present.add(True)
                        break
                    else:
                        opr_present.add(False)

                if True not in opr_present:
                    present.add(False)
                    break
                else:
                    present.add(True)

            if False not in present:
                # all operators are present
                # no need to generate a C.E with not node; add it to ignore list
                self.disjoint_class_expressionsNos_for_all_DisjointClassesAxioms_to_be_ignored.append(ax_no)
                flag = True
            else:
                pass

        return flag

    def Process_FacetRestriction(self, facet_restriction_raw, nodes_dict, iri_node_str_dict, depth):

        # print('depth', depth)

        facet_restriction = cast(self.OWLFacet, facet_restriction_raw)

        facet = facet_restriction.getFacet()
        literal = facet_restriction.getFacetValue().getLiteral()

        node = self.diagram_to_place.factory.create(Item.FacetNode)
        nodes_dict[str(node)] = []

        iri = self.IRI.create('')
        iri_node_str_dict[str(node)] = [node, iri]

        # set facet and value for the node
        label = node.compose(facet, literal)

        node.setText(label)

        return node

    def Process_Literal(self, literal_raw, nodes_dict, iri_node_str_dict, depth):

        literal = cast(self.OWLLiteral, literal_raw)

        # print('depth', depth)

        # datatype = literal_raw.getDatatype()
        # lang = literal_raw.getLang()
        # literal = literal_raw.getLiteral()

        # iri = datatype.getIRI()

        node = self.diagram_to_place.factory.create(Item.IndividualNode)
        node.setText(literal_raw.toString())
        nodes_dict[str(node)] = []

        iri = self.IRI.create('')
        iri_node_str_dict[str(node)] = [node, iri]
        # create indivudual node and set text

        return node

    def Process_DataRange(self, filler_range_raw, nodes_dict, edges_dict, iri_node_str_dict, depth):

        filler_range = cast(self.OWLDataRange, filler_range_raw)

        range_type = filler_range.getDataRangeType()

        # print('depth',depth)

        # print('range_type.toString()',range_type.toString())
        # print('self.DataRangeType.DATATYPE.toString()', self.DataRangeType.DATATYPE.toString())

        if (range_type.toString() == self.DataRangeType.DATATYPE.toString()):

            # print('range_type.toString() == range_type.DATATYPE.toString()')

            n_dre_cast = filler_range.asOWLDatatype()
            iri_to_append = n_dre_cast.getIRI()
            # print('iri_to_append',iri_to_append)

            # node = self.get_node_from_iri(iri_to_append, iri_node_str_dict)
            # if node is None:
            node = self.diagram_to_place.factory.create(Item.ValueDomainNode)

            # set the value domain
            iri_and_rc = self.project.get_iri_and_rc_from_full_iri(iri_to_append.toString())
            prefix = self.project.get_prefix_for_iri(iri_and_rc[0])

            # print('prefix', prefix)
            # print('iri_and_rc[1]', iri_and_rc[1])

            node.setText(prefix + ':' + iri_and_rc[1])
            # node.setText(iri_to_append)

            nodes_dict[str(node)] = []
            iri_node_str_dict[str(node)] = [node, iri_to_append]

            return node

        elif (range_type.toString() == self.DataRangeType.DATA_COMPLEMENT_OF.toString()):

            n_dre_cast = cast(self.OWLDataComplementOf, filler_range)

            data_range = n_dre_cast.getDataRange()

            processed_data_range = self.Process_DataRange(data_range, nodes_dict, edges_dict, iri_node_str_dict,
                                                          depth + 1)

            NOTnode = self.diagram_to_place.factory.create(Item.ComplementNode)
            iri = self.IRI.create('')
            iri_node_str_dict[str(NOTnode)] = [NOTnode, iri]
            nodes_dict[str(NOTnode)] = []

            nodes_dict[str(NOTnode)].append(processed_data_range)
            edges_dict[Item.InputEdge.value].append([processed_data_range, NOTnode])

            return NOTnode

        elif (range_type.toString() == self.DataRangeType.DATA_INTERSECTION_OF.toString()):

            n_dre_cast = cast(self.OWLDataIntersectionOf, filler_range)

            # create a new AND node
            ANDnode = self.diagram_to_place.factory.create(Item.IntersectionNode)
            iri = self.IRI.create('')
            iri_node_str_dict[str(ANDnode)] = [ANDnode, iri]
            nodes_dict[str(ANDnode)] = []

            operands = n_dre_cast.getOperands()
            itr = operands.iterator()
            while (itr.hasNext()):
                operand = itr.next()
                processed_operand = self.Process_DataRange(operand, nodes_dict, edges_dict, iri_node_str_dict,
                                                           depth + 1)
                nodes_dict[str(ANDnode)].append(processed_operand)
                edges_dict[Item.InputEdge.value].append([processed_operand, ANDnode])

            return ANDnode

        elif (range_type.toString() == self.DataRangeType.DATA_UNION_OF.toString()):

            n_dre_cast = cast(self.OWLDataUnionOf, filler_range)

            # create a new OR node
            ORnode = self.diagram_to_place.factory.create(Item.UnionNode)
            iri = self.IRI.create('')
            iri_node_str_dict[str(ORnode)] = [ORnode, iri]
            nodes_dict[str(ORnode)] = []

            operands = n_dre_cast.getOperands()
            itr = operands.iterator()
            while (itr.hasNext()):
                operand = itr.next()
                processed_operand = self.Process_DataRange(operand, nodes_dict, edges_dict, iri_node_str_dict,
                                                           depth + 1)
                nodes_dict[str(ORnode)].append(processed_operand)
                edges_dict[Item.InputEdge.value].append([processed_operand, ORnode])

            return ORnode

        elif (range_type.toString() == self.DataRangeType.DATA_ONE_OF.toString()):

            oneofNode = self.diagram_to_place.factory.create(Item.EnumerationNode)
            iri = self.IRI.create('')
            iri_node_str_dict[str(oneofNode)] = [oneofNode, iri]
            nodes_dict[str(oneofNode)] = []

            n_dre_cast = cast(self.OWLDataOneOf, filler_range)

            values = n_dre_cast.getValues()
            itr = values.iterator()
            while (itr.hasNext()):
                literal = itr.next()
                processed_literal = self.Process_Literal(literal, nodes_dict, iri_node_str_dict, depth + 1)
                nodes_dict[str(oneofNode)].append(processed_literal)
                edges_dict[Item.InputEdge.value].append([processed_literal, oneofNode])

            return oneofNode

        elif (range_type.toString() == self.DataRangeType.DATATYPE_RESTRICTION.toString()):

            n_dre_cast = cast(self.OWLDatatypeRestriction, filler_range)

            DATAnode = self.diagram_to_place.factory.create(Item.DatatypeRestrictionNode)
            nodes_dict[str(DATAnode)] = []
            iri_to_append = self.IRI.create('')
            iri_node_str_dict[str(DATAnode)] = [DATAnode, iri_to_append]

            data_type = n_dre_cast.getDatatype()
            processed_data_type = self.Process_DataRange(data_type, nodes_dict, edges_dict, iri_node_str_dict,
                                                         depth + 1)
            nodes_dict[str(DATAnode)].append(processed_data_type)
            edges_dict[Item.InputEdge.value].append([processed_data_type, DATAnode])

            facet_restrictions = n_dre_cast.getFacetRestrictions()
            itr = facet_restrictions.iterator()
            while (itr.hasNext()):
                facet_restriction = itr.next()
                processed_facet_restriction = self.Process_FacetRestriction(facet_restriction, nodes_dict,
                                                                            iri_node_str_dict, depth + 1)
                nodes_dict[str(DATAnode)].append(processed_facet_restriction)
                edges_dict[Item.InputEdge.value].append([processed_facet_restriction, DATAnode])

            return DATAnode

        else:
            return None

    def Process_ObjectPropertyExpression(self, ope, iri_node_str_dict):

        new_casted_ope = cast(self.OWLObjectPropertyExpression, ope)

        flag_inverse = False

        # print('new_casted_ope.toString()[0:9]',new_casted_ope.toString()[0:9])
        if new_casted_ope.toString()[0:9] == 'InverseOf':
            flag_inverse = True
            object_property = new_casted_ope.getInverseProperty().asOWLObjectProperty()
        else:
            object_property = new_casted_ope.asOWLObjectProperty()

        iri_to_append = object_property.getIRI()
        node = self.get_node_from_iri(iri_to_append, iri_node_str_dict)
        if node is None:
            node = self.diagram_to_place.factory.create(Item.RoleNode)
            iri_node_str_dict[str(node)] = [node, iri_to_append]

        return [node, iri_to_append, 0, flag_inverse]

    def Process_ObjectPropertyExpression_2(self, ope, nodes_dict, iri_node_str_dict, depth):

        # print('depth', depth)

        new_casted_ope = cast(self.OWLObjectPropertyExpression, ope)

        flag_inverse = False
        if new_casted_ope.toString()[0:9] == 'InverseOf':
            flag_inverse = True
            object_property = new_casted_ope.getInverseProperty().asOWLObjectProperty()
        else:
            object_property = new_casted_ope.asOWLObjectProperty()

        iri_to_append = object_property.getIRI()
        node = self.get_node_from_iri(iri_to_append, iri_node_str_dict)
        if node is None:
            node = self.diagram_to_place.factory.create(Item.RoleNode)
            nodes_dict[str(node)] = []
            iri_node_str_dict[str(node)] = [node, iri_to_append]
            self.AttributesANDRoles_to_DomainANDRange_mapping[str(node)] = []

        return [node, flag_inverse]

    def Process_DataPropertyExpression_2(self, dpe, nodes_dict, iri_node_str_dict, depth):

        # print('depth', depth)

        new_casted_dpe = cast(self.OWLDataPropertyExpression, dpe)

        data_property = new_casted_dpe.asOWLDataProperty()

        iri_to_append = data_property.getIRI()
        node = self.get_node_from_iri(iri_to_append, iri_node_str_dict)
        if node is None:
            node = self.diagram_to_place.factory.create(Item.AttributeNode)
            nodes_dict[str(node)] = []
            iri_node_str_dict[str(node)] = [node, iri_to_append]
            self.AttributesANDRoles_to_DomainANDRange_mapping[str(node)] = []

        return node

    def Process_Individual(self, ind_raw, nodes_dict, iri_node_str_dict, depth):

        ind = cast(self.OWLIndividual, ind_raw)

        if ind.isAnonymous():
            return self.diagram_to_place.factory.create(Item.IndividualNode)
        else:
            iri_to_append = ind.asOWLNamedIndividual().getIRI()

            node = self.get_node_from_iri(iri_to_append, iri_node_str_dict)

            if node is None:
                node = self.diagram_to_place.factory.create(Item.IndividualNode)
                nodes_dict[str(node)] = []
                iri_node_str_dict[str(node)] = [node, iri_to_append]
            return node

    # not used
    def Process_ClassExpression(self, class_expression, commands, iri_node_str_dict):

        cl_str = class_expression.toString()

        if class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OWL_CLASS.toString():

            iri_to_append = class_expression.asOWLClass().getIRI()
            node = self.get_node_from_iri(iri_to_append, iri_node_str_dict)
            if node is None:
                node = self.diagram_to_place.factory.create(Item.ConceptNode)
                iri_node_str_dict[str(node)] = [node, iri_to_append]
            return [node, iri_to_append, 0]

        else:
            if class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_ALL_VALUES_FROM.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataAllValuesFrom, class_expression)

                filler_range = new_casted_class_expression.getFiller()
                data_property_expression = new_casted_class_expression.getProperty()

                print('filler_range.toString()', filler_range.toString())
                print('data_property_expression.toString()', data_property_expression.toString())
                # property-> attribute

                processed_dpe = self.Process_DataPropertyExpression(data_property_expression)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_SOME_VALUES_FROM.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataSomeValuesFrom, class_expression)

                filler_range = new_casted_class_expression.getFiller()
                data_property_expression = new_casted_class_expression.getProperty()

                print('filler_range.toString()', filler_range.toString())
                print('data_property_expression.toString()', data_property_expression.toString())

                processed_dpe = self.Process_DataPropertyExpression(data_property_expression)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_HAS_VALUE.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataHasValue, class_expression)

                filler = new_casted_class_expression.getFiller()
                data_property_expression = new_casted_class_expression.getProperty()

                processed_dpe = self.Process_DataPropertyExpression(data_property_expression)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_MAX_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataMaxCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler_range = new_casted_class_expression.getFiller()
                data_property_expression = new_casted_class_expression.getProperty()

                print("cardinality:- " + cardinality)
                print("filler_range:- " + filler_range.toString())
                print("data_property_expression:- " + data_property_expression.toString())

                processed_dpe = self.Process_DataPropertyExpression(data_property_expression)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_MIN_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataMinCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler = new_casted_class_expression.getFiller()
                data_property_expression = new_casted_class_expression.getProperty()

                print("cardinality:- " + cardinality)
                print("filler_exact:- " + filler.toString())
                print("data_property_expression:- " + data_property_expression.toString())

                processed_dpe = self.Process_DataPropertyExpression(data_property_expression)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_EXACT_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLDataExactCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler = new_casted_class_expression.getFiller()
                data_property_expression = new_casted_class_expression.getProperty()

                print("cardinality:- " + cardinality)
                print("filler:- " + filler.toString())
                print("data_property_expression:- " + data_property_expression.toString())

                processed_dpe = self.Process_DataPropertyExpression(data_property_expression)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_ALL_VALUES_FROM.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectAllValuesFrom, class_expression)

                filler_exp = new_casted_class_expression.getFiller()
                new_castred_filler = cast(self.OWLClassExpression, filler_exp)
                object_property_expression = new_casted_class_expression.getProperty()

                print('new_castred_filler.toString()', new_castred_filler.toString())
                print('new_castred_filler', new_castred_filler)
                print('object_property_expression.toString()', object_property_expression.toString())
                print('object_property_expression', object_property_expression)

                processed_ope = self.Process_ObjectPropertyExpression(object_property_expression, iri_node_str_dict)

                # if flag_inverse is True, append black square else append white square
                flag_inverse = processed_ope[3]
                if not flag_inverse:
                    node_forall = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    node_forall = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                node_forall.setText('forall')

                temp = []

                temp.append(node_forall)
                iri = self.IRI.create('')
                temp.append(iri)
                if new_castred_filler.toString() == 'owl:Thing':
                    temp.append(1)
                else:
                    temp.append(2)

                temp.append(processed_ope[0])
                temp.append(processed_ope[1])
                temp.append(processed_ope[2])

                if new_castred_filler.toString() == 'owl:Thing':
                    pass
                else:
                    processed_filler = self.Process_ClassExpression(new_castred_filler, commands, iri_node_str_dict)
                    temp.extend(processed_filler)

                iri_node_str_dict[str(node_forall)] = [node_forall, iri]

                for t in temp:
                    print('t-', t)

                return temp

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_SOME_VALUES_FROM.toString():
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectSomeValuesFrom, class_expression)

                filler_exp = new_casted_class_expression.getFiller()
                object_property_expression = new_casted_class_expression.getProperty()

                print('filler_ind.toString()', filler_exp.toString())
                print('object_property_expression.toString()', object_property_expression.toString())

                processed_ope = self.Process_ObjectPropertyExpression(object_property_expression, iri_node_str_dict)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_HAS_VALUE.toString():
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectHasValue, class_expression)

                filler_ind = new_casted_class_expression.getFiller()
                object_property_expression = new_casted_class_expression.getProperty()

                print('filler_ind.toString()', filler_ind.toString())
                print('object_property_expression.toString()', object_property_expression.toString())

                processed_ope = self.Process_ObjectPropertyExpression(object_property_expression, iri_node_str_dict)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_MAX_CARDINALITY.toString():
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectMaxCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler = new_casted_class_expression.getFiller()
                object_property_expression = new_casted_class_expression.getProperty()

                print("cardinality:- " + cardinality)
                print("filler:- " + filler.toString())
                print("object_property_expression:- " + object_property_expression.toString())

                processed_ope = self.Process_ObjectPropertyExpression(object_property_expression, iri_node_str_dict)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_MIN_CARDINALITY.toString():
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectMinCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler = new_casted_class_expression.getFiller()
                object_property_expression = new_casted_class_expression.getProperty()

                print("cardinality:- " + cardinality)
                print("filler:- " + filler.toString())
                print("object_property_expression:- " + object_property_expression.toString())

                processed_ope = self.Process_ObjectPropertyExpression(object_property_expression, iri_node_str_dict,
                                                                      iri_node_str_dict)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_EXACT_CARDINALITY.toString():
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectExactCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler = new_casted_class_expression.getFiller()
                object_property_expression = new_casted_class_expression.getProperty()

                print("cardinality:- " + cardinality)
                print("filler:- " + filler.toString())
                print("object_property_expression:- " + object_property_expression.toString())

                processed_ope = self.Process_ObjectPropertyExpression(object_property_expression, iri_node_str_dict)

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_UNION_OF.toString():

                # new_class_expression =self.OWLObjectUnionOf.getClass().cast(class_expression)
                new_casted_class_expression = cast(self.OWLObjectUnionOf, class_expression)

                # create a new OR node
                ORnode = self.diagram_to_place.factory.create(Item.UnionNode)
                # get all operands
                operands = new_casted_class_expression.getOperands()
                itr = operands.iterator()

                # for all operands
                # append[ORnode,None,operand,operand_iri]Process_ClassExpression

                temp = []

                temp.append(ORnode)
                iri = self.IRI.create('')
                temp.append(iri)
                temp.append(operands.size())

                while (itr.hasNext()):
                    operand = itr.next()
                    processed_operand = self.Process_ClassExpression(operand, commands, iri_node_str_dict)
                    temp.extend(processed_operand)

                iri_node_str_dict[str(ORnode)] = [ORnode, iri]

                return temp

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_ONE_OF.toString():
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_COMPLEMENT_OF.toString():

                new_casted_class_expression = cast(self.OWLObjectComplementOf, class_expression)

                # create a new NOT node
                NOTnode = self.diagram_to_place.factory.create(Item.ComplementNode)
                # get all operands
                operand = new_casted_class_expression.getOperand()

                # for all operands
                # append[NOTnode,None,operand,operand_iri]Process_ClassExpression

                temp = []

                processed_operand = self.Process_ClassExpression(operand, commands, iri_node_str_dict)
                temp.append(NOTnode)
                iri = self.IRI.create('')
                temp.append(iri)
                temp.append(1)
                temp.extend(processed_operand)

                iri_node_str_dict[str(NOTnode)] = [NOTnode, iri]
                return temp

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_INTERSECTION_OF.toString():

                new_casted_class_expression = cast(self.OWLObjectIntersectionOf, class_expression)

                # create a new AND node
                ANDnode = self.diagram_to_place.factory.create(Item.IntersectionNode)
                # get all operands
                operands = new_casted_class_expression.getOperands()
                itr = operands.iterator()

                # for all operands
                # append[ANDnode,None,operand,operand_iri]Process_ClassExpression

                temp = []

                temp.append(ANDnode)
                iri = self.IRI.create('')
                temp.append(iri)
                temp.append(operands.size())

                while (itr.hasNext()):
                    operand = itr.next()
                    processed_operand = self.Process_ClassExpression(operand, commands, iri_node_str_dict)

                    temp.extend(processed_operand)

                iri_node_str_dict[str(ANDnode)] = [ANDnode, iri]

                return temp


            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_HAS_SELF.toString():
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectHasSelf, class_expression)

                object_property_expression = new_casted_class_expression.getProperty()

                print("object_property_expression:- " + object_property_expression.toString())

                processed_ope = self.Process_ObjectPropertyExpression(object_property_expression, iri_node_str_dict)

            else:
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

    def Process_ClassExpression_2(self, class_expression_raw, nodes_dict, edges_dict, iri_node_str_dict, depth):

        class_expression = cast(self.OWLClassExpression, class_expression_raw)

        cl_str = class_expression.toString()

        # print('class_expression.toString()',class_expression.toString())
        # print('depth - ', depth)

        for ind_drce, classexpression_unique in enumerate(self.ClassExpressions_unique):
            casted_class_expression = cast(self.Object, class_expression)
            casted_classexpression_unique = cast(self.Object, classexpression_unique)

            int_java = class_expression.compareTo(classexpression_unique)

            # print('int_java',int_java)

            # res = casted_class_expression.equals(casted_classexpression_unique)
            # res_2 = class_expression.equals(classexpression_unique)

            # print('res',res)
            # print('res', res_2)

            if int_java == 0:
                # print('classexpression_unique', classexpression_unique.toString())
                # print('class_expression', class_expression.toString())
                node_to_return = self.ClassExpressions_to_nodes_mapping[ind_drce]
                return node_to_return

        if class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OWL_CLASS.toString():

            new_casted_class_expression = cast(self.OWLClass, class_expression)

            iri_to_append = new_casted_class_expression.getIRI()

            # node = self.get_node_from_iri(iri_to_append, iri_node_str_dict)
            # if node is None:
            node = self.diagram_to_place.factory.create(Item.ConceptNode)
            nodes_dict[str(node)] = []
            iri_node_str_dict[str(node)] = [node, iri_to_append]

            self.ClassExpressions_unique.append(new_casted_class_expression)
            self.ClassExpressions_to_nodes_mapping.append(node)

            return node

        else:
            if class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_ALL_VALUES_FROM.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataAllValuesFrom, class_expression)

                filler_range = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLDataRange, filler_range)

                data_property_expression = new_casted_class_expression.getProperty()
                processed_dpe = self.Process_DataPropertyExpression_2(data_property_expression, nodes_dict,
                                                                      iri_node_str_dict, depth + 1)

                node_forall = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                node_forall.setText('forall')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_forall)] = [node_forall, iri]
                nodes_dict[str(node_forall)] = []
                nodes_dict[str(node_forall)].append(processed_dpe)
                edges_dict[Item.InputEdge.value].append([processed_dpe, node_forall])

                if new_casted_filler.toString() == 'rdfs:Literal':
                    pass
                else:
                    processed_filler = self.Process_DataRange(new_casted_filler, nodes_dict, edges_dict,
                                                              iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_forall)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_forall])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_forall)

                return node_forall

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_SOME_VALUES_FROM.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataSomeValuesFrom, class_expression)

                filler_range = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLDataRange, filler_range)

                data_property_expression = new_casted_class_expression.getProperty()
                processed_dpe = self.Process_DataPropertyExpression_2(data_property_expression, nodes_dict,
                                                                      iri_node_str_dict, depth + 1)

                node_exists = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                node_exists.setText('exists')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_exists)] = [node_exists, iri]
                nodes_dict[str(node_exists)] = [processed_dpe]
                edges_dict[Item.InputEdge.value].append([processed_dpe, node_exists])

                # print('new_casted_filler.toString()',new_casted_filler.toString())

                if new_casted_filler.toString() == 'rdfs:Literal':
                    pass
                else:
                    processed_filler = self.Process_DataRange(new_casted_filler, nodes_dict, edges_dict,
                                                              iri_node_str_dict, depth + 1)

                    nodes_dict[str(node_exists)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_exists])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_exists)

                return node_exists

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_HAS_VALUE.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataHasValue, class_expression)

                filler_individual = new_casted_class_expression.getFiller()

                if filler_individual.isAnonymous():
                    pass
                else:
                    iri_filler_individual = filler_individual.asOWLNamedIndividual().getIRI()
                    individualNode = self.diagram_to_place.factory.create(Item.EnumerationNode)
                    iri_node_str_dict[str(individualNode)] = [individualNode, iri_filler_individual]

                oneofNode = self.diagram_to_place.factory.create(Item.EnumerationNode)
                iri_oneof = self.IRI.create('')
                iri_node_str_dict[str(oneofNode)] = [oneofNode, iri_oneof]
                nodes_dict[str(oneofNode)] = []
                nodes_dict[str(oneofNode)].append(individualNode)
                edges_dict[Item.InputEdge.value].append([individualNode, oneofNode])

                data_property_expression = new_casted_class_expression.getProperty()
                processed_dpe = self.Process_DataPropertyExpression_2(data_property_expression, nodes_dict,
                                                                      iri_node_str_dict, depth + 1)

                node_exists = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                node_exists.setText('exists')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_exists)] = [node_exists, iri]
                nodes_dict[str(node_exists)] = []
                nodes_dict[str(node_exists)].append(processed_dpe)
                edges_dict[Item.InputEdge.value].append([processed_dpe, node_exists])
                nodes_dict[str(node_exists)].append(oneofNode)
                edges_dict[Item.InputEdge.value].append([oneofNode, node_exists])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_exists)

                return node_exists

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_MAX_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataMaxCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()

                filler_range = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLDataRange, filler_range)

                data_property_expression = new_casted_class_expression.getProperty()

                ###

                processed_dpe = self.Process_DataPropertyExpression_2(data_property_expression, nodes_dict,
                                                                      iri_node_str_dict, depth + 1)

                node_cardinality = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                node_cardinality.setText('(-,' + str(cardinality) + ')')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_cardinality)] = [node_cardinality, iri]
                nodes_dict[str(node_cardinality)] = []
                nodes_dict[str(node_cardinality)].append(processed_dpe)
                edges_dict[Item.InputEdge.value].append([processed_dpe, node_cardinality])

                if new_casted_filler.toString() == 'rdfs:Literal':
                    pass
                else:
                    processed_filler = self.Process_DataRange(new_casted_filler, nodes_dict, edges_dict,
                                                              iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_cardinality)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_cardinality])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_cardinality)

                return node_cardinality

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_MIN_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

                new_casted_class_expression = cast(self.OWLDataMinCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()

                filler_range = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLDataRange, filler_range)

                data_property_expression = new_casted_class_expression.getProperty()
                processed_dpe = self.Process_DataPropertyExpression_2(data_property_expression, nodes_dict,
                                                                      iri_node_str_dict, depth + 1)

                node_cardinality = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                node_cardinality.setText('(' + str(cardinality) + ',-)')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_cardinality)] = [node_cardinality, iri]
                nodes_dict[str(node_cardinality)] = []
                nodes_dict[str(node_cardinality)].append(processed_dpe)
                edges_dict[Item.InputEdge.value].append([processed_dpe, node_cardinality])

                if new_casted_filler.toString() == 'rdfs:Literal':
                    pass
                else:
                    processed_filler = self.Process_DataRange(new_casted_filler, nodes_dict, edges_dict,
                                                              iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_cardinality)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_cardinality])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_cardinality)

                return node_cardinality

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.DATA_EXACT_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLDataExactCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()

                filler_range = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLDataRange, filler_range)

                data_property_expression = new_casted_class_expression.getProperty()
                processed_dpe = self.Process_DataPropertyExpression_2(data_property_expression, nodes_dict,
                                                                      iri_node_str_dict, depth + 1)

                node_cardinality = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                node_cardinality.setText('(' + str(cardinality) + ',' + str(cardinality) + ')')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_cardinality)] = [node_cardinality, iri]
                nodes_dict[str(node_cardinality)] = []
                nodes_dict[str(node_cardinality)].append(processed_dpe)
                edges_dict[Item.InputEdge.value].append([processed_dpe, node_cardinality])

                if new_casted_filler.toString() == 'rdfs:Literal':
                    pass
                else:
                    processed_filler = self.Process_DataRange(new_casted_filler, nodes_dict, edges_dict,
                                                              iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_cardinality)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_cardinality])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_cardinality)

                return node_cardinality

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_ALL_VALUES_FROM.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectAllValuesFrom, class_expression)

                filler_exp = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLClassExpression, filler_exp)
                object_property_expression = new_casted_class_expression.getProperty()

                processed_ope = self.Process_ObjectPropertyExpression_2(object_property_expression, nodes_dict,
                                                                        iri_node_str_dict, depth + 1)

                # if flag_inverse is True, append black square else append white square
                flag_inverse = processed_ope[1]
                if not flag_inverse:
                    node_forall = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    node_forall = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                node_forall.setText('forall')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_forall)] = [node_forall, iri]
                nodes_dict[str(node_forall)] = []

                nodes_dict[str(node_forall)].append(processed_ope[0])
                edges_dict[Item.InputEdge.value].append([processed_ope[0], node_forall])

                if new_casted_filler.toString() == 'owl:Thing':
                    pass
                else:
                    processed_filler = self.Process_ClassExpression_2(new_casted_filler, nodes_dict, edges_dict,
                                                                      iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_forall)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_forall])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_forall)

                return node_forall

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_SOME_VALUES_FROM.toString():

                new_casted_class_expression = cast(self.OWLObjectSomeValuesFrom, class_expression)

                filler_exp = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLClassExpression, filler_exp)
                object_property_expression = new_casted_class_expression.getProperty()

                processed_ope = self.Process_ObjectPropertyExpression_2(object_property_expression, nodes_dict,
                                                                        iri_node_str_dict, depth + 1)

                # if flag_inverse is True, append black square else append white square
                flag_inverse = processed_ope[1]
                if not flag_inverse:
                    node_exists = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    node_exists = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                node_exists.setText('exists')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_exists)] = [node_exists, iri]
                nodes_dict[str(node_exists)] = []

                nodes_dict[str(node_exists)].append(processed_ope[0])
                edges_dict[Item.InputEdge.value].append([processed_ope[0], node_exists])

                if new_casted_filler.toString() == 'owl:Thing':
                    pass
                else:
                    processed_filler = self.Process_ClassExpression_2(new_casted_filler, nodes_dict, edges_dict,
                                                                      iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_exists)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_exists])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_exists)

                return node_exists

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_HAS_VALUE.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectHasValue, class_expression)

                filler_individual = new_casted_class_expression.getFiller()

                if filler_individual.isAnonymous():
                    pass
                else:
                    iri_filler_individual = filler_individual.asOWLNamedIndividual().getIRI()
                    individualNode = self.diagram_to_place.factory.create(Item.EnumerationNode)
                    iri_node_str_dict[str(individualNode)] = [individualNode, iri_filler_individual]

                oneofNode = self.diagram_to_place.factory.create(Item.EnumerationNode)
                iri_oneof = self.IRI.create('')
                iri_node_str_dict[str(oneofNode)] = [oneofNode, iri_oneof]
                nodes_dict[str(oneofNode)] = []
                nodes_dict[str(oneofNode)].append(individualNode)
                edges_dict[Item.InputEdge.value].append([individualNode, oneofNode])

                object_property_expression = new_casted_class_expression.getProperty()
                processed_ope = self.Process_ObjectPropertyExpression_2(object_property_expression, nodes_dict,
                                                                        iri_node_str_dict, depth + 1)

                flag_inverse = processed_ope[1]
                if not flag_inverse:
                    node_exists = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    node_exists = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                    node_exists.setText('exists')
                iri = self.IRI.create('')
                iri_node_str_dict[str(node_exists)] = [node_exists, iri]
                nodes_dict[str(node_exists)] = []
                nodes_dict[str(node_exists)].append(processed_ope[0])
                edges_dict[Item.InputEdge.value].append([processed_ope[0], node_exists])
                nodes_dict[str(node_exists)].append(oneofNode)
                edges_dict[Item.InputEdge.value].append([oneofNode, node_exists])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_exists)

                return node_exists

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_HAS_SELF.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectHasSelf, class_expression)

                object_property_expression = new_casted_class_expression.getProperty()
                processed_ope = self.Process_ObjectPropertyExpression_2(object_property_expression, nodes_dict,
                                                                        iri_node_str_dict, depth + 1)

                flag_inverse = processed_ope[1]
                if not flag_inverse:
                    node_self = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    node_self = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                node_self.setText('self')

                iri = self.IRI.create('')
                iri_node_str_dict[str(node_self)] = [node_self, iri]
                nodes_dict[str(node_self)] = []
                nodes_dict[str(node_self)].append(processed_ope[0])
                edges_dict[Item.InputEdge.value].append([processed_ope[0], node_self])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_self)

                return node_self

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_MAX_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectMaxCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLClassExpression, filler)

                object_property_expression = new_casted_class_expression.getProperty()
                processed_ope = self.Process_ObjectPropertyExpression_2(object_property_expression, nodes_dict,
                                                                        iri_node_str_dict, depth + 1)

                flag_inverse = processed_ope[1]
                if not flag_inverse:
                    node_cardinality = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    node_cardinality = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                node_cardinality.setText('(-,' + str(cardinality) + ')')

                iri = self.IRI.create('')
                iri_node_str_dict[str(node_cardinality)] = [node_cardinality, iri]
                nodes_dict[str(node_cardinality)] = []
                nodes_dict[str(node_cardinality)].append(processed_ope[0])
                edges_dict[Item.InputEdge.value].append([processed_ope[0], node_cardinality])

                if new_casted_filler.toString() == 'owl:Thing':
                    pass
                else:
                    processed_filler = self.Process_ClassExpression_2(new_casted_filler, nodes_dict, edges_dict,
                                                                      iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_cardinality)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_cardinality])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_cardinality)

                return node_cardinality

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_MIN_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectMinCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLClassExpression, filler)

                object_property_expression = new_casted_class_expression.getProperty()
                processed_ope = self.Process_ObjectPropertyExpression_2(object_property_expression, nodes_dict,
                                                                        iri_node_str_dict, depth + 1)

                flag_inverse = processed_ope[1]
                if not flag_inverse:
                    node_cardinality = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    node_cardinality = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                node_cardinality.setText('(' + str(cardinality) + ',-)')

                iri = self.IRI.create('')
                iri_node_str_dict[str(node_cardinality)] = [node_cardinality, iri]
                nodes_dict[str(node_cardinality)] = []
                nodes_dict[str(node_cardinality)].append(processed_ope[0])
                edges_dict[Item.InputEdge.value].append([processed_ope[0], node_cardinality])

                if new_casted_filler.toString() == 'owl:Thing':
                    pass
                else:
                    processed_filler = self.Process_ClassExpression_2(new_casted_filler, nodes_dict, edges_dict,
                                                                      iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_cardinality)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_cardinality])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_cardinality)

                return node_cardinality

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_EXACT_CARDINALITY.toString():
                # return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)
                new_casted_class_expression = cast(self.OWLObjectExactCardinality, class_expression)

                cardinality = new_casted_class_expression.getCardinality()
                filler = new_casted_class_expression.getFiller()
                new_casted_filler = cast(self.OWLClassExpression, filler)

                object_property_expression = new_casted_class_expression.getProperty()
                processed_ope = self.Process_ObjectPropertyExpression_2(object_property_expression, nodes_dict,
                                                                        iri_node_str_dict, depth + 1)

                flag_inverse = processed_ope[1]
                if not flag_inverse:
                    node_cardinality = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    node_cardinality = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                node_cardinality.setText('(' + str(cardinality) + ',' + str(cardinality) + ')')

                iri = self.IRI.create('')
                iri_node_str_dict[str(node_cardinality)] = [node_cardinality, iri]
                nodes_dict[str(node_cardinality)] = []
                nodes_dict[str(node_cardinality)].append(processed_ope[0])
                edges_dict[Item.InputEdge.value].append([processed_ope[0], node_cardinality])

                if new_casted_filler.toString() == 'owl:Thing':
                    pass
                else:
                    processed_filler = self.Process_ClassExpression_2(new_casted_filler, nodes_dict, edges_dict,
                                                                      iri_node_str_dict, depth + 1)
                    nodes_dict[str(node_cardinality)].append(processed_filler)
                    edges_dict[Item.InputEdge.value].append([processed_filler, node_cardinality])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(node_cardinality)

                return node_cardinality

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_UNION_OF.toString():

                # new_class_expression =self.OWLObjectUnionOf.getClass().cast(class_expression)
                new_casted_class_expression = cast(self.OWLObjectUnionOf, class_expression)

                # get all operands
                operands = new_casted_class_expression.getOperands()
                itr = operands.iterator()

                all_operands = []
                all_processed_operands = []

                while (itr.hasNext()):
                    operand = itr.next()
                    all_operands.append(operand)
                    processed_operand = self.Process_ClassExpression_2(operand, nodes_dict, edges_dict,
                                                                       iri_node_str_dict, depth + 1)
                    all_processed_operands.append(processed_operand)
                    # nodes_dict[str(ORnode)].append(processed_operand)
                    # edges_dict[Item.InputEdge.value].append([processed_operand, ORnode])

                all_oprs_present = self.check_if_all_class_expressions_are_disjoint(all_operands)

                if all_oprs_present:
                    # create a new black node
                    ORnode = self.diagram_to_place.factory.create(Item.DisjointUnionNode)
                else:
                    # create a new OR node
                    ORnode = self.diagram_to_place.factory.create(Item.UnionNode)
                iri = self.IRI.create('')
                iri_node_str_dict[str(ORnode)] = [ORnode, iri]
                nodes_dict[str(ORnode)] = []

                for processed_operand in all_processed_operands:
                    nodes_dict[str(ORnode)].append(processed_operand)
                    edges_dict[Item.InputEdge.value].append([processed_operand, ORnode])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(ORnode)

                return ORnode

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_ONE_OF.toString():

                new_casted_class_expression = cast(self.OWLObjectOneOf, class_expression)

                oneofNode = self.diagram_to_place.factory.create(Item.EnumerationNode)
                iri = self.IRI.create('')
                iri_node_str_dict[str(oneofNode)] = [oneofNode, iri]
                nodes_dict[str(oneofNode)] = []

                individuals = new_casted_class_expression.getIndividuals();
                itr = individuals.iterator()
                while (itr.hasNext()):
                    individual = itr.next()
                    individualNode = self.Process_Individual(individual, nodes_dict, iri_node_str_dict, depth + 1)

                    nodes_dict[str(oneofNode)].append(individualNode)
                    edges_dict[Item.InputEdge.value].append([individualNode, oneofNode])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(oneofNode)

                return oneofNode

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_COMPLEMENT_OF.toString():

                new_casted_class_expression = cast(self.OWLObjectComplementOf, class_expression)

                # create a new NOT node
                NOTnode = self.diagram_to_place.factory.create(Item.ComplementNode)
                iri = self.IRI.create('')
                iri_node_str_dict[str(NOTnode)] = [NOTnode, iri]
                nodes_dict[str(NOTnode)] = []

                # get all operands
                operand = new_casted_class_expression.getOperand()

                processed_operand = self.Process_ClassExpression_2(operand, nodes_dict, edges_dict, iri_node_str_dict,
                                                                   depth + 1)

                nodes_dict[str(NOTnode)].append(processed_operand)
                edges_dict[Item.InputEdge.value].append([processed_operand, NOTnode])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(NOTnode)

                return NOTnode

            elif class_expression.getClassExpressionType().toString() == self.ClassExpressionType.OBJECT_INTERSECTION_OF.toString():

                new_casted_class_expression = cast(self.OWLObjectIntersectionOf, class_expression)

                # create a new AND node
                ANDnode = self.diagram_to_place.factory.create(Item.IntersectionNode)
                iri = self.IRI.create('')
                iri_node_str_dict[str(ANDnode)] = [ANDnode, iri]
                nodes_dict[str(ANDnode)] = []

                # get all operands
                operands = new_casted_class_expression.getOperands()
                itr = operands.iterator()

                while (itr.hasNext()):
                    operand = itr.next()
                    processed_operand = self.Process_ClassExpression_2(operand, nodes_dict, edges_dict,
                                                                       iri_node_str_dict, depth + 1)
                    nodes_dict[str(ANDnode)].append(processed_operand)
                    edges_dict[Item.InputEdge.value].append([processed_operand, ANDnode])

                self.ClassExpressions_unique.append(class_expression)
                self.ClassExpressions_to_nodes_mapping.append(ANDnode)

                return ANDnode

            else:
                return self.create_and_return_dummy_node(cl_str, iri_node_str_dict)

    def Process_Class_axiom(self, axiom, iri_node_str_dict):

        commands = []

        if axiom.getAxiomType().toString() == self.AxiomType.SUBCLASS_OF.toString():

            superclass = axiom.getSuperClass();
            super_class_commands = self.Process_ClassExpression(superclass, commands, iri_node_str_dict)

            # if super_class_commands[2] == 0:
            #    super_class_commands[2] = 1
            commands.append('SuperClass')
            commands.extend(super_class_commands)

            subclass = axiom.getSubClass();
            sub_class_commands = self.Process_ClassExpression(subclass, commands, iri_node_str_dict)

            commands.append('SubClass')
            commands.extend(sub_class_commands)

        elif axiom.getAxiomType().toString() == self.AxiomType.DISJOINT_CLASSES.toString():

            count = 0

            class_expressions = axiom.getClassExpressions();
            itr_class_expressions = class_expressions.iterator()
            while itr_class_expressions.hasNext():
                ce = itr_class_expressions.next()
                disjoint_commands = self.Process_ClassExpression(ce, commands, iri_node_str_dict)

                commands.append('DisjointClass' + str(count))
                commands.extend(disjoint_commands)

                count = count + 1

        elif axiom.getAxiomType().toString() == self.AxiomType.EQUIVALENT_CLASSES.toString():

            count = 0

            class_expressions = axiom.getClassExpressions();
            itr_class_expressions = class_expressions.iterator()
            while itr_class_expressions.hasNext():
                ce = itr_class_expressions.next()
                equivalent_commands = self.Process_ClassExpression(ce, commands, iri_node_str_dict)

                commands.append('EquivalentClass' + str(count))
                commands.extend(equivalent_commands)

                count = count + 1
        else:
            pass

        return commands

    def Process_SubObjectPropertyOF_axioms(self, axioms, nodes_dict, edges_dict, iri_node_str_dict, is_chain):

        for i, axiom in enumerate(axioms):

            # print(i,'/',len(axioms))
            # print('axiom.toString()',axiom.toString())

            depth = 0

            if is_chain:
                chain_raw = axiom.getPropertyChain()
                # chain = cast(self.List, chain_raw)
                chain_itr = chain_raw.iterator()
                chain = []
                while chain_itr.hasNext():
                    chain.append(chain_itr.next())

                sub_opes = []

                for c in chain:
                    sub_ope = self.Process_ObjectPropertyExpression_2(c, nodes_dict, iri_node_str_dict, depth)
                    inverse = sub_ope[1]
                    if inverse:
                        inverse_node = self.diagram_to_place.factory.create(Item.RoleInverseNode)
                        iri_to_append = self.IRI.create('')
                        nodes_dict[str(inverse_node)] = []
                        edges_dict[Item.InputEdge.value].append([sub_ope[0], inverse_node])
                        iri_node_str_dict[str(inverse_node)] = [inverse_node, iri_to_append]
                        sub_opes.append(inverse_node)
                    else:
                        sub_opes.append(sub_ope[0])
                sub_ope_processed = self.diagram_to_place.factory.create(Item.RoleChainNode)
                iri_to_append = self.IRI.create('')
                iri_node_str_dict[str(sub_ope_processed)] = [sub_ope_processed, iri_to_append]
                nodes_dict[str(sub_ope_processed)] = []
                for s in sub_opes:
                    nodes_dict[str(sub_ope_processed)].append(s)
                    edges_dict[Item.InputEdge.value].append([s, sub_ope_processed])

            else:
                sub_ope_raw = axiom.getSubProperty();
                sub_ope = self.Process_ObjectPropertyExpression_2(sub_ope_raw, nodes_dict, iri_node_str_dict, depth)
                inverse = sub_ope[1]
                if inverse:
                    inverse_node = self.diagram_to_place.factory.create(Item.RoleInverseNode)
                    iri_to_append = self.IRI.create('')
                    nodes_dict[str(inverse_node)] = []
                    edges_dict[Item.InputEdge.value].append([sub_ope[0], inverse_node])
                    iri_node_str_dict[str(inverse_node)] = [inverse_node, iri_to_append]
                    sub_ope_processed = inverse_node
                else:
                    sub_ope_processed = sub_ope[0]

            super_ope_raw = axiom.getSuperProperty();
            super_ope = self.Process_ObjectPropertyExpression_2(super_ope_raw, nodes_dict, iri_node_str_dict, depth)
            if inverse:
                inverse_node = self.diagram_to_place.factory.create(Item.RoleInverseNode)
                iri_to_append = self.IRI.create('')
                nodes_dict[str(inverse_node)] = []
                edges_dict[Item.InputEdge.value].append([super_ope[0], inverse_node])
                iri_node_str_dict[str(inverse_node)] = [inverse_node, iri_to_append]
                super_ope_processed = inverse_node
            else:
                super_ope_processed = super_ope[0]

            if str(super_ope_processed) not in nodes_dict.keys():
                nodes_dict[str(super_ope_processed)] = []
            nodes_dict[str(super_ope_processed)].append(sub_ope_processed)
            edges_dict[Item.InclusionEdge.value].append([sub_ope_processed, super_ope_processed])

    def Process_InverseObjectProperties_axioms(self, axioms, nodes_dict, edges_dict, iri_node_str_dict):

        for i, axiom in enumerate(axioms):

            # print(i,'/',len(axioms))
            # print('axiom.toString()',axiom.toString())

            depth = 0

            first_ope_raw = axiom.getFirstProperty();
            first_ope = self.Process_ObjectPropertyExpression_2(first_ope_raw, nodes_dict, iri_node_str_dict, depth)
            inverse = first_ope[1]
            if inverse:
                inverse_node = self.diagram_to_place.factory.create(Item.RoleInverseNode)
                iri_to_append = self.IRI.create('')
                nodes_dict[str(inverse_node)] = []
                edges_dict[Item.InputEdge.value].append([first_ope[0], inverse_node])
                iri_node_str_dict[str(inverse_node)] = [inverse_node, iri_to_append]
                first_ope_processed = inverse_node
            else:
                first_ope_processed = first_ope[0]

            second_ope_raw = axiom.getSecondProperty();
            second_ope = self.Process_ObjectPropertyExpression_2(second_ope_raw, nodes_dict, iri_node_str_dict, depth)
            if inverse:
                inverse_node = self.diagram_to_place.factory.create(Item.RoleInverseNode)
                iri_to_append = self.IRI.create('')
                nodes_dict[str(inverse_node)] = []
                edges_dict[Item.InputEdge.value].append([second_ope[0], inverse_node])
                iri_node_str_dict[str(inverse_node)] = [inverse_node, iri_to_append]
                second_ope_processed = inverse_node
            else:
                second_ope_processed = second_ope[0]

            main_inverse_node = self.diagram_to_place.factory.create(Item.RoleInverseNode)
            iri_to_append = self.IRI.create('')
            iri_node_str_dict[str(main_inverse_node)] = [main_inverse_node, iri_to_append]
            nodes_dict[str(main_inverse_node)] = [first_ope_processed, second_ope_processed]
            edges_dict[Item.InputEdge.value].append([first_ope_processed, main_inverse_node])
            edges_dict[Item.EquivalenceEdge.value].append([main_inverse_node, second_ope_processed])

    def Process_SubDataPropertyOF_axioms(self, axioms, nodes_dict, edges_dict, iri_node_str_dict):

        for i, axiom in enumerate(axioms):
            # print(i,'/',len(axioms))
            # print('axiom.toString()',axiom.toString())

            depth = 0

            sub_dpe = axiom.getSubProperty();
            processed_sub_dpe = self.Process_DataPropertyExpression_2(sub_dpe, nodes_dict, iri_node_str_dict, depth)

            super_dpe = axiom.getSuperProperty();
            processed_super_dpe = self.Process_DataPropertyExpression_2(super_dpe, nodes_dict, iri_node_str_dict, depth)

            nodes_dict[str(processed_super_dpe)].append(processed_sub_dpe)
            edges_dict[Item.InclusionEdge.value].append([processed_sub_dpe, processed_super_dpe])

    def Process_SubClassOF_axioms(self, axioms, nodes_dict, edges_dict, iri_node_str_dict,
                                  object_or_data_property_domain_or_range):

        # 1 Object Property Domain
        # 2 Object Property Range
        # 3 Data Property Domain
        # 4 Data Property Range

        for i, axiom in enumerate(axioms):

            # print(i,'/',len(axioms))
            # print('axiom.toString()',axiom.toString())

            depth = 0
            if object_or_data_property_domain_or_range in [1, 3]:
                super_expression = axiom.getDomain()
            elif object_or_data_property_domain_or_range in [2, 4]:
                super_expression = axiom.getRange()
            else:
                super_expression = axiom.getSuperClass()

            if object_or_data_property_domain_or_range == 4:
                # casted_super_expression = cast(self.OWLDataRange, super_expression)
                super_root_node = self.Process_DataRange(super_expression, nodes_dict, edges_dict, iri_node_str_dict,
                                                         depth)
            else:
                # casted_super_expression = cast(self.OWLClassExpression, super_expression)
                super_root_node = self.Process_ClassExpression_2(super_expression, nodes_dict, edges_dict,
                                                                 iri_node_str_dict, depth)

            depth = 0
            if object_or_data_property_domain_or_range in [1, 2]:

                sub_op_expression = axiom.getProperty()
                # casted_op_expression = cast(self.OWLObjectPropertyExpression, sub_op_expression)
                sub_root_node_ope = self.Process_ObjectPropertyExpression_2(sub_op_expression, nodes_dict,
                                                                            iri_node_str_dict, depth)

                iri_to_append = self.IRI.create('')
                if object_or_data_property_domain_or_range == 1:
                    sub_root_node = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    sub_root_node = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                nodes_dict[str(sub_root_node)] = [sub_root_node_ope[0]]
                edges_dict[Item.InputEdge.value].append([sub_root_node_ope[0], sub_root_node])
                iri_node_str_dict[str(sub_root_node)] = [sub_root_node, iri_to_append]
            elif object_or_data_property_domain_or_range in [3, 4]:

                sub_dp_expression = axiom.getProperty()
                # casted_dp_expression = cast(self.OWLDataPropertyExpression, sub_dp_expression)
                sub_root_node_dpe = self.Process_DataPropertyExpression_2(sub_dp_expression, nodes_dict,
                                                                          iri_node_str_dict, depth)

                iri_to_append = self.IRI.create('')
                if object_or_data_property_domain_or_range == 3:
                    sub_root_node = self.diagram_to_place.factory.create(Item.DomainRestrictionNode)
                else:
                    sub_root_node = self.diagram_to_place.factory.create(Item.RangeRestrictionNode)
                nodes_dict[str(sub_root_node)] = [sub_root_node_dpe]
                edges_dict[Item.InputEdge.value].append([sub_root_node_dpe, sub_root_node])
                iri_node_str_dict[str(sub_root_node)] = [sub_root_node, iri_to_append]
            else:

                sub_cl_expression = axiom.getSubClass()
                # casted_subclass_expression = cast(self.OWLClassExpression, sub_cl_expression)
                sub_root_node = self.Process_ClassExpression_2(sub_cl_expression, nodes_dict, edges_dict,
                                                               iri_node_str_dict, depth)

            nodes_dict[str(super_root_node)].append(sub_root_node)
            edges_dict[Item.InclusionEdge.value].append([sub_root_node, super_root_node])

            # if object_or_data_property_domain_or_range in [1, 2, 3, 4]:

            ExFallCar_node = None
            class_expression_OR_value_domain_node = None

            if ('DomainRestrictionNode:' in str(sub_root_node)) or ('RangeRestrictionNode:' in str(sub_root_node)):
                ExFallCar_node = sub_root_node
                class_expression_OR_value_domain_node = super_root_node
            elif ('DomainRestrictionNode:' in str(super_root_node)) or (
                        'RangeRestrictionNode:' in str(super_root_node)):
                ExFallCar_node = super_root_node
                class_expression_OR_value_domain_node = sub_root_node
            else:
                pass

            if ExFallCar_node is not None:
                if str(ExFallCar_node) not in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
                    self.AttributesANDRoles_to_DomainANDRange_mapping[str(ExFallCar_node)] = []
                self.AttributesANDRoles_to_DomainANDRange_mapping[str(ExFallCar_node)].append(
                    class_expression_OR_value_domain_node)

    def Process_EquivalentClasses_axioms(self, axioms, nodes_dict, edge_dict, iri_node_str_dict):

        for i, axiom in enumerate(axioms):

            equivalent_classes = []
            eqcls_placed = []

            class_expressions = axiom.getClassExpressions()
            itr_class_expressions = class_expressions.iterator()
            while itr_class_expressions.hasNext():
                ce = itr_class_expressions.next()
                depth = 0

                nodes_dict_copy = nodes_dict.copy()

                equivalent_root_node = self.Process_ClassExpression_2(ce, nodes_dict, edge_dict, iri_node_str_dict,
                                                                      depth)
                equivalent_classes.append(equivalent_root_node)

                if str(equivalent_root_node) in nodes_dict_copy.keys():
                    eqcls_placed.append(True)
                else:
                    eqcls_placed.append(False)

            for i in range(0, len(equivalent_classes) - 1):
                ec1 = equivalent_classes[i]
                if str(ec1) not in nodes_dict.keys():
                    nodes_dict[str(ec1)] = []
                cl1_placed = eqcls_placed[i]

                for j in range(i + 1, len(equivalent_classes)):
                    ec2 = equivalent_classes[j]
                    if str(ec2) not in nodes_dict.keys():
                        nodes_dict[str(ec2)] = []
                    cl2_placed = eqcls_placed[j]

                    edge_dict[Item.EquivalenceEdge.value].append([ec1, ec2])

                    if cl1_placed and cl2_placed:
                        # if both are placed -> do nothing
                        pass
                    elif cl1_placed and (not cl2_placed):
                        # place nd2 below nd1
                        nodes_dict[str(ec1)].append(ec2)
                    elif (not cl1_placed) and cl2_placed:
                        # place nd1 below nd2
                        nodes_dict[str(ec2)].append(ec1)
                    elif (not cl1_placed) and (not cl2_placed):
                        # if both are not placed, randomly place one below the other
                        nodes_dict[str(ec1)].append(ec2)

                    if ('DomainRestrictionNode:' in str(ec1)) or (
                                'RangeRestrictionNode:' in str(ec1)):
                        ExFallCar_node = ec1
                        class_expression_node = ec2
                    elif ('DomainRestrictionNode:' in str(ec2)) or (
                                'RangeRestrictionNode:' in str(ec2)):
                        ExFallCar_node = ec2
                        class_expression_node = ec1
                    else:
                        ExFallCar_node = None
                        class_expression_node = None

                    if ExFallCar_node is not None:
                        if str(ExFallCar_node) not in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
                            self.AttributesANDRoles_to_DomainANDRange_mapping[str(ExFallCar_node)] = []
                        self.AttributesANDRoles_to_DomainANDRange_mapping[str(ExFallCar_node)].append(
                            class_expression_node)

    def Process_EquivalentObjectANDDataProperties_axioms(self, axioms, nodes_dict, edge_dict, iri_node_str_dict,
                                                         entity_type):

        for i, axiom in enumerate(axioms):

            equivalent_entities = []

            entity_expressions = axiom.getProperties()

            itr_entity_expressions = entity_expressions.iterator()
            while itr_entity_expressions.hasNext():
                ee = itr_entity_expressions.next()
                depth = 0

                if entity_type == 0:
                    # casted_ee = cast(self.OWLObjectPropertyExpression, ee)
                    equivalent_root_node = self.Process_ObjectPropertyExpression_2(ee, nodes_dict, edge_dict,
                                                                                   iri_node_str_dict, depth)
                else:
                    # casted_ee = cast(self.OWLDataPropertyExpression, ee)
                    equivalent_root_node = self.Process_DataPropertyExpression_2(ee, nodes_dict, edge_dict,
                                                                                 iri_node_str_dict, depth)

                equivalent_entities.append(equivalent_root_node)

            for i in range(0, len(equivalent_entities) - 1):
                ee1 = equivalent_entities[i]
                if str(ee1) not in nodes_dict.keys():
                    nodes_dict[str(ee1)] = []

                for j in range(i + 1, len(equivalent_entities)):
                    ee2 = equivalent_entities[j]
                    if str(ee2) not in nodes_dict.keys():
                        nodes_dict[str(ee2)] = []

                    edge_dict[Item.EquivalenceEdge.value].append([ee1, ee2])
                    nodes_dict[str(ee1)].append(ee2)

    def Process_DisjointClasses_axioms_2(self, nodes_dict, edge_dict, iri_node_str_dict):

        # print('Process_DisjointClasses_axioms_2')

        for ax_no, dis_ces in enumerate(self.disjoint_class_expressions_for_all_DisjointClassesAxioms):
            if ax_no in self.disjoint_class_expressionsNos_for_all_DisjointClassesAxioms_to_be_ignored:
                continue
            else:
                disjoint_classes = []
                dtcls_placed = []

                # print('len(dis_ces)',len(dis_ces))
                # print('dis_ces',dis_ces)

                for ce in dis_ces:
                    depth = 0
                    nodes_dict_copy = nodes_dict.copy()

                    disjoint_root_node = self.Process_ClassExpression_2(ce, nodes_dict, edge_dict, iri_node_str_dict,
                                                                        depth)
                    disjoint_classes.append(disjoint_root_node)

                    if str(disjoint_root_node) in nodes_dict_copy.keys():
                        dtcls_placed.append(True)
                    else:
                        dtcls_placed.append(False)

                for i in range(0, len(disjoint_classes) - 1):

                    dc1 = disjoint_classes[i]
                    if str(dc1) not in nodes_dict.keys():
                        nodes_dict[str(dc1)] = []
                    cl1_placed = dtcls_placed[i]

                    for j in range(i + 1, len(disjoint_classes)):

                        dc2 = disjoint_classes[j]

                        if str(dc2) not in nodes_dict.keys():
                            nodes_dict[str(dc2)] = []
                        cl2_placed = dtcls_placed[j]

                        not_node = self.diagram_to_place.factory.create(Item.ComplementNode)
                        iri_node_str_dict[str(not_node)] = [not_node, self.IRI.create('')]
                        nodes_dict[str(not_node)] = [dc1, dc2]

                        if cl1_placed and cl2_placed:
                            # if both are placed ->
                            edge_dict[Item.InputEdge.value].append([dc1, not_node])
                            edge_dict[Item.InclusionEdge.value].append([dc2, not_node])

                        elif cl1_placed and (not cl2_placed):
                            edge_dict[Item.InputEdge.value].append([dc2, not_node])
                            edge_dict[Item.InclusionEdge.value].append([dc1, not_node])

                        elif (not cl1_placed) and cl2_placed:
                            edge_dict[Item.InputEdge.value].append([dc1, not_node])
                            edge_dict[Item.InclusionEdge.value].append([dc2, not_node])

                        elif (not cl1_placed) and (not cl2_placed):
                            # if both are not placed
                            edge_dict[Item.InputEdge.value].append([dc1, not_node])
                            edge_dict[Item.InclusionEdge.value].append([dc2, not_node])

    def Process_DisjointClasses_axioms(self, axioms, nodes_dict, edge_dict, iri_node_str_dict):

        for i, axiom in enumerate(axioms):

            disjoint_classes = []
            dtcls_placed = []

            class_expressions = axiom.getClassExpressions()
            itr_class_expressions = class_expressions.iterator()
            while itr_class_expressions.hasNext():
                ce = itr_class_expressions.next()
                depth = 0
                nodes_dict_copy = nodes_dict.copy()

                disjoint_root_node = self.Process_ClassExpression_2(ce, nodes_dict, edge_dict, iri_node_str_dict, depth)
                disjoint_classes.append(disjoint_root_node)

                if str(disjoint_root_node) in nodes_dict_copy.keys():
                    dtcls_placed.append(True)
                else:
                    dtcls_placed.append(False)

            for i in range(0, len(disjoint_classes) - 1):

                dc1 = disjoint_classes[i]
                if str(dc1) not in nodes_dict.keys():
                    nodes_dict[str(dc1)] = []
                cl1_placed = dtcls_placed[i]

                for j in range(i + 1, len(disjoint_classes)):

                    dc2 = disjoint_classes[j]

                    if str(dc2) not in nodes_dict.keys():
                        nodes_dict[str(dc2)] = []
                    cl2_placed = dtcls_placed[j]

                    not_node = self.diagram_to_place.factory.create(Item.ComplementNode)
                    iri_node_str_dict[str(not_node)] = [not_node, self.IRI.create('')]
                    nodes_dict[str(not_node)] = [dc1, dc2]

                    if cl1_placed and cl2_placed:
                        # if both are placed ->
                        edge_dict[Item.InputEdge.value].append([dc1, not_node])
                        edge_dict[Item.InclusionEdge.value].append([dc2, not_node])

                    elif cl1_placed and (not cl2_placed):
                        edge_dict[Item.InputEdge.value].append([dc2, not_node])
                        edge_dict[Item.InclusionEdge.value].append([dc1, not_node])

                    elif (not cl1_placed) and cl2_placed:
                        edge_dict[Item.InputEdge.value].append([dc1, not_node])
                        edge_dict[Item.InclusionEdge.value].append([dc2, not_node])

                    elif (not cl1_placed) and (not cl2_placed):
                        # if both are not placed
                        edge_dict[Item.InputEdge.value].append([dc1, not_node])
                        edge_dict[Item.InclusionEdge.value].append([dc2, not_node])

    def Process_DisjointObjectANDDataProperties_axioms(self, axioms, nodes_dict, edge_dict, iri_node_str_dict,
                                                       entity_type):

        for i, axiom in enumerate(axioms):

            disjoint_entities = []

            entity_expressions = axiom.getProperties()

            itr_entity_expressions = entity_expressions.iterator()
            while itr_entity_expressions.hasNext():
                de = itr_entity_expressions.next()
                depth = 0

                if entity_type == 0:
                    # casted_de = cast(self.OWLObjectPropertyExpression, de)
                    disjoint_root_node = self.Process_ObjectPropertyExpression_2(de, nodes_dict, edge_dict,
                                                                                 iri_node_str_dict, depth)
                else:
                    # casted_de = cast(self.OWLDataPropertyExpression, de)
                    disjoint_root_node = self.Process_DataPropertyExpression_2(de, nodes_dict, edge_dict,
                                                                               iri_node_str_dict, depth)

                disjoint_entities.append(disjoint_root_node)

            for i in range(0, len(disjoint_entities) - 1):
                de1 = disjoint_entities[i]
                if str(de1) not in nodes_dict.keys():
                    nodes_dict[str(de1)] = []

                for j in range(i + 1, len(disjoint_entities)):
                    de2 = disjoint_entities[j]
                    if str(de2) not in nodes_dict.keys():
                        nodes_dict[str(de2)] = []

                    not_node = self.diagram_to_place.factory.create(Item.ComplementNode)
                    iri_node_str_dict[str(not_node)] = [not_node, self.IRI.create('')]
                    nodes_dict[str(not_node)] = [de1, de2]

                    # if both are placed ->
                    edge_dict[Item.InputEdge.value].append([de1, not_node])
                    edge_dict[Item.InclusionEdge.value].append([de2, not_node])

    def Process_DisjointUnion_axioms(self, axioms, nodes_dict, edges_dict, iri_node_str_dict):

        for axiom in axioms:
            super_cl = axiom.getOWLClass()
            class_expressions = axiom.getClassExpressions()
            class_expressions_itr = class_expressions.iterator()

            super_node = self.Process_ClassExpression_2(super_cl, nodes_dict, edges_dict,
                                                        iri_node_str_dict, 0)

            disjoint_union_node = self.diagram_to_place.factory.create(Item.DisjointUnionNode)

            if str(disjoint_union_node) not in nodes_dict.keys():
                nodes_dict[str(disjoint_union_node)] = []
            nodes_dict[str(super_node)].append(disjoint_union_node)
            edges_dict[Item.InclusionEdge.value].append([disjoint_union_node, super_node])
            iri_node_str_dict[str(disjoint_union_node)] = [disjoint_union_node, self.IRI.create('')]

            while class_expressions_itr.hasNext():

                cl = class_expressions_itr.next()

                depth = 0

                sub_root_node = self.Process_ClassExpression_2(cl, nodes_dict, edges_dict,
                                                               iri_node_str_dict, depth)

                nodes_dict[str(disjoint_union_node)].append(sub_root_node)
                edges_dict[Item.InputEdge.value].append([sub_root_node, disjoint_union_node])

                ExFallCar_node = None

                if ('DomainRestrictionNode:' in str(sub_root_node)) or ('RangeRestrictionNode:' in str(sub_root_node)):
                    ExFallCar_node = sub_root_node

                if ExFallCar_node is not None:
                    if str(ExFallCar_node) not in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
                        self.AttributesANDRoles_to_DomainANDRange_mapping[str(ExFallCar_node)] = []
                    self.AttributesANDRoles_to_DomainANDRange_mapping[str(ExFallCar_node)].append(
                        super_node)

    def Process_Assertion_axioms(self, axioms, nodes_dict, edges_dict, iri_node_str_dict, type_no):

        if type_no == 0:

            for ax in axioms:

                ce = ax.getClassExpression()
                casted_ce = cast(self.OWLClassExpression, ce)
                ce_node = self.Process_ClassExpression_2(casted_ce, nodes_dict, edges_dict, iri_node_str_dict, 0)

                if str(ce_node) not in nodes_dict.keys():
                    nodes_dict[str(ce_node)] = []

                individual = ax.getIndividual()
                casted_individual =  cast(self.OWLIndividual, individual)
                individual_node = self.Process_Individual(casted_individual, nodes_dict, iri_node_str_dict, 0)

                nodes_dict[str(ce_node)].append(individual_node)
                edges_dict[Item.MembershipEdge.value].append([individual_node, ce_node])

        elif (type_no == 11) or (type_no == 12):

            for ax in axioms:

                literal = ax.getObject()
                casted_literal = cast(self.OWLLiteral, literal)
                literal_node = self.Process_Literal(casted_literal, nodes_dict, iri_node_str_dict, 0)

                property = ax.getProperty()
                casted_property = cast(self.OWLDataPropertyExpression, property)
                property_node = self.Process_DataPropertyExpression_2(casted_property, nodes_dict, iri_node_str_dict, 0)

                if str(property_node) not in nodes_dict.keys():
                    nodes_dict[str(property_node)] = []

                individual = ax.getSubject()
                casted_individual = cast(self.OWLIndividual, individual)
                individual_node = self.Process_Individual(casted_individual, nodes_dict, iri_node_str_dict, 0)

                connector_node = self.diagram_to_place.factory.create(Item.PropertyAssertionNode)
                nodes_dict[str(connector_node)] = []
                iri_node_str_dict[str(connector_node)] = [connector_node, self.IRI.create('')]

                nodes_dict[str(connector_node)].append(individual_node)
                nodes_dict[str(connector_node)].append(literal_node)
                edges_dict[Item.InputEdge.value].append([individual_node, connector_node])
                edges_dict[Item.InputEdge.value].append([literal_node, connector_node])

                if (type_no == 11):
                    nodes_dict[str(property_node)].append(connector_node)
                    edges_dict[Item.MembershipEdge.value].append([connector_node, property_node])
                else:
                    NOT_node = self.diagram_to_place.factory.create(Item.ComplementNode)
                    nodes_dict[str(NOT_node)] = []
                    iri_node_str_dict[str(NOT_node)] = [NOT_node, self.IRI.create('')]

                    nodes_dict[str(NOT_node)].append(connector_node)
                    nodes_dict[str(NOT_node)].append(property_node)
                    edges_dict[Item.MembershipEdge.value].append([connector_node, NOT_node])

                    edges_dict[Item.InputEdge.value].append([property_node, NOT_node])


        elif (type_no == 21) or (type_no == 22):

            for ax in axioms:

                individual_object = ax.getObject()
                casted_individual_object = cast(self.OWLIndividual, individual_object)
                individual_object_node = self.Process_Individual(casted_individual_object, nodes_dict, iri_node_str_dict, 0)

                property = ax.getProperty()
                casted_property = cast(self.OWLObjectPropertyExpression, property)
                property_node = self.Process_ObjectPropertyExpression_2(casted_property, nodes_dict, iri_node_str_dict, 0)

                if str(property_node) not in nodes_dict.keys():
                    nodes_dict[str(property_node)] = []

                individual_subject = ax.getSubject()
                casted_individual_subject = cast(self.OWLIndividual, individual_subject)
                individual_subject_node = self.Process_Individual(casted_individual_subject, nodes_dict, iri_node_str_dict, 0)

                connector_node = self.diagram_to_place.factory.create(Item.PropertyAssertionNode)
                nodes_dict[str(connector_node)] = []
                iri_node_str_dict[str(connector_node)] = [connector_node, self.IRI.create('')]

                nodes_dict[str(connector_node)].append(individual_object_node)
                nodes_dict[str(connector_node)].append(individual_subject_node)
                edges_dict[Item.InputEdge.value].append([individual_object_node, connector_node])
                edges_dict[Item.InputEdge.value].append([individual_subject_node, connector_node])

                if (type_no == 21):
                    nodes_dict[str(property_node)].append(connector_node)
                    edges_dict[Item.MembershipEdge.value].append([connector_node, property_node])
                else:
                    NOT_node = self.diagram_to_place.factory.create(Item.ComplementNode)
                    nodes_dict[str(NOT_node)] = []
                    iri_node_str_dict[str(NOT_node)] = [NOT_node, self.IRI.create('')]

                    nodes_dict[str(NOT_node)].append(connector_node)
                    nodes_dict[str(NOT_node)].append(property_node)
                    edges_dict[Item.MembershipEdge.value].append([connector_node, NOT_node])

                    edges_dict[Item.InputEdge.value].append([property_node, NOT_node])

        else:
            pass

    def fetch_ontology_from_file(self, filename_inp):

        try:
            file = self.File(filename_inp)
            # print('file.exists()',file.exists())
            self.ontology = self.man.loadOntologyFromOntologyDocument(file);
        except():
            print('error reading file')

    def convert_axioms_to_nodes_and_edges_or_metadata(self, iri_node_str_dict):

        declaration_commands = []
        class_commands_1 = []
        class_commands_2 = []
        class_commands_3 = []

        SubClassOf_axioms = self.ontology.getAxioms(self.AxiomType.SUBCLASS_OF)
        DisjointClass_axioms = self.ontology.getAxioms(self.AxiomType.DISJOINT_CLASSES)
        EquivalentClass_axioms = self.ontology.getAxioms(self.AxiomType.EQUIVALENT_CLASSES)
        # cast(self.Set, SubClassOf_axioms)
        # cast(self.Set, DisjointClass_axioms)
        # cast(self.Set, EquivalentClass_axioms)

        itr_1 = SubClassOf_axioms.iterator()
        itr_2 = DisjointClass_axioms.iterator()
        itr_3 = EquivalentClass_axioms.iterator()

        while itr_1.hasNext():
            a = itr_1.next()
            class_commands_1.append(self.Process_Class_axiom(a, iri_node_str_dict))
            # self.Process_Class_axiom(a, graph_dict_subclassof, graph_dict_edges)
        while itr_2.hasNext():
            a = itr_2.next()
            class_commands_2.append(self.Process_Class_axiom(a, iri_node_str_dict))
            # self.Process_Class_axiom(a, graph_dict_nodes_disjoint_classes, graph_dict_edges)
        while itr_3.hasNext():
            a = itr_3.next()
            class_commands_3.append(self.Process_Class_axiom(a, iri_node_str_dict))
            # self.Process_Class_axiom(a, graph_dict_nodes_equivalent_classes, graph_dict_edges)
        ##########################
        """
        ###  Declaration axioms ###
        declaration_axioms = self.ontology.getAxioms(self.AxiomType.DECLARATION)
        cast(self.Set, declaration_axioms)
        declaration_axioms_itr = declaration_axioms.iterator()
        while declaration_axioms_itr.hasNext():

            decl_axiom_to_process = declaration_axioms_itr.next()
            # cast(self.OWLAxiom, decl_axiom_to_process)
            op = self.Process_Declaration(decl_axiom_to_process, class_commands)
            if op is not None:
                declaration_commands.append(op)
        ###########################
        """
        """
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
                #op=self.Process_logical_axiom(axiom_to_process)
                #if op is not None:
                #    commands.append(op)
                flag = True

            if flag is False:
                #self.process_other_type_of_axiom(axiom_to_process)
                pass
        """
        return [declaration_commands, class_commands_1, class_commands_2, class_commands_3]
        # return [        graph_dict_subclassof, graph_dict_nodes_disjoint_classes, \
        #                graph_dict_nodes_equivalent_classes, graph_dict_edges]

    def get_subgraph(self, commands, graph_nodes):

        commands_duplicate = commands.copy()

        leaf_nodes = []
        parent_node = None

        i = len(commands_duplicate) - 1

        # repeat until there is just 1 leaf node:
        while (len(commands_duplicate) > 3) and (i >= 2):
            # get the leaf nodes

            c = commands_duplicate[i]

            if (str(type(c)) == '<class \'int\'>') and (c == 0):
                leaf_node = commands_duplicate[i - 2]
                leaf_nodes.append(leaf_node)
            elif (str(type(c)) == '<class \'int\'>') and (c > 0):
                parent_node = commands_duplicate[i - 2]

            if parent_node is not None:
                if str(parent_node) not in graph_nodes.keys():
                    graph_nodes[str(parent_node)] = []
                graph_nodes[str(parent_node)].extend(leaf_nodes.copy())

                for l in leaf_nodes:
                    if str(l) not in graph_nodes.keys():
                        graph_nodes[str(l)] = []

                leaf_nodes.clear()
                parent_node = None

                commands_duplicate = commands_duplicate[0:i].copy()
                commands_duplicate.append(0)

                i = len(commands_duplicate) - 1

            else:
                i = i - 3

                # find the parent of the leaf nodes
                # connect leaf node with parent
                # make parent as the leaf node

        if str(commands_duplicate[0]) not in graph_nodes.keys():
            graph_nodes[str(commands_duplicate[0])] = []

        return commands_duplicate[0]

    def get_root_nodes_of_graph(self, graph_dict):

        keys = graph_dict.keys()
        keys_set = set()

        all_values = set()

        for k in keys:
            # sub_str = k[0:k.index(':')]
            # if sub_str not in exclusion_list:
            keys_set.add(k)
            values = graph_dict[k]
            for v in values:
                # sub_str_2 = str(v)[0:str(v).index(':')]
                # if sub_str not in exclusion_list:
                all_values.add(str(v))

        diff = keys_set.difference(all_values)

        return diff

    def create_graph_sub_class_of(self, all_commands):

        graph_dict_nodes = dict()

        list_of_all_commands = []
        for cmds in all_commands:
            list_of_all_commands.extend(cmds)

        for axiom_commands in all_commands:

            # for i, c in enumerate(axiom_commands):
            # c[0]=superclass
            # for a in axiom_commands:
            #    print(str(type(a)), '-a1', a)
            # start from super and find the next index of Subclass
            # next_ind_subclass = commands_1.index('SubClass', i, len(commands_1))
            next_ind_subclass = axiom_commands.index('SubClass')
            # indices of superclass is from i+1 -> next_ind_subclass-1
            super_cl = axiom_commands[1:next_ind_subclass]
            sub_cl = []

            # for c in super_cl:
            #    print('super_cl', c)

            for j in range(next_ind_subclass + 1, len(axiom_commands)):
                c2 = axiom_commands[j]
                # if (str(type(c2)) == '<class \'str\'>') and ((c2 == 'SuperClass')):
                #    break
                sub_cl.append(c2)

            # for c in sub_cl:
            #    print('sub_cl', c)

            root_super = self.get_subgraph(super_cl, graph_dict_nodes)

            # for k in graph_dict_nodes.keys():
            #    print(k, '--k', graph_dict_nodes[k])

            root_sub = self.get_subgraph(sub_cl, graph_dict_nodes)

            graph_dict_nodes[str(root_super)].append(root_sub)

            # for k in graph_dict_nodes.keys():
            #    print(k, '-k', graph_dict_nodes[k])

            # for a in axiom_commands:
            #    print(str(type(a)), '-a2', a)

            """
            opr to be done together for all the 3 modules
            for super_str in graph_dict_nodes.keys():
                #print(str(type(super_str)), '!', super_str)
                super_node = self.get_node_and_iri_from_commands(list_of_all_commands, super_str)[0]
                subs = graph_dict_nodes[super_str]
                for s in subs:
                    if super_node.Type is Item.ConceptNode:
                        graph_dict_edges[Item.InclusionEdge.value].append([s, super_node])
                    elif super_node.Type in [Item.UnionNode, Item.ComplementNode, Item.DisjointUnionNode,
                                             Item.DisjointUnionNode, Item.IntersectionNode]:
                        graph_dict_edges[Item.InputEdge.value].append([s, super_node])
            """

        return graph_dict_nodes

    def create_graph_disjoint_classes(self, all_commands, iri_node_str_dict):

        graph_dict_nodes_disjoint = dict()

        for axiom_commands in all_commands:

            disjoint_class_expressions = []

            for i in range(0, len(axiom_commands) - 1):
                c1 = axiom_commands[i]
                if (str(type(c1)) == '<class \'str\'>') and ('Disjoint' in c1):

                    cmds = []

                    for j in range(i + 1, len(axiom_commands)):
                        c2 = axiom_commands[j]
                        if (str(type(c2)) == '<class \'str\'>') and ('Disjoint' in c2):
                            break
                        cmds.append(axiom_commands[j])

                    for cm in cmds:
                        print('-cm', cm)
                    print('---')
                    dis_ce = self.get_subgraph(cmds, graph_dict_nodes_disjoint)
                    disjoint_class_expressions.append(dis_ce)

            for i in range(0, len(disjoint_class_expressions) - 1):
                n1 = disjoint_class_expressions[i]
                for j in range(i + 1, len(disjoint_class_expressions)):
                    n2 = disjoint_class_expressions[j]

                    not_node = self.diagram_to_place.factory.create(Item.ComplementNode)
                    if str(n1) not in graph_dict_nodes_disjoint:
                        graph_dict_nodes_disjoint[str(n1)] = [not_node]
                    else:
                        graph_dict_nodes_disjoint[str(n1)].append(not_node)

                    graph_dict_nodes_disjoint[str(not_node)] = [n2]

                    iri_node_str_dict[str(not_node)] = [not_node, self.IRI.create('')]

                    # graph_dict_edges[Item.InclusionEdge].append([not_node, n2])  opr to be done all together for the 3 modules
                    # graph_dict_edges[Item.InputEdge].append([n1, not_node])

        return graph_dict_nodes_disjoint

    # not used
    def create_graph_equivalent_classes(self, all_commands, graph_dict_edges, iri_node_str_dict):

        graph_dict_nodes_equivalent = dict()

        for axiom_commands in all_commands:

            equivalent_class_expressions = []

            for i in range(0, len(axiom_commands) - 1):
                c1 = axiom_commands[i]
                if (str(type(c1)) == '<class \'str\'>') and ('Equivalent' in c1):

                    cmds = []

                    for j in range(i + 1, len(axiom_commands)):
                        c2 = axiom_commands[j]
                        if (str(type(c2)) == '<class \'str\'>') and ('Equivalent' in c2):
                            break
                        cmds.append(axiom_commands[j])
                    eql_ce = self.get_subgraph(cmds, graph_dict_nodes_equivalent)
                    equivalent_class_expressions.append(eql_ce)

            for i in range(0, len(equivalent_class_expressions) - 1):
                n1 = equivalent_class_expressions[i]
                if str(n1) not in graph_dict_nodes_equivalent.keys():
                    graph_dict_nodes_equivalent[str(n1)] = []

                for j in range(i + 1, len(equivalent_class_expressions)):
                    n2 = equivalent_class_expressions[j]
                    if str(n2) not in graph_dict_nodes_equivalent.keys():
                        graph_dict_nodes_equivalent[str(n2)] = []

                    fake_node = self.diagram_to_place.factory.create(Item.ConceptNode)
                    graph_dict_nodes_equivalent[str(fake_node)] = []
                    # graph_dict_nodes_equivalent[str(fake_node)].append([n1,n2])
                    graph_dict_nodes_equivalent[str(fake_node)].append(n1)
                    graph_dict_nodes_equivalent[str(fake_node)].append(n2)

                    iri_node_str_dict[str(fake_node)] = [fake_node, self.IRI.create('fake')]
                    # graph_dict_nodes_equivalent[str(n1)].append(n2)
                    # graph_dict_nodes_equivalent[str(n2)].append(n1)

                    graph_dict_edges[Item.EquivalenceEdge].append([n1, n2])

        return graph_dict_nodes_equivalent

    def get_dept_width_height_and_repeat_for_children(self, graph, dwh_dict, node_str, current_depth, stack_trace):

        # print('>>> dwh_of_nodes_dict')
        # print('node_str',node_str,'current_depth',current_depth)
        # for key in dwh_dict.keys():
        #    print(key, ' - ', dwh_dict[key])
        # print('>>> dwh_of_nodes_dict END')

        if node_str in dwh_dict.keys():
            # print('node_str in dwh_dict.keys()')
            dwh_dict[node_str][0].add(current_depth)
        else:
            # print('children = graph[node_str]')

            minimum_width = 1
            if node_str in self.attributes_for_classexpressions.keys():
                total_number_of_ExFallCar_nodes = self.attributes_for_classexpressions[node_str][-1]
                # print('total_number_of_ExFallCar_nodes',total_number_of_ExFallCar_nodes)
                number_of_lines_on_one_side = math.ceil(total_number_of_ExFallCar_nodes / 2)
                number_of_groups = math.ceil(number_of_lines_on_one_side / 4)
                minimum_width = 2 * number_of_groups + 1
                # print('node_str',node_str)
                # print('     minimum_width', minimum_width)

            children = graph[node_str]

            if len(children) == 0:
                values_to_set = []
                depths = set()
                depths.add(current_depth)
                values_to_set.append(depths)
                values_to_set.append([minimum_width, 1])
                values_to_set.append(1)
            else:
                for child in children:
                    if child not in stack_trace:
                        stack_trace.append(child)

                        self.get_dept_width_height_and_repeat_for_children(graph, dwh_dict, str(child),
                                                                           current_depth + 1, stack_trace)

                summation_width = 0
                summation_height = 0

                for child in children:
                    # if child not in stack_trace:
                    if str(child) in dwh_dict.keys():
                        values = dwh_dict[str(child)]
                        width_child = max(values[1])
                        summation_width = summation_width + width_child

                        height_child = values[2]
                        summation_height = summation_height + height_child

                values_to_set = []
                depth_values = set()
                depth_values.add(current_depth)
                values_to_set.append(depth_values)
                values_to_set.append([minimum_width, summation_width])
                values_to_set.append(summation_height + 1)

            dwh_dict[node_str] = values_to_set

    def get_node_and_iri_from_commands(self, commands, str_node):

        for i, c in enumerate(commands):
            if 'eddy.core.items.nodes' in str(type(c)):
                if str(c) == str_node:
                    iri = commands[i + 1]
                    return [c, iri]

        return [None, None]

    # not used
    def getfree_positions(self, pos_type, from_pos, number_of_positions, occupied_positions):

        if from_pos is not None:
            fpx = from_pos[0]
            fpy = from_pos[1]
        else:
            fpx = 0
            fpy = 0

        t = [fpx, fpy - 200]
        r = [fpx + 300, fpy]
        l = [fpx - 300, fpy]
        b = [fpx, fpy + 200]

        tr = [fpx + 300, fpy - 200]
        tl = [fpx - 300, fpy - 200]

        br = [fpx + 300, fpy + 200]
        bl = [fpx - 300, fpy + 200]

        if pos_type == 'top':
            priority = [t, tr, tl, r, l, b, br, bl]
        elif pos_type == 'right':
            priority = [r, tr, br, t, b, l, bl, tl]
        elif pos_type == 'left':
            priority = [l, tl, bl, t, b, r, br, tr]
        elif pos_type == 'bottom':
            priority = [b, br, bl, r, l, t, tr, tl]
        else:
            pass

        # get first position
        # recursively get subsequent positions
        return_result = []

        for p in priority:
            if p not in occupied_positions:

                return_result.append(p)
                if number_of_positions > 1:
                    new_occupied_positions = []
                    new_occupied_positions.extend(occupied_positions)
                    new_occupied_positions.append(p)

                    res = self.getfree_positions(pos_type, p, number_of_positions - 1, new_occupied_positions)
                    return_result.extend(res)

                break
            else:
                pass

        return return_result

    # not used
    def place_unplaced_equivalent_nodes_in_diagram(self, unplaced_nodes_classes, graph_equivalent_classes, edges_dict,
                                                   iri_node_str_dict):

        now_placed_classes = set()

        for pairs in edges_dict[Item.EquivalenceEdge.value]:

            eqcl_1 = pairs[0]
            eqcl_2 = pairs[1]

            eqcl_1_placed = str(eqcl_1) in self.occupied_positions_xy.keys()
            eqcl_2_placed = str(eqcl_2) in self.occupied_positions_xy.keys()

            if not eqcl_1_placed and not eqcl_2_placed:
                # place the objects below all other objects and draw the edge

                print('eqcl_2', eqcl_2)

                free_positions = self.getfree_positions('top', None, 2, self.occupied_positions_xy.values())

                free_pos_1 = free_positions[0]
                free_pos_2 = free_positions[1]

                new_xP = free_pos_1[0]
                new_yP = free_pos_1[1]
                print('new_xP', new_xP, ',new_yP', new_yP)
                # place p
                snapToGrid = self.session.action('toggle_grid').isChecked()
                eqcl_2.setPos(snap(QtCore.QPoint(new_xP, new_yP), Diagram.GridSize, snapToGrid))

                self.occupied_positions_xy[str(eqcl_2)] = [new_xP, new_yP]

                now_placed_classes.add(str(eqcl_2))
                #########
                print('eqcl_1', eqcl_1)

                new_xC = free_pos_2[0]
                new_yC = free_pos_2[1]
                print('new_xC', new_xC, ',new_yC', new_yC)
                # place p
                snapToGrid = self.session.action('toggle_grid').isChecked()
                eqcl_1.setPos(snap(QtCore.QPoint(new_xC, new_yC), Diagram.GridSize, snapToGrid))

                self.occupied_positions_xy[str(eqcl_1)] = [new_xC, new_yC]

                now_placed_classes.add(str(eqcl_1))

            elif eqcl_1_placed and not eqcl_2_placed:
                # place the parent_node as close as possible to the child_node and draw the edge
                print('eqcl_2', eqcl_2)

                free_positions = self.getfree_positions('top', self.occupied_positions_xy[str(eqcl_1)], 1,
                                                        self.occupied_positions_xy.values())

                free_pos_1 = free_positions[0]

                new_xP = free_pos_1[0]
                new_yP = free_pos_1[1]
                print('new_xP', new_xP, ',new_yP', new_yP)
                # place p
                snapToGrid = self.session.action('toggle_grid').isChecked()
                eqcl_2.setPos(snap(QtCore.QPoint(new_xP, new_yP), Diagram.GridSize, snapToGrid))

                self.occupied_positions_xy[str(eqcl_2)] = [new_xP, new_yP]

                now_placed_classes.add(str(eqcl_2))

            elif not eqcl_1_placed and eqcl_2_placed:
                # place the child_node as close as possible to the parent_node and draw the edge
                print('eqcl_1', eqcl_1)

                free_positions = self.getfree_positions('bottom', self.occupied_positions_xy[str(eqcl_2)], 1,
                                                        self.occupied_positions_xy.values())

                free_pos_1 = free_positions[0]

                new_xC = free_pos_1[0]
                new_yC = free_pos_1[1]
                print('new_xC', new_xC, ',new_yC', new_yC)

                # place p
                snapToGrid = self.session.action('toggle_grid').isChecked()
                eqcl_1.setPos(snap(QtCore.QPoint(new_xC, new_yC), Diagram.GridSize, snapToGrid))

                self.occupied_positions_xy[str(eqcl_1)] = [new_xC, new_yC]

                now_placed_classes.add(str(eqcl_1))
            else:
                pass

        return now_placed_classes

    def check_if_point_in_occupied_rectangles(self, point):

        X_point = point[0]
        Y_point = point[1]

        for rect in self.occupied_rectangles:
            # [max_X, max_Y, min_X, min_Y]
            max_X = rect[0]
            max_Y = rect[1]
            min_X = rect[2]
            min_Y = rect[3]

            if X_point <= max_X and X_point >= min_X:
                if Y_point <= max_Y and Y_point >= min_Y:
                    return True

        return False

    def get_free_points_around_a_point_at_a_specific_distance(self, center_point, dist, place_role):

        # print('>>>      get_free_points_around_a_point')

        # print('center_point',center_point)


        x_center_point = int(center_point[0] / 150)
        y_center_point = int(center_point[1] / 200)

        generated_points = []

        # for i in range((int(x_center_point/300)-dist), (int(x_center_point/300)+dist)):
        for i in range((x_center_point - dist), (x_center_point + dist)):
            # for j in range((int(y_center_point / 200) - dist), (int(y_center_point / 200) + dist)):
            for j in range((y_center_point - dist), (y_center_point + dist)):

                point = [150 * i, 100 * j]

                if place_role is False:
                    if (point[1] % 200 != 0):
                        continue
                else:
                    if (point[0] % 300 == 0 or point[1] % 200 == 0):
                        continue

                if (point in self.occupied_positions_xy.values()) or (
                        self.check_if_point_in_occupied_rectangles(point)):
                    pass
                else:
                    generated_points.append(point)

        generated_points_dict = dict()

        for gp in generated_points:
            x_gp = gp[0]
            y_gp = gp[1]
            if y_gp not in generated_points_dict.keys():
                generated_points_dict[y_gp] = []
            generated_points_dict[y_gp].append(x_gp)

        #if place_role:
        #    for role in sorted(generated_points_dict.keys()):
        #        gps = generated_points_dict[role]
                # print('role-',role)
                # for g in gps:
                #    print('g',g)

        # print('>>>      get_free_points_around_a_point END')

        return generated_points

    def get_free_points_around_a_point(self, center_point, dist, place_role):

        # print('>>>      get_free_points_around_a_point')

        # print('center_point',center_point)


        x_center_point = int(center_point[0] / 150)
        y_center_point = int(center_point[1] / 200)

        generated_points = []

        while (len(generated_points) == 0 or dist < 10):
            # for i in range((int(x_center_point/300)-dist), (int(x_center_point/300)+dist)):
            for i in range((x_center_point - dist), (x_center_point + dist)):
                # for j in range((int(y_center_point / 200) - dist), (int(y_center_point / 200) + dist)):
                for j in range((y_center_point - dist), (y_center_point + dist)):

                    if place_role is False:
                        point = [150 * i, 200 * j]
                    else:
                        point = [150 * i, 100 * j]
                        if (point[0] % 300 == 0 or point[1] % 200 == 0):
                            pass

                    if (point in self.occupied_positions_xy.values()) or (
                            self.check_if_point_in_occupied_rectangles(point)):
                        pass
                    else:
                        generated_points.append(point)
            dist = dist + 1

        generated_points_dict = dict()

        for gp in generated_points:
            x_gp = gp[0]
            y_gp = gp[1]
            if y_gp not in generated_points_dict.keys():
                generated_points_dict[y_gp] = []
            generated_points_dict[y_gp].append(x_gp)

        if place_role:
            for role in sorted(generated_points_dict.keys()):
                gps = generated_points_dict[role]
                print('role-', role)
                for g in gps:
                    print('g', g)

        # print('>>>      get_free_points_around_a_point END')

        return generated_points

    def compute_optimized_point_using_possible_points(self, list_of_possible_points):

        # print('>>>      compute_optimized_point_using_possible_points')
        # print('number of anchors = ',len(list_of_possible_points))
        # for pts in list_of_possible_points:
        #    print('pts',pts)


        if len(list_of_possible_points) == 0:
            print('>>>      compute_optimized_point_using_possible_points END')
            return None
        else:
            list_of_points = []
            anchors = []
            for points in list_of_possible_points:
                for i, p in enumerate(points):
                    if i == 0:
                        anchors.append(p)
                    else:
                        if p not in list_of_points:
                            list_of_points.append(p)

            sum_of_distances = []

            for p in list_of_points:
                px = p[0]
                py = p[1]
                sum_dist = 0.00
                for a in anchors:
                    ax = a[0]
                    ay = a[1]
                    dist = math.sqrt(math.pow((px - ax), 2) + math.pow((py - ay), 2))
                    sum_dist = sum_dist + dist
                sum_of_distances.append(sum_dist)

            min_sum_of_distances = min(sum_of_distances)
            index_min_sum_of_distances = sum_of_distances.index(min_sum_of_distances)

            best_point = list_of_points[index_min_sum_of_distances]

            # print('best_point', best_point)
            # print('>>>      compute_optimized_point_using_possible_points END')

            return best_point

    def compute_optimized_point_using_possible_points_2(self, list_of_anchor_points, place_role=False):

        if len(list_of_anchor_points) == 0:
            return None

        # print('place_role',place_role)
        # print('list_of_anchor_points',list_of_anchor_points)
        # print('list_of_anchor_points[:]', list_of_anchor_points[:])

        max_X = 0.0
        max_Y = 0.0

        for p in list_of_anchor_points:
            max_X = max(max_X, p[0])
            max_Y = max(max_Y, p[1])

        min_X = max_X
        min_Y = max_Y

        for p in list_of_anchor_points:
            min_X = min(min_X, p[0])
            min_Y = min(min_Y, p[1])

        min_X = math.floor(int(min_X) / 150)
        min_X = min_X * 150
        max_X = math.ceil(int(max_X) / 150)
        max_X = max_X * 150

        min_Y = math.floor(int(min_Y) / 100)
        min_Y = min_Y * 100
        max_Y = math.ceil(int(max_Y) / 100)
        max_Y = max_Y * 100

        # print('min_X',min_X)
        # print('min_Y',min_Y)
        # print('max_X',max_X)
        # print('max_Y',max_Y)

        all_vals_X = []
        all_vals_Y = []
        x_itr = []
        y_itr = []
        for x in range(min_X, max_X + 1, 150):
            # print('x',x)
            if place_role and (x % 300 == 0):
                continue

            sum_X = 0.0
            for p in list_of_anchor_points:
                sum_X = sum_X + abs(p[0] - x)
            # print('sum_X', sum_X)
            all_vals_X.append(sum_X)
            x_itr.append(x)

        for y in range(min_Y, max_Y + 1, 100):
            # for y in range(-1000, max_Y + 1, 100):
            # print('y', y)

            if (place_role is False) and (y % 200 != 0):
                continue
            if (place_role is True) and (y % 200 == 0):
                continue

            sum_Y = 0.0
            for p in list_of_anchor_points:
                sum_Y = sum_Y + abs(p[1] - y)
            # print('sum_Y', sum_Y)
            all_vals_Y.append(sum_Y)
            y_itr.append(y)

        # print('all_vals_X', all_vals_X)
        # print('all_vals_Y', all_vals_Y)

        if len(all_vals_X) == 0:
            all_vals_X.append(list_of_anchor_points[0][0])
            x_itr.append(list_of_anchor_points[0][0])
        if len(all_vals_Y) == 0:
            all_vals_Y.append(list_of_anchor_points[0][1])
            y_itr.append(list_of_anchor_points[0][1])

        min_X = x_itr[all_vals_X.index(min(all_vals_X))]
        min_Y = y_itr[all_vals_Y.index(min(all_vals_Y))]

        # if (([px, py]) in self.occupied_positions_xy.values()) or (self.check_if_point_in_occupied_rectangles([px, py])):

        generated_points = []
        dist = 1
        while (len(generated_points) == 0):
            generated_points = self.get_free_points_around_a_point_at_a_specific_distance([min_X, min_Y], dist,
                                                                                          place_role)
            dist = dist + 1

        point_to_return = generated_points[0]

        # print('return_values-',point_to_return)

        return point_to_return

    def place_unplaced_node_aided_by_anchors(self, unplaced_node_str, children, anchors_for_unplaced_nodes,
                                             iri_node_str_dict, place_role=False):

        if place_role is False:
            if 'RoleNode:' in unplaced_node_str:
                place_role = True

        anchors_of_nd = anchors_for_unplaced_nodes[unplaced_node_str]

        # print('>>>      place_unplaced_node_aided_by_anchors')
        # print('      unplaced_node_str',unplaced_node_str)
        # print('      anchors_of_nd', anchors_of_nd)

        # if len of anchor is 0, take the anchors of the children
        if len(anchors_of_nd) == 0:
            anchors_of_nd = []
            # children = sub_graph[und]
            for c in children:
                anchor_of_child = anchors_for_unplaced_nodes[str(c)]
                anchors_of_nd.extend(anchor_of_child)

        dist = 20

        possible_points_union_anchor = []
        weighted_average_point = None
        all_anchor_points = []

        for anchor in anchors_of_nd:
            # print('anchor',anchor)
            pos_anchor = self.occupied_positions_xy[anchor]
            all_anchor_points.append(pos_anchor)
            # pts_around_anchor = self.get_free_points_around_a_point(pos_anchor,dist,place_role)
            # temp = [pos_anchor]
            # temp.extend(pts_around_anchor)
            # possible_points_union_anchor.append(temp)

        optimized_point = self.compute_optimized_point_using_possible_points_2(all_anchor_points, place_role)
        # optimized_point = self.compute_optimized_point_using_possible_points(possible_points_union_anchor)

        # print('optimized_point', optimized_point)

        if optimized_point is not None:
            # place nd
            snapToGrid = self.session.action('toggle_grid').isChecked()
            unplaced_node = iri_node_str_dict[unplaced_node_str][0]
            if unplaced_node is None:
                print('>>> place_nodes_in_diagram - node is None')
                LOGGER.critical('>>> place_nodes_in_diagram - node is None')
            unplaced_node.setPos(
                snap(QtCore.QPoint(optimized_point[0], optimized_point[1]), Diagram.GridSize, snapToGrid))

            self.occupied_positions_xy[unplaced_node_str] = optimized_point
            return True
        else:
            return False

    def place_subgraph_aided_by_anchors(self, sub_graph, anchors_for_unplaced_nodes, iri_node_str_dict):

        # root_nodes_of_subgraph = self.get_root_nodes_of_graph(sub_graph)
        for und in sub_graph.keys():
            # print('und',und)
            # print('anchors_for_unplaced_nodes', anchors_for_unplaced_nodes)
            children = sub_graph[und]
            placed = self.place_unplaced_node_aided_by_anchors(und, children, anchors_for_unplaced_nodes,
                                                               iri_node_str_dict)

        # remove all the keys that are placed given that all its children are placed as well
        key_to_remove = []

        for und in sub_graph.keys():

            parent_placed = und in self.occupied_positions_xy.keys()
            # print('parent_placed',parent_placed,' und',und)
            children = sub_graph[und]
            children_placed = set()
            for c in children:
                if str(c) in self.occupied_positions_xy.keys():
                    children_placed.add(True)
                else:
                    children_placed.add(False)

            # print('children_placed',children_placed)

            if parent_placed is True:
                if False not in children_placed:
                    # print('key_to_remove.append(und)')
                    key_to_remove.append(und)

        # print('key_to_remove',key_to_remove)

        for k in key_to_remove:
            del sub_graph[k]

            # for und in sub_graph.keys():
            # print('und*',und,' ',sub_graph[und])

        # print('')

        count = 0
        max_count = 0

        prev_to_place = set()

        change = True
        while (change and count < 10000):

            count = count + 1
            max_count = max(max_count, count)

            to_place = set()

            for und in sub_graph.keys():

                parent_placed = und in self.occupied_positions_xy.keys()
                children = sub_graph[und]
                children_placed = []
                for c in children:
                    if str(c) in self.occupied_positions_xy.keys():
                        children_placed.append(True)
                    else:
                        children_placed.append(False)

                if parent_placed is False:
                    if True not in children_placed:
                        # nothing can be done in this iteration
                        # print(und,' -nothing can be done in this iteration')
                        pass
                    else:
                        # make all the placed children as anchors
                        if und not in anchors_for_unplaced_nodes.keys():
                            anchors_for_unplaced_nodes[und] = []
                        for i, c in enumerate(children):
                            if children_placed[i] is True:
                                anchors_for_unplaced_nodes[und].append(str(c))
                                to_place.add(und)
                else:
                    if False not in children_placed:
                        # noting to place in this iteration
                        pass
                        # print(und, ' -noting to place in this iteration')
                    else:
                        # for all the unplaced children make the parent as the anchor
                        for i, c in enumerate(children):
                            if children_placed[i] is False:
                                if str(c) not in anchors_for_unplaced_nodes.keys():
                                    anchors_for_unplaced_nodes[str(c)] = []
                                anchors_for_unplaced_nodes[str(c)].append(und)
                                to_place.add(str(c))

            for t in to_place:
                # print('*** 2nd attempt ',t)
                children_of_t = sub_graph[t]
                self.place_unplaced_node_aided_by_anchors(t, children_of_t, anchors_for_unplaced_nodes,
                                                          iri_node_str_dict)

            # c1 = len(to_place) > 0
            c2 = prev_to_place.issubset(to_place)
            c3 = to_place.issubset(prev_to_place)

            change = not (c2 and c3)

            prev_to_place.clear()
            prev_to_place = to_place.copy()

        # print('max_count', max_count)

        still_unplaced = set()
        for und in sub_graph.keys():
            if und not in self.occupied_positions_xy.keys():
                still_unplaced.add(und)
            children = sub_graph[und]
            for c in children:
                if str(c) not in self.occupied_positions_xy.keys():
                    still_unplaced.add(str(c))

        # print('>>> still_unplaced',still_unplaced)

        return still_unplaced

    def identify_anchors_for_unplaced_nodes(self, unplaced_nodes, graph_nodes):

        anchors = dict()

        for un in unplaced_nodes:
            anchors[un] = []

        for graph_nd in graph_nodes.keys():
            children = graph_nodes[graph_nd]
            for c in children:
                if graph_nd in unplaced_nodes:
                    if str(c) not in unplaced_nodes:
                        anchors[graph_nd].append(str(c))
                else:
                    if str(c) in unplaced_nodes:
                        anchors[str(c)].append(graph_nd)

        return anchors

    def traverse_graph_dfs(self, root_node, graph_unplaced_nodes):

        # print(space+'root_node', root_node)

        return_result = set()
        return_result.add(root_node)

        children = graph_unplaced_nodes[root_node]

        for child in children:
            # result = self.traverse_graph_dfs(str(child), graph_unplaced_nodes, space+'      ')
            result = self.traverse_graph_dfs(str(child), graph_unplaced_nodes)
            return_result = return_result.union(result)

        # print(space+'return_result', return_result)

        return return_result

    def seperate_graph_into_disconnected_subgaphs(self, graph_unplaced_nodes):

        root_nodes = self.get_root_nodes_of_graph(graph_unplaced_nodes)
        traversal_result = []

        # for r in root_nodes:
        # print('un-r',r)

        for rn in root_nodes:
            # print('')
            # res = self.traverse_graph_dfs(rn, graph_unplaced_nodes,'')
            res = self.traverse_graph_dfs(rn, graph_unplaced_nodes)
            traversal_result.append(res)

            # print('len(traversal_result)', len(traversal_result))

            # for tr in traversal_result:
            # print('tr',tr)

        i = 0

        while (i < len(traversal_result) - 1):
            elements_1 = traversal_result[i]
            j = i + 1
            while (len(traversal_result) > 1) and (j < len(traversal_result)):
                elements_2 = traversal_result[j]
                intersection = elements_1.intersection(elements_2)
                if len(intersection) > 0:
                    # print('i', i)
                    # print('j', j)
                    # print('     elements_1', elements_1)
                    # print('     elements_2', elements_2)
                    # print('     *****   intersection    *****', intersection)
                    traversal_result[i] = traversal_result[i].union(elements_2)
                    # traversal_result[j].clear()
                    traversal_result.pop(j)
                    # print('     traversal_result', traversal_result)
                    i = -1
                    break
                else:
                    j = j + 1
            i = i + 1

        disconnected_graphs = []

        for tr in traversal_result:

            sub_graph = dict()

            for gr_nd in graph_unplaced_nodes:
                if gr_nd in tr:
                    sub_graph[gr_nd] = graph_unplaced_nodes[gr_nd]

            disconnected_graphs.append(sub_graph)

        return disconnected_graphs

    def construct_graph_of_unplaced_nodes(self, unplaced_nodes, graph_nodes):

        graph_of_unplaced_nodes = dict()

        for gn in graph_nodes.keys():
            if gn in unplaced_nodes:
                children = graph_nodes[gn]
                children_to_append = []
                for c in children:
                    if str(c) in unplaced_nodes:
                        children_to_append.append(c)
                graph_of_unplaced_nodes[gn] = children_to_append

        return graph_of_unplaced_nodes

    def place_individual_nodes_in_diagram(self, unplaced_nodes, graph_nodes):

        last_y_pos = 0
        for pos in self.occupied_positions_xy.values():
            last_y_pos = max(last_y_pos,pos[1])

        while last_y_pos%200 != 0:
            last_y_pos = last_y_pos+1

        # 1 for roles|attributes
        for entity in self.Entities_with_individuals.keys():
            if 'AttributeNode:' in entity or 'RoleNode:' in entity:
                entity_pos_ind_init = last_y_pos
                pos_entity = self.occupied_positions_xy[entity]
                ind_nodes = self.Entities_with_individuals[entity]

                for i_nd in ind_nodes:
                    if str(i_nd) not in self.occupied_positions_xy.keys():
                        entity_pos_ind_init = entity_pos_ind_init+50
                        new_pos = [pos_entity[0], entity_pos_ind_init]
                        while new_pos in self.occupied_positions_xy.values():
                            entity_pos_ind_init = entity_pos_ind_init + 50
                            new_pos = [pos_entity[0], entity_pos_ind_init]
                        # place p
                        snapToGrid = self.session.action('toggle_grid').isChecked()
                        i_nd.setPos(snap(QtCore.QPoint(pos_entity[0], entity_pos_ind_init), Diagram.GridSize, snapToGrid))

                        self.occupied_positions_xy[str(i_nd)] = [pos_entity[0], entity_pos_ind_init]


        # 2 for classes
        for entity in self.Entities_with_individuals.keys():
            if 'ConceptNode:' in entity :
                entity_pos_ind_init = last_y_pos
                pos_entity = self.occupied_positions_xy[entity]
                ind_nodes = self.Entities_with_individuals[entity]

                for i_nd in ind_nodes:
                    if str(i_nd) not in self.occupied_positions_xy.keys():
                        entity_pos_ind_init = entity_pos_ind_init + 50
                        new_pos = [pos_entity[0], entity_pos_ind_init]
                        while new_pos in self.occupied_positions_xy.values():
                            entity_pos_ind_init = entity_pos_ind_init + 50
                            new_pos = [pos_entity[0], entity_pos_ind_init]
                        # place p
                        snapToGrid = self.session.action('toggle_grid').isChecked()
                        i_nd.setPos(snap(QtCore.QPoint(pos_entity[0], entity_pos_ind_init), Diagram.GridSize, snapToGrid))

                        self.occupied_positions_xy[str(i_nd)] = [pos_entity[0], entity_pos_ind_init]

    def place_individual_nodes_in_diagram_2(self, graph_nodes_inds, iri_node_str_dict):

        last_y_pos = 0
        for pos in self.occupied_positions_xy.values():
            last_y_pos = max(last_y_pos,pos[1])

        while last_y_pos%200 != 0:
            last_y_pos = last_y_pos+1

        # 0 for complement nodes
        for str_nd in graph_nodes_inds.keys():
            if 'ComplementNode:' in str_nd:

                children = graph_nodes_inds[str_nd]
                if len(children) == 2:

                    if 'AttributeNode:' in str(children[0]) or 'RoleNode:' in str(children[0]):
                        entity_pos = self.occupied_positions_xy[str(children[0])]
                        if 'PropertyAssertionNode:' in str(children[1]):
                            connector_node = children[1]
                        else:
                            LOGGER.error('PropertyAssertionNode not in str(children[1] / programming error')
                            print('children',children)
                    else:
                        entity_pos = self.occupied_positions_xy[str(children[1])]
                        if 'PropertyAssertionNode:' in str(children[0]):
                            connector_node = children[0]
                        else:
                            LOGGER.error('PropertyAssertionNode not in str(children[0] / programming error')
                            print('children',children)

                    if connector_node:
                        new_point_connector_node = [entity_pos[0], last_y_pos+400]

                        snapToGrid = self.session.action('toggle_grid').isChecked()
                        connector_node.setPos(
                                    snap(QtCore.QPoint(new_point_connector_node[0], new_point_connector_node[1]), Diagram.GridSize,
                                         snapToGrid))
                        self.occupied_positions_xy[str(connector_node)] = new_point_connector_node



                    NOT_node = iri_node_str_dict[str_nd][0]
                    new_point_NOT_node = [entity_pos[0], last_y_pos+200]

                    snapToGrid = self.session.action('toggle_grid').isChecked()
                    NOT_node.setPos(
                        snap(QtCore.QPoint(new_point_NOT_node[0], new_point_NOT_node[1]), Diagram.GridSize,
                             snapToGrid))
                    self.occupied_positions_xy[str(NOT_node)] = new_point_NOT_node

                else:
                    LOGGER.error('Programming error len(children)!=2')
                    print('children',children)

        # 1 for connector nodes without NOT nodes
        for str_nd in graph_nodes_inds.keys():
            if 'AttributeNode:' in str_nd or 'RoleNode:' in str_nd:
                children = graph_nodes_inds[str_nd]

                children_connectors_only = []

                for child in children:
                    if 'PropertyAssertionNode:' in str(child):
                        children_connectors_only.append(child)

                if len(children_connectors_only) > 0:
                    entity_pos = self.occupied_positions_xy[str_nd]
                    #connector_children = graph_nodes[str(child)]

                    generated_points = self.generate_points_around_connector(len(children_connectors_only), [entity_pos[0], last_y_pos+400])

                    if len(generated_points) < len(children_connectors_only):
                        LOGGER.error('Programming error, len(generated_points) < len(connector_children), contact programmer')
                    else:
                        for i, cc in enumerate(children_connectors_only):

                            gp = generated_points[i]

                            snapToGrid = self.session.action('toggle_grid').isChecked()
                            cc.setPos(snap(QtCore.QPoint(gp[0], gp[1]), Diagram.GridSize, snapToGrid))
                            self.occupied_positions_xy[str(cc)] = [gp[0], gp[1]]

        # 2 for connectors
        for str_nd in graph_nodes_inds.keys():
            if 'PropertyAssertionNode:' in str_nd:

                connector_pos = self.occupied_positions_xy[str_nd]
                connector_children = graph_nodes_inds[str_nd]

                generated_points = self.generate_points_around_connector(len(connector_children),
                                                                         [connector_pos[0], connector_pos[1]+200])

                if len(generated_points) < len(connector_children):
                    LOGGER.error(
                        'Programming error, len(generated_points) < len(connector_children), contact programmer')
                else:
                    for i, child_of_cc in enumerate(connector_children):
                        if (str(child_of_cc) not in self.occupied_positions_xy.keys()):

                            gp = generated_points[i]

                            snapToGrid = self.session.action('toggle_grid').isChecked()
                            child_of_cc.setPos(snap(QtCore.QPoint(gp[0], gp[1]), Diagram.GridSize, snapToGrid))
                            self.occupied_positions_xy[str(child_of_cc)] = [gp[0], gp[1]]

        # 3 for classes
        for str_nd in graph_nodes_inds.keys():
            if 'ConceptNode:' in str_nd:

                children = graph_nodes_inds[str_nd]
                children_IndNodes_only = []

                for child in children:
                    if 'IndividualNode:' in str(child) and (str(child) not in self.occupied_positions_xy.keys()):
                        children_IndNodes_only.append(child)

                if len(children_IndNodes_only) > 0:

                    concept_pos = self.occupied_positions_xy[str_nd]
                    generated_points = self.generate_points_around_connector(len(children_IndNodes_only),
                                                                             [concept_pos[0], last_y_pos+200])

                    if len(generated_points) < len(children_IndNodes_only):
                        LOGGER.error(
                            'Programming error, len(generated_points) < len(children_IndNodes_only), contact programmer')
                    else:
                        for i, child_of_concept in enumerate(children_IndNodes_only):
                            if (str(child_of_concept) not in self.occupied_positions_xy.keys()):
                                gp = generated_points[i]

                                snapToGrid = self.session.action('toggle_grid').isChecked()
                                child_of_concept.setPos(snap(QtCore.QPoint(gp[0], gp[1]), Diagram.GridSize, snapToGrid))
                                self.occupied_positions_xy[str(child_of_concept)] = [gp[0], gp[1]]

    def place_nodes_in_diagram(self, str_node, dwh_of_nodes_dict, iri_node_str_dict, graph_dict, sp, depth):

        # print('str(type(str_node))',str(type(str_node)))
        # print('place_nodes_depth', depth)

        dwh_values_of_p = dwh_of_nodes_dict[str_node]

        depths = dwh_values_of_p[0]
        min_and_summation_width = dwh_values_of_p[1]
        min_width = min_and_summation_width[0]
        # summation_width = min_and_summation_width[1]
        # height = dwh_values_of_p[2]

        children = graph_dict[str_node]

        children_sp = sp
        children_width = 0

        for c in children:
            # recursive function
            if str(c) not in self.occupied_positions_xy.keys():
                width_of_last_node = self.place_nodes_in_diagram(str(c), dwh_of_nodes_dict, iri_node_str_dict,
                                                                 graph_dict, children_sp, depth + 1)
                children_sp = children_sp + width_of_last_node
                children_width = children_width + width_of_last_node

        width_to_set = max(min_width, children_width)

        if str_node not in self.occupied_positions_xy.keys():

            node_and_iri = iri_node_str_dict[str_node]
            node = node_and_iri[0]

            # compute where to place
            depth_to_set = max(depths)
            # new_x = sp * 300 + 300 * ((width / 2))
            new_x = 300.0 * (sp + (width_to_set / 2))
            new_y = depth_to_set * 200.0

            # place p
            snapToGrid = self.session.action('toggle_grid').isChecked()
            # node_and_iri = self.get_node_and_iri_from_commands(commands, str_node)

            if node is None:
                print('>>> place_nodes_in_diagram - node is None')
                LOGGER.critical('>>> place_nodes_in_diagram - node is None')
            node.setPos(snap(QtCore.QPoint(new_x, new_y), Diagram.GridSize, snapToGrid))

            self.occupied_positions_xy[str_node] = [new_x, new_y]

            return width_to_set
        else:
            return 0

    def place_nodes_in_diagram_3(self, unplaced_nodes, graph_nodes, graph_edges, iri_node_str_dict):

        # for gn in graph_nodes:
        # print('gn',gn)
        # print('     ',graph_nodes[gn])

        # construct
        graph_unplaced_nodes = self.construct_graph_of_unplaced_nodes(unplaced_nodes, graph_nodes)

        # for un in graph_unplaced_nodes:
        # print('un',un)
        # print('     ',graph_unplaced_nodes[un])

        sub_graphs_unplaced_nodes = self.seperate_graph_into_disconnected_subgaphs(graph_unplaced_nodes)

        anchors_for_unplaced_nodes = self.identify_anchors_for_unplaced_nodes(unplaced_nodes, graph_nodes)

        # for a in anchors_for_unplaced_nodes:
        # print('a',a)

        unplaced_nodes_for_graph = set()

        for i, sub_graph in enumerate(sub_graphs_unplaced_nodes):
            # print('subgraph-',i)
            unplaced_nodes_for_subgraph = self.place_subgraph_aided_by_anchors(sub_graph, anchors_for_unplaced_nodes,
                                                                               iri_node_str_dict)
            unplaced_nodes_for_graph = unplaced_nodes_for_graph.union(unplaced_nodes_for_subgraph)

        for un in unplaced_nodes_for_graph:
            if len(un) > 0:
                print('still_unplaced - ', un)

    # not used
    def get_optimized_point_for_list_of_points(self, list_of_points):

        return None

    def generate_points_around_role(self, num_pts, role_point, pts_type):

        generated_points = []

        if pts_type == 1:

            # generated_points.extend(self.generate_points_around_role(5, role_point, 'a'))
            # generated_points.extend(self.generate_points_around_role(5, role_point, 'c'))

            if num_pts <= 2:
                if num_pts == 1:
                    generated_points.extend(self.generate_points_around_role(1, role_point, 'x'))

                    generated_points.extend(self.generate_points_around_role(1, role_point, 'y'))
                else:
                    generated_points.extend(self.generate_points_around_role(1, role_point, 'x'))
                    generated_points.extend(self.generate_points_around_role(1, role_point, 'y'))

            elif num_pts > 2 and num_pts <= 10:

                if num_pts <= 5:  # 3-5

                    if num_pts == 3:

                        generated_points.extend(self.generate_points_around_role(1, role_point, 'x'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'y'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'p'))

                        generated_points.extend(self.generate_points_around_role(1, role_point, 'q'))

                    elif num_pts == 4:

                        generated_points.extend(self.generate_points_around_role(1, role_point, 'x'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'y'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'p'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'q'))

                    else:

                        generated_points.extend(self.generate_points_around_role(1, role_point, 'e'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'f'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 't'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'q'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'u'))

                else:  # 6-10

                    if num_pts == 6:

                        generated_points.extend(self.generate_points_around_role(1, role_point, 'p'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'q'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'r'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 's'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 't'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'u'))

                    elif num_pts == 10:

                        generated_points.extend(self.generate_points_around_role(5, role_point, 'a'))
                        generated_points.extend(self.generate_points_around_role(5, role_point, 'c'))

                    else:  # 7,8,9,10
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'x'))
                        generated_points.extend(self.generate_points_around_role(1, role_point, 'y'))

                        if num_pts == 7:

                            generated_points.extend(self.generate_points_around_role(1, role_point, 'e'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 'f'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 't'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 'q'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 'u'))

                        elif num_pts == 8:

                            generated_points.extend(self.generate_points_around_role(1, role_point, 'p'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 'q'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 'r'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 's'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 't'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 'u'))

                        elif num_pts == 9:
                            generated_points.extend(self.generate_points_around_role(5, role_point, 'c'))

                            generated_points.extend(self.generate_points_around_role(1, role_point, 'e'))
                            generated_points.extend(self.generate_points_around_role(1, role_point, 'f'))

            elif num_pts > 10 and num_pts <= 12:

                generated_points.extend(self.generate_points_around_role(5, role_point, 'a'))
                generated_points.extend(self.generate_points_around_role(5, role_point, 'c'))

                if num_pts == 11:
                    generated_points.extend(self.generate_points_around_role(1, role_point, 'x'))

                    generated_points.extend(self.generate_points_around_role(1, role_point, 'y'))
                else:
                    generated_points.extend(self.generate_points_around_role(1, role_point, 'x'))
                    generated_points.extend(self.generate_points_around_role(1, role_point, 'y'))

        elif pts_type == 2:

            # generated_points.extend(self.generate_points_around_role(5, role_point, 'a'))
            # generated_points.extend(self.generate_points_around_role(5, role_point, 'b'))
            # generated_points.extend(self.generate_points_around_role(5, role_point, 'c'))


            if num_pts <= 5:

                generated_points.extend(self.generate_points_around_role(num_pts, role_point, 'a'))

            elif num_pts > 5 and num_pts <= 10:

                generated_points.extend(self.generate_points_around_role(5, role_point, 'a'))
                generated_points.extend(self.generate_points_around_role(num_pts - 5, role_point, 'c'))

            elif num_pts > 10 and num_pts <= 15:

                generated_points.extend(self.generate_points_around_role(5, role_point, 'a'))
                generated_points.extend(self.generate_points_around_role(5, role_point, 'c'))
                generated_points.extend(self.generate_points_around_role(num_pts - 10, role_point, 'b'))

        elif pts_type == 'a':

            st_X = role_point[0] - 80
            st_Y = role_point[1] - 50

            while num_pts != 0:
                new_pt = [st_X, st_Y]
                generated_points.append(new_pt)
                st_X = st_X + 40
                num_pts = num_pts - 1

        elif pts_type == 'b':

            st_X = role_point[0] - 80
            st_Y = role_point[1]

            while num_pts != 0:
                new_pt = [st_X, st_Y]
                generated_points.append(new_pt)
                st_X = st_X + 40
                num_pts = num_pts - 1

        elif pts_type == 'c':

            st_X = role_point[0] - 80
            st_Y = role_point[1] + 50

            while num_pts != 0:
                new_pt = [st_X, st_Y]
                generated_points.append(new_pt)
                st_X = st_X + 40
                num_pts = num_pts - 1

        elif pts_type in ['x', 'y', 'e', 'f', 'p', 'q', 'r', 's', 't', 'u']:
            if pts_type == 'x':

                st_X = role_point[0] - 80
                st_Y = role_point[1]

            elif pts_type == 'y':

                st_X = role_point[0] + 80
                st_Y = role_point[1]

            elif pts_type == 'e':

                st_X = role_point[0] - 40
                st_Y = role_point[1] - 50

            elif pts_type == 'f':

                st_X = role_point[0] + 40
                st_Y = role_point[1] - 50

            elif pts_type == 'p':

                st_X = role_point[0]
                st_Y = role_point[1] - 50

            elif pts_type == 'q':

                st_X = role_point[0]
                st_Y = role_point[1] + 50

            elif pts_type == 'r':

                st_X = role_point[0] - 80
                st_Y = role_point[1] - 50

            elif pts_type == 's':

                st_X = role_point[0] + 80
                st_Y = role_point[1] - 50

            elif pts_type == 't':

                st_X = role_point[0] - 80
                st_Y = role_point[1] + 50

            elif pts_type == 'u':

                st_X = role_point[0] + 80
                st_Y = role_point[1] + 50

            new_pt = [st_X, st_Y]
            generated_points.append(new_pt)

        else:
            pass

        return generated_points

    def generate_points_around_connector(self, num_pts, connector_point):

        points_to_return = []

        dist = 1
        X_cpt = int(connector_point[0])
        Y_cpt = int(connector_point[1])

        while len(points_to_return) < num_pts:

            X_left = int(X_cpt - (dist * 150))
            X_right = int(X_cpt + (dist * 150))
            Y_bottom = int(Y_cpt + (dist * 100))

            for y in range(Y_cpt, Y_bottom + 1, 100):
                for x in range(X_cpt, X_right+1, 150):
                    if [x, y] not in self.occupied_positions_xy.values():
                        points_to_return.append([x, y])
                for x in range(X_left, X_cpt + 1, 150):
                    if [x, y] not in self.occupied_positions_xy.values():
                        points_to_return.append([x, y])

            dist = dist + 1

        return points_to_return

    def place_attributes_and_related_nodes(self):

        list_of_nodes_to_place = []

        for str_class_expression in self.attributes_for_classexpressions.keys():

            if str_class_expression not in self.occupied_positions_xy.keys():
                continue

            pos_str_class_expression = self.occupied_positions_xy[str_class_expression]
            posX_str_CE = pos_str_class_expression[0]
            posY_str_CE = pos_str_class_expression[1]

            # print(str_class_expression,' - pos_str_class_expression',pos_str_class_expression)

            attributes = self.attributes_for_classexpressions[str_class_expression]

            all_half_lines = []

            for i, atr in enumerate(attributes):

                # print('atr',atr)

                if i != len(attributes) - 1:

                    DomainANDRange_nodes = self.AttributesANDRoles_to_DomainANDRange_mapping[str(atr)]

                    # print('DomainANDRange_nodes', DomainANDRange_nodes)

                    for dmORrg in DomainANDRange_nodes:

                        classexpressionORvaluedomain_nodes = self.AttributesANDRoles_to_DomainANDRange_mapping[
                            str(dmORrg)]

                        # print('classexpressionORvaluedomain_nodes', classexpressionORvaluedomain_nodes)

                        flag = False
                        flag_2 = False
                        for ceORvd in classexpressionORvaluedomain_nodes:
                            if 'ValueDomainNode:' in str(ceORvd):
                                half_line = [atr, dmORrg, ceORvd]
                                all_half_lines.append(half_line)
                                flag_2 = True
                            else:
                                flag = True
                        if flag and not flag_2:
                            all_half_lines.append([atr, dmORrg])

            # for i, hl in enumerate(all_half_lines):
            #    print(i,'-',hl)

            groups = []
            group = []
            line = []

            if len(all_half_lines) == 1:
                line.extend(all_half_lines[0])
                group.append(line.copy())
                line.clear()
                groups.append(group.copy())
                group.clear()
            else:
                for i, hl in enumerate(all_half_lines):
                    # print(i,'-hl',hl)
                    if (i % 2 == 1) or (i == len(all_half_lines) - 1):
                        if (len(line) > 0) and (str(line[len(line) - 1]) == str(hl[0])):
                            line.extend(hl[1:len(hl)])
                        else:
                            line.extend(hl)

                        group.append(line.copy())
                        line.clear()
                        if (len(group) == 4) or (i == len(all_half_lines) - 1):
                            groups.append(group.copy())
                            group.clear()
                    else:
                        hl.reverse()
                        line.extend(hl)

                        # print('line',line)
                        # print('group',group)
                        # print('groups',groups)

            # for i,gr in enumerate(groups):
            #    print(i,'.gr',gr)
            #    for l in gr:
            #        print('     l-',l)

            for gr_no, group_lr in enumerate(groups):

                if gr_no % 2 == 0:
                    mul = 1.0
                else:
                    mul = -1.0

                attributed_placed = set()

                for ln, line_lr in enumerate(group_lr):
                    # print('     line_lr-',line_lr)
                    atr = None
                    value_domains = []
                    domainORrange_nodes = []
                    for j, e in enumerate(line_lr):
                        if 'AttributeNode:' in str(e):
                            atr = e
                        elif (('DomainRestrictionNode:' in str(e)) or ('RangeRestrictionNode:' in str(e))):
                            domainORrange_nodes.append(e)
                        elif 'ValueDomainNode:' in str(e):
                            value_domains.append(e)
                        else:
                            LOGGER.error('case not considered, contact programmer /3345')

                        X_atr = posX_str_CE + (mul * (gr_no + 1.0) * 300.0)
                        Y_atr = posY_str_CE - 100.0 + (ln * 60.0)
                        if atr is not None:
                            # print('mul',mul,',gr_no',gr_no,',ln',ln)
                            # print([atr, X_atr, Y_atr])
                            if str(atr) not in attributed_placed:
                                list_of_nodes_to_place.append([atr, X_atr, Y_atr])
                                attributed_placed.add(str(atr))

                    if len(domainORrange_nodes) >= 1:

                        dmORrg_nd1 = domainORrange_nodes[0]
                        X_dmORrg_nd1 = X_atr + (-1.0) * (10.0 + 20.0 + 10.0)
                        Y_dmORrg_nd1 = Y_atr + 20.0
                        # print([dmORrg_nd1, X_dmORrg_nd1, Y_dmORrg_nd1])
                        list_of_nodes_to_place.append([dmORrg_nd1, X_dmORrg_nd1, Y_dmORrg_nd1])

                        if len(domainORrange_nodes) == 2:
                            dmORrg_nd2 = domainORrange_nodes[1]
                            X_dmORrg_nd2 = X_atr + (+1.0) * (10.0 + 20.0 + 10.0)
                            Y_dmORrg_nd2 = Y_atr + 20.0
                            # print([dmORrg_nd2, X_dmORrg_nd2, Y_dmORrg_nd2])
                            list_of_nodes_to_place.append([dmORrg_nd2, X_dmORrg_nd2, Y_dmORrg_nd2])
                        else:
                            X_dmORrg_nd2 = None
                            Y_dmORrg_nd2 = None

                    if len(value_domains) >= 1:

                        vd_nd1 = value_domains[0]
                        X_vd_nd1 = X_dmORrg_nd1 + (-1.0) * (10.0 + 20.0 + 77.0)
                        Y_vd_nd1 = Y_dmORrg_nd1 - 10.0
                        # print([vd_nd1, X_vd_nd1, Y_vd_nd1])
                        list_of_nodes_to_place.append([vd_nd1, X_vd_nd1, Y_vd_nd1])

                        if len(value_domains) == 2:
                            vd_nd2 = value_domains[1]
                            X_vd_nd2 = X_dmORrg_nd2 + (+1.0) * (10.0 + 20.0 + 77.0)
                            Y_vd_nd2 = Y_dmORrg_nd2 - 10.0
                            # print([vd_nd2, X_vd_nd2, Y_vd_nd2])
                            list_of_nodes_to_place.append([vd_nd2, X_vd_nd2, Y_vd_nd2])
                        else:
                            X_vd_nd2 = None
                            Y_vd_nd2 = None
                    else:
                        X_vd_nd1 = None
                        Y_vd_nd1 = None
                        X_vd_nd2 = None
                        Y_vd_nd2 = None

            X_eles = [X_atr]
            if X_dmORrg_nd1 is not None:
                X_eles.append(X_dmORrg_nd1)
            if X_dmORrg_nd2 is not None:
                X_eles.append(X_dmORrg_nd2)
            if X_vd_nd1 is not None:
                X_eles.append(X_vd_nd1)
            if X_vd_nd2 is not None:
                X_eles.append(X_vd_nd2)

            Y_eles = [Y_atr]
            if Y_dmORrg_nd1 is not None:
                Y_eles.append(Y_dmORrg_nd1)
            if Y_dmORrg_nd2 is not None:
                Y_eles.append(Y_dmORrg_nd2)
            if Y_vd_nd1 is not None:
                Y_eles.append(Y_vd_nd1)
            if Y_vd_nd2 is not None:
                Y_eles.append(Y_vd_nd2)

            max_X = max(X_eles) + 150
            max_Y = max(Y_eles) + 75
            min_X = min(X_eles) + 150
            min_Y = min(Y_eles) - 75

            while min_X % 150.0 != 0:
                min_X = min_X - 1.0
            while max_X % 150.0 != 0:
                max_X = max_X + 1.0
            while min_Y % 200.0 != 0:
                min_Y = min_Y - 1.0
            while max_Y % 200.0 != 0:
                max_Y = max_Y + 1.0
            # print([max_X, max_Y, min_X, min_Y])

            self.occupied_rectangles.append([max_X, max_Y, min_X, min_Y])

            # print('len(list_of_nodes_to_place)',len(list_of_nodes_to_place))

        for i, node_XY in enumerate(list_of_nodes_to_place):
            node = node_XY[0]
            new_x = node_XY[1]
            new_y = node_XY[2]

            # print(i,'     node_XY',node_XY)

            snapToGrid = self.session.action('toggle_grid').isChecked()
            # node_and_iri = self.get_node_and_iri_from_commands(commands, str_node)

            if node is None:
                print('>>> place_nodes_in_diagram - node is None')
                LOGGER.critical('>>> place_nodes_in_diagram - node is None')
            node.setPos(snap(QtCore.QPoint(new_x, new_y), Diagram.GridSize, snapToGrid))
            self.occupied_positions_xy[str(node)] = [new_x, new_y]

    def place_roles_and_related_nodes(self, graph_dict_nodes, iri_node_str_dict):

        anchors_for_roles = dict()

        for str_node in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
            if 'RoleNode:' in str_node:

                classexpression_anchors_raw = set()
                # classexpression_anchors_points = []

                white_and_black_square_nodes = self.AttributesANDRoles_to_DomainANDRange_mapping[str_node]

                for wORb_node in white_and_black_square_nodes:
                    class_expression_nodes = self.AttributesANDRoles_to_DomainANDRange_mapping[str(wORb_node)]
                    classexpression_anchors_raw = classexpression_anchors_raw.union(set(class_expression_nodes))

                classexpression_anchors = []

                for ce in classexpression_anchors_raw:
                    if str(ce) in self.occupied_positions_xy.keys():
                        classexpression_anchors.append(str(ce))

                anchors_for_roles[str_node] = classexpression_anchors

                # for ce in classexpression_anchors:
                # points_ce_XY = self.occupied_positions_xy[str(ce)]
                # classexpression_anchors_points.append(points_ce_XY)

                # optimized_point = self.get_optimized_point_for_list_of_points(classexpression_anchors_points)

        for str_role in anchors_for_roles.keys():
            self.place_unplaced_node_aided_by_anchors(str_role, [], anchors_for_roles, iri_node_str_dict,
                                                      place_role=True)

        # for str_role in anchors_for_roles.keys():
        #    if str_role not in self.occupied_positions_xy.keys():
        #        children = graph_dict_nodes[str_role]
        #        self.place_unplaced_node_aided_by_anchors(str_role, children, anchors_for_roles, iri_node_str_dict, place_role=True)


        for str_node in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
            if ('RoleNode:' in str_node) and (str_node in self.occupied_positions_xy.keys()):

                role_pos = self.occupied_positions_xy[str_node]
                white_and_black_square_nodes = self.AttributesANDRoles_to_DomainANDRange_mapping[str_node]

                free_points = []

                if len(white_and_black_square_nodes) <= 12:
                    free_points.extend(self.generate_points_around_role(len(white_and_black_square_nodes), role_pos, 1))
                else:
                    free_positions_ROLES = [role_pos]

                    rx = role_pos[0]
                    ry = role_pos[1]
                    generated_boxes = [[rx + 300, ry], [rx - 300, ry], [rx, ry + 200], [rx, ry - 200],
                                       [rx + 300, ry - 200], [rx - 300, ry - 200], [rx - 300, ry + 200],
                                       [rx + 300, ry + 200]]
                    generated_boxes.extend(
                        [[rx + 600, ry], [rx - 600, ry], [rx, ry + 400], [rx, ry - 400], [rx + 300, ry - 400],
                         [rx + 600, ry - 200], [rx - 300, ry - 400], [rx - 600, ry - 200]])
                    generated_boxes.extend(
                        [[rx - 300, ry + 400], [rx - 600, ry + 200], [rx + 600, ry + 200], [rx + 300, ry + 400],
                         [rx + 600, ry - 400], [rx - 600, ry + 400], [rx - 600, ry + 400], [rx + 600, ry + 400]])

                    for gb_center_pt in generated_boxes:
                        if (
                            gb_center_pt not in self.occupied_positions_xy.values()) and self.check_if_point_in_occupied_rectangles(
                                gb_center_pt):
                            free_positions_ROLES.append(gb_center_pt)

                    if len(white_and_black_square_nodes) >= 13 and len(white_and_black_square_nodes) <= 25:

                        free_points.extend(self.generate_points_around_role(10, role_pos, 1))
                        difference = len(white_and_black_square_nodes) - 10
                        free_points.extend(self.generate_points_around_role(difference, free_positions_ROLES[0], 2))

                    elif len(white_and_black_square_nodes) >= 26 and len(white_and_black_square_nodes) <= 40:

                        free_points.extend(self.generate_points_around_role(10, role_pos, 1))
                        free_points.extend(self.generate_points_around_role(15, free_positions_ROLES[0], 2))
                        difference = len(white_and_black_square_nodes) - 25
                        if len(free_positions_ROLES) >= 2:
                            free_points.extend(self.generate_points_around_role(difference, free_positions_ROLES[1], 2))

                    elif len(white_and_black_square_nodes) >= 41 and len(white_and_black_square_nodes) <= 55:

                        free_points.extend(self.generate_points_around_role(10, role_pos, 1))
                        free_points.extend(self.generate_points_around_role(15, free_positions_ROLES[0], 2))
                        if len(free_positions_ROLES) >= 2:
                            free_points.extend(self.generate_points_around_role(15, free_positions_ROLES[1], 2))
                            difference = len(white_and_black_square_nodes) - 40
                            if len(free_positions_ROLES) >= 3:
                                free_points.extend(
                                    self.generate_points_around_role(difference, free_positions_ROLES[2], 2))

                    else:
                        free_points.extend(self.generate_points_around_role(10, role_pos, 1))
                        free_points.extend(self.generate_points_around_role(15, free_positions_ROLES[0], 2))
                        if len(free_positions_ROLES) >= 2:
                            free_points.extend(self.generate_points_around_role(15, free_positions_ROLES[1], 2))
                            if len(free_positions_ROLES) >= 3:
                                free_points.extend(
                                    self.generate_points_around_role(difference, free_positions_ROLES[2], 2))
                                difference = len(white_and_black_square_nodes) - 55
                                if len(free_positions_ROLES) >= 4:
                                    free_points.extend(
                                        self.generate_points_around_role(difference, free_positions_ROLES[3], 2))

                        if difference > 15:
                            print(difference, ' white AND/OR black squares of roles not places perfectly')

                # print('len(white_and_black_square_nodes)',len(white_and_black_square_nodes))
                # print('len(free_points)',len(free_points))

                while (len(free_points) < len(white_and_black_square_nodes)):
                    check_pos = [role_pos[0] + 300, role_pos[1]]
                    while check_pos in self.occupied_positions_xy.values():
                        check_pos[0] = check_pos[0] + 300
                    free_points.extend(self.generate_points_around_role(15, check_pos, 2))

                for whiteORblack_node in white_and_black_square_nodes:

                    CE_anchors = self.AttributesANDRoles_to_DomainANDRange_mapping[str(whiteORblack_node)]

                    sum_dist = []

                    # print('free_points',free_points)

                    for fp in free_points:

                        sum_X = 0
                        sum_Y = 0

                        for CE_anch in CE_anchors:

                            if str(CE_anch) in self.occupied_positions_xy.keys():
                                XY_CE_anch = self.occupied_positions_xy[str(CE_anch)]
                                X_CE_anch = XY_CE_anch[0]
                                Y_CE_anch = XY_CE_anch[1]
                            else:
                                continue

                            X_dist = abs(fp[0] - X_CE_anch)
                            Y_dist = abs(fp[1] - Y_CE_anch)

                            sum_X = sum_X + X_dist
                            sum_Y = sum_Y + Y_dist

                        sum_dist.append(sum_X + sum_Y)

                    min = max(sum_dist)
                    for s in sum_dist:
                        if (s < min) and (s != 0):
                            min = s

                    s_ind = sum_dist.index(min)
                    fp_to_select = free_points[s_ind]

                    snapToGrid = self.session.action('toggle_grid').isChecked()

                    if whiteORblack_node is None:
                        print('>>> place_nodes_in_diagram - node is None')
                        LOGGER.critical('>>> place_nodes_in_diagram - node is None')
                    whiteORblack_node.setPos(
                        snap(QtCore.QPoint(fp_to_select[0], fp_to_select[1]), Diagram.GridSize, snapToGrid))

                    self.occupied_positions_xy[str(whiteORblack_node)] = fp_to_select

                    free_points.remove(fp_to_select)

    def get_total_number_of_slots_of_attributes_for_classexpressions(self, iri_node_str_dict):

        for str_node in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
            if 'AttributeNode:' in str_node:
                domainORrange_nodes = self.AttributesANDRoles_to_DomainANDRange_mapping[str_node]
                for dmORrg in domainORrange_nodes:
                    classexpressions_OR_valuedomain = self.AttributesANDRoles_to_DomainANDRange_mapping[str(dmORrg)]
                    for ceOrvd in classexpressions_OR_valuedomain:
                        if 'ValueDomainNode:' not in str(ceOrvd):
                            if str(ceOrvd) not in self.attributes_for_classexpressions.keys():
                                self.attributes_for_classexpressions[str(ceOrvd)] = set()
                            self.attributes_for_classexpressions[str(ceOrvd)].add(iri_node_str_dict[str_node][0])

        # 2 dmORrg nodes can fit in 1 line
        for str_ce in self.attributes_for_classexpressions.keys():

            summation_dmORrg_nodes = 0
            # print('str_ce',str_ce)
            attributes = self.attributes_for_classexpressions[str_ce]
            for atr in attributes:
                children = self.AttributesANDRoles_to_DomainANDRange_mapping[str(atr)]
                # print('     atr',atr)
                # print('         children', children)
                summation_dmORrg_nodes = summation_dmORrg_nodes + len(children)

            temp = list(attributes)
            temp.append(summation_dmORrg_nodes)
            self.attributes_for_classexpressions[str_ce] = temp

    def place_edges_in_diagram(self, graph_dict_edges):

        commands_to_return = []

        EDGE_diagram_edge_pair = []
        DIAGRAM_diagram_edge_pair = []

        all_diagrams = set()
        break_points = []
        bp_edges = []
        bp_indices = []

        for edge_type in graph_dict_edges.keys():
            pairs = graph_dict_edges[edge_type]
            for p in pairs:
                child = p[0]
                parent = p[1]

                str_child_parent = str(child) + str(parent)

                # InclusionEdge = 65554
                # InputEdge = 65556
                cdr = 'DomainRestrictionNode:' in str_child_parent or 'RangeRestrictionNode:' in str_child_parent
                ca = 'AttributeNode:' in str(child) and 'AttributeNode:' in str(parent)
                cpi = ('ComplementNode:' in str(parent) or 'InverseNode:' in str(parent)) and (edge_type == 65556)

                flag = ca or (not cpi and not cdr)

                if flag is False:
                    break_point = None
                else:
                    # print('str_child_parent', str_child_parent)
                    # print(cdr, ca, cpi, '-', flag)
                    if (edge_type == 65554) or (edge_type == 65556):
                        child_pos = self.occupied_positions_xy[str(child)]
                        parent_pos = self.occupied_positions_xy[str(parent)]
                        if (child_pos[0] == parent_pos[0]) or (child_pos[1] == parent_pos[1]):
                            break_point = None
                        else:
                            if parent_pos[1] < child_pos[1]:  # if parent is above the child
                                if edge_type == 65554:
                                    break_point = QtCore.QPointF(child_pos[0], child_pos[1] - 100.0)
                                else:
                                    break_point = QtCore.QPointF(child_pos[0], parent_pos[1] + 100.0)
                            elif parent_pos[1] > child_pos[1]:  # if parent is below the child
                                if edge_type == 65554:
                                    break_point = QtCore.QPointF(child_pos[0], child_pos[1] + 150.0)
                                else:
                                    break_point = QtCore.QPointF(parent_pos[0], child_pos[1] + 150.0)
                            else:
                                break_point = None
                    else:
                        break_point = None

                edge = self.diagram_to_place.factory.create(Item(edge_type), source=child)

                child.updateNode(selected=False)
                currentNode = parent
                insertEdge = False

                if currentNode:
                    currentNode.updateNode(selected=False)
                    pvr = self.diagram_to_place.project.profile.checkEdge(child, edge, parent)
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
                        self.diagram_to_place.removeItem(edge)

                if insertEdge:
                    # commands_to_return.append(CommandEdgeAdd(self.diagram_to_place, edge))
                    DIAGRAM_diagram_edge_pair.append(self.diagram_to_place)
                    EDGE_diagram_edge_pair.append(edge)

                    if break_point is not None:
                        # print(edge_type,',break_point-',break_point, ' child_pos-',child_pos,' parent_pos-',parent_pos)
                        all_diagrams.add(self.diagram_to_place)
                        break_points.append(break_point)
                        bp_edges.append(edge)
                        bp_indices.append(0)
                else:
                    # print('         insert edge is false',edge)
                    pass

        print('len(DIAGRAM_diagram_edge_pair)', len(DIAGRAM_diagram_edge_pair))
        print('len(EDGE_diagram_edge_pair)', len(EDGE_diagram_edge_pair))

        if len(EDGE_diagram_edge_pair) > 0:
            commands_to_return.append(CommandEdgesAdd(DIAGRAM_diagram_edge_pair, EDGE_diagram_edge_pair))
        if len(break_points) > 0:
            commands_to_return.append(
                CommandEdgesBreakpointsAdd(list(all_diagrams), bp_edges, bp_indices, break_points))

        return [commands_to_return, EDGE_diagram_edge_pair]

    def place_sub_graph_in_a_diagram(self, graph_dict_nodes, iri_node_str_dict, sp_inp):

        # all root nodes are of type str
        root_nodes = self.get_root_nodes_of_graph(graph_dict_nodes)
        dwh_of_nodes_dict = dict()
        current_depth = 0

        # print('len(root_nodes)',len(root_nodes))
        # for r in list(root_nodes):
        #    print('r',r)

        stack_trace = []

        for r in list(root_nodes):
            # print('     r', r)

            children = graph_dict_nodes[r]

            # for c in children:
            #    print('         c',c)

            stack_trace.append(r)

            self.get_dept_width_height_and_repeat_for_children(graph_dict_nodes, dwh_of_nodes_dict, r, current_depth,
                                                               stack_trace)

        max_depth = 0
        for key in dwh_of_nodes_dict.keys():
            depths = dwh_of_nodes_dict[key][0]
            max_depth = max(max_depth, max(depths))

        widths_of_placed_nodes_dict = dict()
        for i in range(0, max_depth + 1):
            widths_of_placed_nodes_dict[i] = []

        depth = 0

        sp = sp_inp
        for r in list(root_nodes):
            # print('r-',r)
            # print('[r][0]-', iri_node_str_dict[r][0])
            # print('[r][1]-', iri_node_str_dict[r][1].toString())
            width_of_last_node = self.place_nodes_in_diagram(r, dwh_of_nodes_dict, iri_node_str_dict, graph_dict_nodes,
                                                             sp, depth)
            sp = sp + width_of_last_node

        """
        summation_width = 0
        max_height = 0
        for r in list(root_nodes):
            dwh_vals = dwh_of_nodes_dict[r]
            width = max(dwh_vals[1])
            height = dwh_vals[2]
            summation_width = summation_width + width
            max_height = max(max_height, height)

        return [summation_width, max_height]
        """

    def get_refined_graph(self, graph_dict_nodes, graph_dict_edges):

        refined_dict = dict()

        for str_node in graph_dict_nodes.keys():
            sub_str = str_node[0:str_node.index(':') + 1]
            # print('sub_str',sub_str)
            if sub_str not in self.exclusion_list:
                refined_dict[str_node] = []
                values = graph_dict_nodes[str_node]
                for v in values:
                    # print('     str(v)',str(v))
                    sub_str_2 = str(v)[0:str(v).index(':') + 1]
                    if sub_str_2 not in self.exclusion_list:
                        refined_dict[str_node].append(v)

        # print('len(refined_dict.keys)',len(refined_dict.keys()))

        refined_dict_2 = refined_dict.copy()

        flag = True

        while (flag):

            delete_list = []

            for str_node in refined_dict_2.keys():
                sub_str = str_node[0:str_node.index(':') + 1]
                children = refined_dict_2[str_node]
                # print('str_node',str_node)
                if sub_str in ['UnionNode:', 'IntersectionNode:']:
                    # print('U|I',len(children))
                    if len(children) <= 1:
                        delete_list.append(str_node)
                if sub_str == 'ComplementNode:':
                    # print('C', len(children))
                    if len(children) == 0:
                        delete_list.append(str_node)

            # print('delete_list',delete_list)

            if len(delete_list) > 0:

                # delete the keys
                for d in delete_list:
                    del refined_dict_2[d]

                # delete values
                for str_node in refined_dict_2.keys():
                    children = refined_dict_2[str_node]
                    new_children = []
                    new_children.extend(children)
                    for d in delete_list:
                        for nc in new_children:
                            if d == str(nc):
                                new_children.remove(nc)
                    refined_dict_2[str_node] = new_children
            else:
                flag = False

        # print('len(refined_dict_2.keys)', len(refined_dict_2.keys()))

        # return refined_dict_2

        refined_dict_3 = refined_dict_2.copy()
        root_nodes_A = self.get_root_nodes_of_graph(graph_dict_nodes)

        flag_2 = True

        while (flag_2):

            delete_list_2 = []
            root_nodes_B = self.get_root_nodes_of_graph(refined_dict_3)

            for str_node in root_nodes_B:
                if str_node not in root_nodes_A:
                    # if there is a concept without children, remove it
                    # if there is an OR/AND node without an incoming edge, remove it
                    # basically, if the node has no incoming input edge, remove it
                    inclusion_edge_pairs = graph_dict_edges[Item.InclusionEdge.value]
                    flag = True
                    for pair in inclusion_edge_pairs:
                        # sub_node = pair[0]
                        super_node = pair[1]
                        if str(super_node) == str_node:
                            flag = False
                            break
                    if flag is True:
                        equivalent_edge_pairs = graph_dict_edges[Item.EquivalenceEdge.value]
                        for pair in equivalent_edge_pairs:
                            node1 = pair[0]
                            node2 = pair[1]
                            if (str(node1) == str_node) or (str(node2) == str_node):
                                flag = False
                                break
                    if flag:
                        delete_list_2.append(str_node)
                sub_str = str_node[0:str_node.index(':') + 1]
                # print('      ', str_node)
                if sub_str == 'ComplementNode:':
                    # print('--**--',str_node)
                    delete_list_2.append(str_node)

            # print('delete_list_2',delete_list_2)

            if len(delete_list_2) > 0:
                for d in delete_list_2:
                    if d in refined_dict_3.keys():
                        del refined_dict_3[d]
            else:
                flag_2 = False

        # print('len(refined_dict_3.keys)', len(refined_dict_3.keys()))

        return refined_dict_3

    def place_graph_in_a_diagram(self, graph_dict_nodes, graph_dict_nodes_inds, graph_dict_edges, iri_node_str_dict):

        filtered_graph_dict_nodes = self.get_refined_graph(graph_dict_nodes, graph_dict_edges)

        """
        print('---   graph_dict_nodes')
        for k in graph_dict_nodes.keys():
            print('k', k)
            values = graph_dict_nodes[k]
            for v in values:
                print('     v', str(v))
        print('---   graph_dict_nodess END')

        print('---   filtered_graph_dict_nodes')
        for k in filtered_graph_dict_nodes.keys():
            print('k',k)
            values = filtered_graph_dict_nodes[k]
            for v in values:
                print('     v',str(v))
        print('---   filtered_graph_dict_nodes END')
        """

        # print('placing filtered classes')
        self.place_sub_graph_in_a_diagram(filtered_graph_dict_nodes, iri_node_str_dict, 0)
        # print('placing filtered classes done')

        all_nodes = set(iri_node_str_dict.keys())

        # place remaining nodes in the diagram
        # print('len(unplaced_nodes)_0',len(unplaced_nodes_0))
        # print('len(unplaced_nodes)', len(unplaced_nodes))
        # for up in unplaced_nodes_0:
        # print('up0-',up)
        # for up in unplaced_nodes:
        # print('up-',up)

        # for key in self.occupied_positions_xy.keys():
        # print(key,'-',self.occupied_positions_xy[key])
        # for val in self.occupied_positions_xy.values():
        # print('val',val)

        self.place_attributes_and_related_nodes()
        self.place_roles_and_related_nodes(graph_dict_nodes, iri_node_str_dict)

        placed_nodes = set(self.occupied_positions_xy.keys())
        unplaced_nodes = all_nodes.difference(placed_nodes)


        self.place_nodes_in_diagram_3(unplaced_nodes, graph_dict_nodes, graph_dict_edges, iri_node_str_dict)



        self.place_individual_nodes_in_diagram_2(graph_dict_nodes_inds, iri_node_str_dict)

        edge_commands_and_edges_to_update = self.place_edges_in_diagram(graph_dict_edges)

        #for un in unplaced_nodes:
        #    print('un-',un)
        # summation_width_2 = values_2[0]
        # max_height_2 = values_2[1]
        # summation_width_3 = values_3[0]
        # max_height_3 = values_3[1]

        # summation_width = summation_width_1 + summation_width_2 + summation_width_3
        # max_height = max(max_height_1,max_height_2,max_height_3)

        max_X = list(self.occupied_positions_xy.values())[0][0]
        max_Y = list(self.occupied_positions_xy.values())[0][1]
        min_X = 0
        min_Y = 0

        for pos in self.occupied_positions_xy.values():

            if pos[0] > max_X:
                max_X = pos[0]
            else:
                if pos[0] < min_X:
                    min_X = pos[0]

            if pos[1] > max_Y:
                max_Y = pos[1]
            else:
                if pos[1] < min_Y:
                    min_Y = pos[1]

        rectangle_to_set_for_diagram = QtCore.QRectF(QtCore.QPointF (min_X-250.0, min_Y-250.0), QtCore.QPointF(max_X+250.0, max_Y+250.0))
        diagram_resize_command = CommandDiagramResize(self.diagram_to_place, rectangle_to_set_for_diagram)

        return [diagram_resize_command, edge_commands_and_edges_to_update[0], edge_commands_and_edges_to_update[1]]

    def Process_metadata_for_roles_and_attributes(self, axioms, iri_node_str_dict):

        itr = axioms.iterator()
        meta_dict_node = dict()

        while itr.hasNext():
            axiom = itr.next()
            property = axiom.getProperty()
            axiom_type_str = axiom.getAxiomType().toString()

            #print('axiom_type_str',axiom_type_str)

            if axiom_type_str == self.AxiomType.FUNCTIONAL_DATA_PROPERTY.toString():
                casted_property_exp = cast(self.OWLDataPropertyExpression, property)
                iri = casted_property_exp.asOWLDataProperty().getIRI()
                node = self.get_node_from_iri(iri, iri_node_str_dict)
            else:
                casted_property_exp = cast(self.OWLObjectPropertyExpression, property)
                iri = casted_property_exp.asOWLObjectProperty().getIRI()
                node = self.get_node_from_iri(iri, iri_node_str_dict)

            if str(node) not in meta_dict_node.keys():
                meta_dict_node[str(node)] = [set(), node]

            #undo = self.project.meta(node.type(), node.text())
            #redo = undo.copy()
            #redo = dict()


            if axiom_type_str == self.AxiomType.FUNCTIONAL_DATA_PROPERTY.toString():
                #redo[K_FUNCTIONAL] = True
                meta_dict_node[str(node)][0].add(K_FUNCTIONAL)
            else:
                if axiom_type_str == self.AxiomType.SYMMETRIC_OBJECT_PROPERTY.toString():
                    #redo[K_SYMMETRIC] = True
                    meta_dict_node[str(node)][0].add(K_SYMMETRIC)
                elif axiom_type_str == self.AxiomType.ASYMMETRIC_OBJECT_PROPERTY.toString():
                    #redo[K_ASYMMETRIC] = True
                    meta_dict_node[str(node)][0].add(K_ASYMMETRIC)
                elif axiom_type_str == self.AxiomType.TRANSITIVE_OBJECT_PROPERTY.toString():
                    #redo[K_TRANSITIVE] = True
                    meta_dict_node[str(node)][0].add(K_TRANSITIVE)
                elif axiom_type_str == self.AxiomType.REFLEXIVE_OBJECT_PROPERTY.toString():
                    #redo[K_REFLEXIVE] = True
                    meta_dict_node[str(node)][0].add(K_REFLEXIVE)
                elif axiom_type_str == self.AxiomType.IRREFLEXIVE_OBJECT_PROPERTY.toString():
                    #redo[K_IRREFLEXIVE] = True
                    meta_dict_node[str(node)][0].add(K_IRREFLEXIVE)
                elif axiom_type_str == self.AxiomType.FUNCTIONAL_OBJECT_PROPERTY.toString():
                    #redo[K_FUNCTIONAL] = True
                    meta_dict_node[str(node)][0].add(K_FUNCTIONAL)
                elif axiom_type_str == self.AxiomType.INVERSE_FUNCTIONAL_OBJECT_PROPERTY.toString():
                    #redo[K_INVERSE_FUNCTIONAL] = True
                    meta_dict_node[str(node)][0].add(K_INVERSE_FUNCTIONAL)

        temp_lists = []

            #if undo!=redo:
        for str_node in meta_dict_node.keys():
            values = meta_dict_node[str_node]
            meta_data = values[0]
            temp_dict = dict()
            for md in meta_data:
                temp_dict[md] = True

            node = values[1]

            temp_lst = [node.type(), node.text(), dict(), temp_dict]
            temp_lists.append(temp_lst)

            #commands_to_return.append(CommandNodeSetMeta(self.project, node.type(), node.text(), dict(), temp_dict))

        return CommandNodesSetMeta(self.project, temp_lists)

    def generate_and_push_commands_to_the_session(self, iri_node_str_dict, other_nodes_to_add, diagram_resize_command,\
                                                  edge_commands, edges_to_update, AllMetadataAxioms_JAVAset):

        t_start = time.clock()

        # print('t_start',t_start)

        print('>>> generate_and_push_commands_to_the_session')

        nodes_in_dict = iri_node_str_dict.keys()

        Duplicate_dict_1 = self.project.copy_IRI_prefixes_nodes_dictionaries(self.project.IRI_prefixes_nodes_dict,
                                                                             dict())
        Duplicate_dict_2 = self.project.copy_IRI_prefixes_nodes_dictionaries(self.project.IRI_prefixes_nodes_dict,
                                                                             dict())

        commands_session = []

        all_iris = set()

        all_nodes_to_add = []
        all_nodes_to_add.extend(nodes_in_dict)
        all_nodes_to_add.extend(list(other_nodes_to_add))

        nodes_to_add = []

        # for i,node_str in enumerate(all_nodes_to_add):
        for node_str in iri_node_str_dict.keys():
            node_and_iri = iri_node_str_dict[node_str]

            node = node_and_iri[0]
            nodes_to_add.append(node)

            full_iri = node_and_iri[1]

            if full_iri.toString() != '':
                res = self.project.get_iri_and_rc_from_full_iri(full_iri.toString())
                iri = res[0]
                rc = res[1]

                if 'AttributeNode:' in str(node) or 'ConceptNode:' in str(node) or 'RoleNode:' in str(
                        node) or 'IndividualNode:' in str(node):
                    node.remaining_characters = rc
                    node.setText(full_iri.toString())

                if (iri is not None):
                    all_iris.add(iri)
                    Duplicate_dict_1 = self.project.addIRINodeEntry(Duplicate_dict_1, iri, node)

                    # commands_session.append(CommandNodeAdd(self.diagram_to_place, node))

        commands_session.append(diagram_resize_command)
        commands_session.append(CommandProjectDisconnectSpecificSignals(self.project))
        commands_session.append(CommandProjetSetIRIPrefixesNodesDict(self.project, Duplicate_dict_2, Duplicate_dict_1,
                                                                     list(all_iris), None))

        commands_session.append(CommandNodesAdd([[self.diagram_to_place, nodes_to_add]]))

        commands_session.append(CommandProjetSetIRIPrefixesNodesDict(self.project, Duplicate_dict_2, Duplicate_dict_1,
                                                                     list(all_iris), None))
        commands_session.append(CommandProjectConnectSpecificSignals(self.project))

        for c in edge_commands:
            commands_session.append(c)

        attributes_and_roles_command = self.Process_metadata_for_roles_and_attributes(AllMetadataAxioms_JAVAset,
                                                                                       iri_node_str_dict)
        commands_session.append(attributes_and_roles_command)



        # commands_session.insert(0, CommandDiagramResize(self.diagram_to_place, self.diagram_to_place.sceneRect()))
        # commands_session.append(CommandDiagramResize(self.diagram_to_place, QtCore.QRectF(-5000, -5000, \
        #                (width_and_height_to_set_for_diagram[0]*300), (width_and_height_to_set_for_diagram[1]*300))))

        t1 = time.clock()

        print('Dict const time', (t1 - t_start))

        print('Pushing to undostack')

        if any(commands_session):
            self.session.undostack.beginMacro('')
            for c in commands_session:
                if c:
                    tc_start = time.clock()
                    self.session.undostack.push(c)
                    tc_end = time.clock()
                    print('c-', c, '  tt', tc_end - tc_start)
            self.session.undostack.endMacro()

        tc_start = time.clock()

        print('len(edges_to_update)',len(edges_to_update))
        print('len(self.project.edges())', len(self.project.edges()))

        for edge in edges_to_update:
            edge.updateEdge()
        tc_end = time.clock()
        print('edge_update-', tc_end - tc_start)

        print('>>> generate_and_push_commands_to_the_session END')

    def generate_and_push_commands(self, iri_node_str_dict, other_nodes_to_add, \
                                   width_to_set_for_diagram, height_to_set_for_diagram, edge_commands):

        # print('>>> generate_and_push_commands_to_the_session')

        nodes_in_dict = iri_node_str_dict.keys()

        Duplicate_dict_1 = self.project.copy_IRI_prefixes_nodes_dictionaries(self.project.IRI_prefixes_nodes_dict,
                                                                             dict())
        Duplicate_dict_2 = self.project.copy_IRI_prefixes_nodes_dictionaries(self.project.IRI_prefixes_nodes_dict,
                                                                             dict())

        commands_session = []

        all_iris = set()

        all_nodes_to_add = []
        all_nodes_to_add.extend(nodes_in_dict)
        all_nodes_to_add.extend(list(other_nodes_to_add))

        nodes_to_add = []

        for i, node_str in enumerate(all_nodes_to_add):
            node_and_iri = iri_node_str_dict[node_str]

            node = node_and_iri[0]
            full_iri = node_and_iri[1]

            res = self.project.get_iri_and_rc_from_full_iri(full_iri.toString())
            iri = res[0]
            rc = res[1]

            if node.Type in [Item.ConceptNode, Item.AttributeNode, Item.RoleNode, Item.IndividualNode]:
                node.remaining_characters = rc
                node.setText(full_iri.toString())

            nodes_to_add.append(node)

            if (iri is not None):
                all_iris.add(iri)
                Duplicate_dict_1 = self.project.addIRINodeEntry(Duplicate_dict_1, iri, node)

                # commands_session.append(CommandNodeAdd(self.diagram_to_place, node))

        commands_session.append(CommandNodesAdd([[self.diagram_to_place, nodes_to_add]]))

        commands_session.insert(0,
                                CommandProjetSetIRIPrefixesNodesDict(self.project, Duplicate_dict_2, Duplicate_dict_1,
                                                                     list(all_iris), None))
        commands_session.append(CommandProjetSetIRIPrefixesNodesDict(self.project, Duplicate_dict_2, Duplicate_dict_1,
                                                                     list(all_iris), None))
        commands_session.insert(0, CommandProjectDisconnectSpecificSignals(self.project))
        commands_session.append(CommandProjectConnectSpecificSignals(self.project))

        for c in edge_commands:
            commands_session.append(c)

        # commands_session.insert(0, CommandDiagramResize(self.diagram_to_place, self.diagram_to_place.sceneRect()))
        # commands_session.append(CommandDiagramResize(self.diagram_to_place, QtCore.QRectF(-5000, -5000, \
        #                (width_and_height_to_set_for_diagram[0]*300), (width_and_height_to_set_for_diagram[1]*300))))

        if any(commands_session):
            for c in commands_session:
                if c:
                    c.redo()

                    # print('>>> generate_and_push_commands_to_the_session END')

    def print_commands(self, all_commands):

        print('len(all_commands)', len(all_commands))

        all_nodes_str_set = set()

        for i, axiom_cmds in enumerate(all_commands):
            print(i, ')', 'len(axiom_cmds)-', len(axiom_cmds))
            for cmd in axiom_cmds:
                if 'eddy.core.node' in str(type(cmd)):
                    print('     node', cmd)
                if (str(type(cmd)) == '<class \'jnius.reflect.org.semanticweb.owlapi.model.IRI\'>'):
                    print('     cmd.toString()', cmd.toString())
                if (str(type(cmd)) == '<class \'int\'>'):
                    print('     ', cmd)
                else:
                    if str(type(cmd)) != '<class \'str\'>':
                        all_nodes_str_set.add(str(cmd))

        print('len(all_nodes_str_set)', len(all_nodes_str_set))

    def print_dictionary(self, dict, short, iri_node_str_dict):

        for key in dict.keys():
            print('key', iri_node_str_dict[key][0], '-', iri_node_str_dict[key][1].toString())
            value = dict[key]
            if not short:
                # print('str(type(value))',str(type(value)))
                if str(type(value)) == '<class \'list\'>':
                    for v in value:
                        print('    v-', iri_node_str_dict[str(v)][0], '-', iri_node_str_dict[str(v)][1].toString())
                else:
                    print('value-', value)
            else:
                print('     value-', value)

    def create_iri_node_str_node_list(self, list_inp):

        dict_op = dict()

        for axioms in list_inp:
            for i, c in enumerate(axioms):
                if str(type(c)) == '<class \'jnius.reflect.org.semanticweb.owlapi.model.IRI\'>':
                    node = axioms[i - 1]
                    if str(node) not in dict_op.keys():
                        dict_op[str(node)] = []
                    dict_op[str(node)].append(node)
                    dict_op[str(node)].append(c)

        return dict_op

    # not used
    def create_graph_edges(self, graph_dict_nodes, graph_dict_edges, iri_node_str_dict):

        list_1 = [Item.UnionNode, Item.ComplementNode, Item.DisjointUnionNode,
                  Item.DisjointUnionNode, Item.IntersectionNode]

        for super_str in graph_dict_nodes.keys():
            # print(str(type(super_str)), '!', super_str)
            # super_node = self.get_node_and_iri_from_commands(list_of_all_commands, super_str)[0]
            super_node = iri_node_str_dict[super_str][0]
            iri = iri_node_str_dict[super_str][1].toString()
            subs = graph_dict_nodes[super_str]
            if iri == 'fake':
                pass
            if super_node.Type is Item.ComplementNode:
                graph_dict_edges[Item.InputEdge.value].append([subs[0], super_node])
                if len(subs) == 1:
                    pass
                elif len(subs) == 2:
                    graph_dict_edges[Item.InclusionEdge.value].append([super_node, subs[1]])
                else:
                    LOGGER.critical('Case not considered. contact progrmmer')
            elif super_node.Type in {Item.RangeRestrictionNode, Item.DomainRestrictionNode}:
                for i, s in enumerate(subs):
                    # if i == len(subs)-1:
                    # graph_dict_edges[Item.InclusionEdge.value].append([s, super_node])
                    # else:
                    graph_dict_edges[Item.InputEdge.value].append([s, super_node])
            else:
                if iri != 'fake':
                    for s in subs:
                        if super_node.Type is Item.ConceptNode:
                            graph_dict_edges[Item.InclusionEdge.value].append([s, super_node])
                        elif super_node.Type in list_1:
                            graph_dict_edges[Item.InputEdge.value].append([s, super_node])

    def get_unionOfdisjoint_node_equivalent(self, dict_nodes):

        return_result = []

        for str_node in dict_nodes.keys():
            if 'UnionNode' in str_node:
                children = dict_nodes[str_node]
                flag = True
                for i in range(0, len(children) - 1):
                    c1 = children[i]
                    for j in range(i + 1, len(children)):
                        c2 = children[j]

                        isdisjoint = True
                        if isdisjoint is False:
                            flag = False
                            break
                if flag:
                    return_result.append(str_node)

        return return_result

    def Mark_all_disjoint_class_expressions(self, disjoint_axioms):

        for da in disjoint_axioms:
            disjoint_class_expressions = da.getClassExpressions()
            itr = disjoint_class_expressions.iterator()
            temp = []
            while (itr.hasNext()):
                dce = itr.next()
                new_castes_dce = cast(self.OWLClassExpression, dce)
                temp.append(new_castes_dce)

            self.disjoint_class_expressions_for_all_DisjointClassesAxioms.append(temp)

    def Extend_AttributesANDRoles_to_DomainANDRange_mapping_dict(self, graph_dict_nodes, iri_node_str_dict):

        for str_nd in graph_dict_nodes:
            if 'DomainRestrictionNode:' in str_nd or 'RangeRestrictionNode:' in str_nd:
                if str_nd not in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
                    children = graph_dict_nodes[str_nd]
                    self.AttributesANDRoles_to_DomainANDRange_mapping[str_nd] = children

        for str_ExFallCar_node in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
            if ('DomainRestrictionNode:' in str_ExFallCar_node) or ('RangeRestrictionNode:' in str_ExFallCar_node):
                children = graph_dict_nodes[str_ExFallCar_node]
                for child in children:
                    if ('AttributeNode:' in str(child)) or ('RoleNode:' in str(child)):
                        node = iri_node_str_dict[str_ExFallCar_node][0]
                        self.AttributesANDRoles_to_DomainANDRange_mapping[str(child)].append(node)
                    elif 'ValueDomainNode:' in str(child):
                        if ('DomainRestrictionNode:' in str_ExFallCar_node):
                            self.AttributesANDRoles_to_DomainANDRange_mapping[str_ExFallCar_node].append(child)

    def junk_code(self):

        """
        all_commands = self.convert_axioms_to_nodes_and_edges_or_metadata(iri_node_str_dict)

        declaration_commands = all_commands[0]

        #self.print_dictionary(iri_node_str_dict)

        """
        """
                for k in iri_node_str_dict:
                    print('key(str)',k)
                    value = iri_node_str_dict[k]
                    print('     value[0]',value[0])
                    print('     value[1]',value[1].toString())

                self.print_dictionary(graph_equivalent_classes,False,iri_node_str_dict)
                """

        # self.print_dictionary(graph_dict_nodes, True, iri_node_str_dict)
        """
        for k in graph_dict_edges.keys():
            print('key',k)
            value = graph_dict_edges[k]
            for v in value:
                print('     v-',v)

        for node_str in iri_node_str_dict.keys():
            print('node_str', node_str)
            value = iri_node_str_dict[node_str]
            print('     node-',value[0])
            print('     iri-',value[1])
        """
        # graph_sub_classOf = graph_dict_nodes.copy()
        # graph_disjoint_classes = dict()
        # graph_equivalent_classes = dict()

    @classmethod
    def filetype(cls):
        """
        Returns the type of the file that will be used for the import.
        :return: File
        """
        return File.Owl

    # def run(self):
    def run(self, path):

        try:

            self.sgnStarted.emit()

            t_start = time.clock()

            self.man = self.OWLManager.createOWLOntologyManager()
            self.df = self.man.getOWLDataFactory()
            self.pm = self.DefaultPrefixManager()

            self.fetch_ontology_from_file(self.path)

            # foreign_ontology_imports = self.import_do
            foreign_ontology_imports = False

            iri_node_str_dict = dict()

            graph_dict_nodes = dict()
            graph_dict_nodes_inds = dict()

            graph_dict_edges = dict()
            graph_dict_edges[Item.InclusionEdge.value] = []
            graph_dict_edges[Item.EquivalenceEdge.value] = []
            graph_dict_edges[Item.InputEdge.value] = []
            graph_dict_edges[Item.MembershipEdge.value] = []
            graph_dict_edges[Item.SameEdge.value] = []
            graph_dict_edges[Item.DifferentEdge.value] = []

            AllTboxANDRboxAxioms_JAVAset = self.HashSet()

            ObjectPropertyDomain_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.OBJECT_PROPERTY_DOMAIN)
            ObjectPropertyRange_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.OBJECT_PROPERTY_RANGE)
            DataPropertyDomain_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.DATA_PROPERTY_DOMAIN)
            DataPropertyRange_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.DATA_PROPERTY_RANGE)
            SubClassOf_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.SUBCLASS_OF)

            DisjointClass_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.DISJOINT_CLASSES)
            DisjointObjectProperty_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.DISJOINT_OBJECT_PROPERTIES)
            DisjointDataProperty_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.DISJOINT_DATA_PROPERTIES)

            EquivalentClass_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.EQUIVALENT_CLASSES)
            EquivalentObjectProperty_axioms_JAVAset = self.ontology.getAxioms(
                self.AxiomType.EQUIVALENT_OBJECT_PROPERTIES)
            EquivalentDataProperty_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.EQUIVALENT_DATA_PROPERTIES)

            InverseObjectProperties_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.INVERSE_OBJECT_PROPERTIES)
            SubObjectProperty_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.SUB_OBJECT_PROPERTY)
            SubObjectPropertychain_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.SUB_PROPERTY_CHAIN_OF)
            SubDataProperty_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.SUB_DATA_PROPERTY)

            DisjointUnion_axioms_JAVAset = self.ontology.getAxioms(self.AxiomType.DISJOINT_UNION)

            #
            AllTboxANDRboxAxioms_JAVAset.addAll(ObjectPropertyDomain_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(ObjectPropertyRange_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(DataPropertyDomain_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(DataPropertyRange_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(SubClassOf_axioms_JAVAset)

            AllTboxANDRboxAxioms_JAVAset.addAll(DisjointClass_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(DisjointObjectProperty_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(DisjointDataProperty_axioms_JAVAset)

            AllTboxANDRboxAxioms_JAVAset.addAll(EquivalentClass_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(EquivalentObjectProperty_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(EquivalentDataProperty_axioms_JAVAset)

            AllTboxANDRboxAxioms_JAVAset.addAll(InverseObjectProperties_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(SubObjectProperty_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(SubObjectPropertychain_axioms_JAVAset)
            AllTboxANDRboxAxioms_JAVAset.addAll(SubDataProperty_axioms_JAVAset)

            AllTboxANDRboxAxioms_JAVAset.addAll(DisjointUnion_axioms_JAVAset)
            #

            if foreign_ontology_imports:
                AllAxioms_JAVAset = self.ontology.getAxioms(self.Imports.INCLUDED)
            else:
                AllAxioms_JAVAset = self.ontology.getAxioms(self.Imports.EXCLUDED)

            itr_all = AllAxioms_JAVAset.iterator()

            itr_0 = DisjointUnion_axioms_JAVAset.iterator()

            itr_1 = SubClassOf_axioms_JAVAset.iterator()
            itr_1_2 = ObjectPropertyDomain_axioms_JAVAset.iterator()
            itr_1_3 = ObjectPropertyRange_axioms_JAVAset.iterator()
            itr_1_4 = DataPropertyDomain_axioms_JAVAset.iterator()
            itr_1_5 = DataPropertyRange_axioms_JAVAset.iterator()

            itr_2 = EquivalentClass_axioms_JAVAset.iterator()
            itr_2_2 = EquivalentObjectProperty_axioms_JAVAset.iterator()
            itr_2_3 = EquivalentDataProperty_axioms_JAVAset.iterator()

            itr_3 = DisjointClass_axioms_JAVAset.iterator()
            itr_3_2 = DisjointObjectProperty_axioms_JAVAset.iterator()
            itr_3_3 = DisjointDataProperty_axioms_JAVAset.iterator()

            itr_4 = SubObjectProperty_axioms_JAVAset.iterator()
            itr_4_2 = InverseObjectProperties_axioms_JAVAset.iterator()
            itr_5 = SubObjectPropertychain_axioms_JAVAset.iterator()
            itr_6 = SubDataProperty_axioms_JAVAset.iterator()

            AllAxioms = []

            DisjointUnion_axioms = []
            ObjectPropertyDomain_axioms = []
            ObjectPropertyRange_axioms = []
            DataPropertyDomain_axioms = []
            DataPropertyRange_axioms = []
            SubClassOf_axioms = []
            EquivalentClasses_axioms = []
            EquivalentObjectProperty_axioms = []
            EquivalentDataProperty_axioms = []
            DisjointClasses_axioms = []
            DisjointObjectProperty_axioms = []
            DisjointDataProperty_axioms = []
            SubObjectProperty_axioms = []
            SubObjectPropertychain_axioms = []
            InverseObjectProperties_axioms = []
            SubDataProperty_axioms = []

            while itr_all.hasNext():
                a = itr_all.next()
                AllAxioms.append(a)
            while itr_0.hasNext():
                a = itr_0.next()
                DisjointUnion_axioms.append(a)
            while itr_1.hasNext():
                a = itr_1.next()
                SubClassOf_axioms.append(a)
            while itr_1_2.hasNext():
                a = itr_1_2.next()
                ObjectPropertyDomain_axioms.append(a)
            while itr_1_3.hasNext():
                a = itr_1_3.next()
                ObjectPropertyRange_axioms.append(a)
            while itr_1_4.hasNext():
                a = itr_1_4.next()
                DataPropertyDomain_axioms.append(a)
            while itr_1_5.hasNext():
                a = itr_1_5.next()
                DataPropertyRange_axioms.append(a)
            while itr_2.hasNext():
                a = itr_2.next()
                EquivalentClasses_axioms.append(a)
            while itr_2_2.hasNext():
                a = itr_2_2.next()
                EquivalentObjectProperty_axioms.append(a)
            while itr_2_3.hasNext():
                a = itr_2_3.next()
                EquivalentDataProperty_axioms.append(a)
            while itr_3.hasNext():
                a = itr_3.next()
                DisjointClasses_axioms.append(a)
            while itr_3_2.hasNext():
                a = itr_3_2.next()
                DisjointObjectProperty_axioms.append(a)
            while itr_3_3.hasNext():
                a = itr_3_3.next()
                DisjointDataProperty_axioms.append(a)
            while itr_4.hasNext():
                a = itr_4.next()
                SubObjectProperty_axioms.append(a)
            while itr_4_2.hasNext():
                a = itr_4_2.next()
                InverseObjectProperties_axioms.append(a)
            while itr_5.hasNext():
                a = itr_5.next()
                SubObjectPropertychain_axioms.append(a)
            while itr_6.hasNext():
                a = itr_6.next()
                SubDataProperty_axioms.append(a)

            AllTboxANDRboxAxioms = []

            AllTboxANDRboxAxioms.extend(DisjointUnion_axioms)
            AllTboxANDRboxAxioms.extend(ObjectPropertyDomain_axioms)
            AllTboxANDRboxAxioms.extend(ObjectPropertyRange_axioms)
            AllTboxANDRboxAxioms.extend(DataPropertyDomain_axioms)
            AllTboxANDRboxAxioms.extend(DataPropertyRange_axioms)
            AllTboxANDRboxAxioms.extend(SubClassOf_axioms)
            AllTboxANDRboxAxioms.extend(SubObjectProperty_axioms)
            AllTboxANDRboxAxioms.extend(SubDataProperty_axioms)
            AllTboxANDRboxAxioms.extend(SubObjectPropertychain_axioms)
            AllTboxANDRboxAxioms.extend(InverseObjectProperties_axioms)
            AllTboxANDRboxAxioms.extend(EquivalentClasses_axioms)
            AllTboxANDRboxAxioms.extend(EquivalentObjectProperty_axioms)
            AllTboxANDRboxAxioms.extend(EquivalentDataProperty_axioms)
            AllTboxANDRboxAxioms.extend(DisjointClasses_axioms)
            AllTboxANDRboxAxioms.extend(DisjointObjectProperty_axioms)
            AllTboxANDRboxAxioms.extend(DisjointDataProperty_axioms)

            AllMetadataAxioms_JAVAset = self.HashSet()
            AllMetadataAxioms = []

            mda = self.ontology.getAxioms(self.AxiomType.FUNCTIONAL_DATA_PROPERTY)

            mdr1 = self.ontology.getAxioms(self.AxiomType.SYMMETRIC_OBJECT_PROPERTY)
            mdr2 = self.ontology.getAxioms(self.AxiomType.ASYMMETRIC_OBJECT_PROPERTY)
            mdr3 = self.ontology.getAxioms(self.AxiomType.TRANSITIVE_OBJECT_PROPERTY)
            mdr4 = self.ontology.getAxioms(self.AxiomType.REFLEXIVE_OBJECT_PROPERTY)
            mdr5 = self.ontology.getAxioms(self.AxiomType.IRREFLEXIVE_OBJECT_PROPERTY)
            mdr6 = self.ontology.getAxioms(self.AxiomType.FUNCTIONAL_OBJECT_PROPERTY)
            mdr7 = self.ontology.getAxioms(self.AxiomType.INVERSE_FUNCTIONAL_OBJECT_PROPERTY)

            AllMetadataAxioms_JAVAset.addAll(mda)
            AllMetadataAxioms_JAVAset.addAll(mdr1)
            AllMetadataAxioms_JAVAset.addAll(mdr2)
            AllMetadataAxioms_JAVAset.addAll(mdr3)
            AllMetadataAxioms_JAVAset.addAll(mdr4)
            AllMetadataAxioms_JAVAset.addAll(mdr5)
            AllMetadataAxioms_JAVAset.addAll(mdr6)
            AllMetadataAxioms_JAVAset.addAll(mdr7)

            AllAboxAxioms_JAVAset = self.HashSet()
            AllAboxAxioms = []

            as0 = self.ontology.getAxioms(self.AxiomType.CLASS_ASSERTION)
            as1 = self.ontology.getAxioms(self.AxiomType.DATA_PROPERTY_ASSERTION)
            as2 = self.ontology.getAxioms(self.AxiomType.NEGATIVE_DATA_PROPERTY_ASSERTION)
            as3 = self.ontology.getAxioms(self.AxiomType.OBJECT_PROPERTY_ASSERTION)
            as4 = self.ontology.getAxioms(self.AxiomType.NEGATIVE_OBJECT_PROPERTY_ASSERTION)

            AllAboxAxioms_JAVAset.addAll(as0)
            AllAboxAxioms_JAVAset.addAll(as1)
            AllAboxAxioms_JAVAset.addAll(as2)
            AllAboxAxioms_JAVAset.addAll(as3)
            AllAboxAxioms_JAVAset.addAll(as4)

            itr_0a = as0.iterator()
            itr_1a = as1.iterator()
            itr_2a = as2.iterator()
            itr_3a = as3.iterator()
            itr_4a = as4.iterator()

            CLASS_ASSERTION_axioms = []
            DATA_PROPERTY_ASSERTION_axioms = []
            NEGATIVE_DATA_PROPERTY_ASSERTION_axioms = []
            OBJECT_PROPERTY_ASSERTION_axioms = []
            NEGATIVE_OBJECT_PROPERTY_ASSERTION_axioms = []

            while itr_0a.hasNext():
                a = itr_0a.next()
                CLASS_ASSERTION_axioms.append(a)
            while itr_1a.hasNext():
                a = itr_1a.next()
                DATA_PROPERTY_ASSERTION_axioms.append(a)
            while itr_2a.hasNext():
                a = itr_2a.next()
                NEGATIVE_DATA_PROPERTY_ASSERTION_axioms.append(a)
            while itr_3a.hasNext():
                a = itr_3a.next()
                OBJECT_PROPERTY_ASSERTION_axioms.append(a)
            while itr_4a.hasNext():
                a = itr_4a.next()
                NEGATIVE_OBJECT_PROPERTY_ASSERTION_axioms.append(a)

            AllAboxAxioms.extend(CLASS_ASSERTION_axioms)
            AllAboxAxioms.extend(DATA_PROPERTY_ASSERTION_axioms)
            AllAboxAxioms.extend(NEGATIVE_DATA_PROPERTY_ASSERTION_axioms)
            AllAboxAxioms.extend(OBJECT_PROPERTY_ASSERTION_axioms)
            AllAboxAxioms.extend(NEGATIVE_OBJECT_PROPERTY_ASSERTION_axioms)

            left_over_axioms_JAVAset = self.HashSet()
            left_over_axioms_JAVAset.addAll(AllAxioms_JAVAset)
            left_over_axioms_JAVAset.removeAll(AllTboxANDRboxAxioms_JAVAset)
            left_over_axioms_JAVAset.removeAll(AllAboxAxioms_JAVAset)
            left_over_axioms_JAVAset.removeAll(AllMetadataAxioms_JAVAset)
            left_over_axioms_itr = left_over_axioms_JAVAset.iterator()
            left_over_axioms = []

            while left_over_axioms_itr.hasNext():
                la = left_over_axioms_itr.next()
                left_over_axioms.append(la)

            decl_count = 0

            for i, la in enumerate(left_over_axioms):
                if (self.AxiomType.DECLARATION.toString() == la.getAxiomType().toString()):
                    decl_count = decl_count + 1;
                else:
                    print(i, '-lo.a.Type', la.getAxiomType().toString())

            print('decl_count', decl_count)

            self.Mark_all_disjoint_class_expressions(DisjointClasses_axioms)

            self.Process_SubClassOF_axioms(SubClassOf_axioms, graph_dict_nodes, graph_dict_edges, iri_node_str_dict, 0)
            self.Process_SubClassOF_axioms(ObjectPropertyDomain_axioms, graph_dict_nodes, graph_dict_edges,
                                           iri_node_str_dict, 1)
            self.Process_SubClassOF_axioms(ObjectPropertyRange_axioms, graph_dict_nodes, graph_dict_edges,
                                           iri_node_str_dict, 2)
            self.Process_SubClassOF_axioms(DataPropertyDomain_axioms, graph_dict_nodes, graph_dict_edges,
                                           iri_node_str_dict, 3)
            self.Process_SubClassOF_axioms(DataPropertyRange_axioms, graph_dict_nodes, graph_dict_edges,
                                           iri_node_str_dict, 4)

            self.Process_DisjointUnion_axioms(DisjointUnion_axioms, graph_dict_nodes, graph_dict_edges,
                                              iri_node_str_dict)

            self.Process_EquivalentClasses_axioms(EquivalentClasses_axioms, graph_dict_nodes, graph_dict_edges,
                                                  iri_node_str_dict)
            self.Process_EquivalentObjectANDDataProperties_axioms(EquivalentObjectProperty_axioms, graph_dict_nodes,
                                                                  graph_dict_edges, iri_node_str_dict, 0)
            self.Process_EquivalentObjectANDDataProperties_axioms(EquivalentDataProperty_axioms, graph_dict_nodes,
                                                                  graph_dict_edges, iri_node_str_dict, 1)

            self.Process_SubObjectPropertyOF_axioms(SubObjectProperty_axioms, graph_dict_nodes, graph_dict_edges,
                                                    iri_node_str_dict, False)
            self.Process_SubObjectPropertyOF_axioms(SubObjectPropertychain_axioms, graph_dict_nodes, graph_dict_edges,
                                                    iri_node_str_dict, True)
            self.Process_SubDataPropertyOF_axioms(SubDataProperty_axioms, graph_dict_nodes, graph_dict_edges,
                                                  iri_node_str_dict)
            self.Process_InverseObjectProperties_axioms(InverseObjectProperties_axioms, graph_dict_nodes,
                                                        graph_dict_edges, iri_node_str_dict)

            # disjoint classes axioms are processed in the end because there is no need to generate not nodes between pairs of disjoint C.Es
            # if a black node already exists between them.
            # Hence, all class expressions should first be converted into nodes and edges.
            # self.Process_DisjointClasses_axioms(DisjointClasses_axioms, graph_dict_nodes, graph_dict_edges, iri_node_str_dict)
            self.Process_DisjointClasses_axioms_2(graph_dict_nodes, graph_dict_edges, iri_node_str_dict)
            self.Process_DisjointObjectANDDataProperties_axioms(DisjointObjectProperty_axioms, graph_dict_nodes,
                                                                graph_dict_edges, iri_node_str_dict, 0)
            self.Process_DisjointObjectANDDataProperties_axioms(DisjointDataProperty_axioms, graph_dict_nodes,
                                                                graph_dict_edges, iri_node_str_dict, 1)

            # for k in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
            #    print('k0',k)
            #    print('     v0',self.AttributesANDRoles_to_DomainANDRange_mapping[k])

            self.Extend_AttributesANDRoles_to_DomainANDRange_mapping_dict(graph_dict_nodes, iri_node_str_dict)
            self.get_total_number_of_slots_of_attributes_for_classexpressions(iri_node_str_dict)

            white_OR_black_square_nodes_not_placed = []
            for str_nd in graph_dict_nodes:
                if 'DomainRestrictionNode:' in str_nd or 'RangeRestrictionNode:' in str_nd:
                    if str_nd not in self.AttributesANDRoles_to_DomainANDRange_mapping.keys():
                        white_OR_black_square_nodes_not_placed.append(str_nd)
            if len(white_OR_black_square_nodes_not_placed) > 0:
                print('>>>      White or black square nodes not connected to attributes or roles:')
                for wb in white_OR_black_square_nodes_not_placed:
                    print('     ', wb)
                print('>>>      White or black square nodes not connected to attributes or roles: END')

            self.Process_Assertion_axioms(CLASS_ASSERTION_axioms, graph_dict_nodes_inds, graph_dict_edges, iri_node_str_dict, 0)
            self.Process_Assertion_axioms(DATA_PROPERTY_ASSERTION_axioms, graph_dict_nodes_inds, graph_dict_edges, iri_node_str_dict, 11)
            self.Process_Assertion_axioms(NEGATIVE_DATA_PROPERTY_ASSERTION_axioms, graph_dict_nodes_inds, graph_dict_edges, iri_node_str_dict, 12)
            self.Process_Assertion_axioms(OBJECT_PROPERTY_ASSERTION_axioms, graph_dict_nodes_inds, graph_dict_edges, iri_node_str_dict, 21)
            self.Process_Assertion_axioms(NEGATIVE_OBJECT_PROPERTY_ASSERTION_axioms, graph_dict_nodes_inds, graph_dict_edges, iri_node_str_dict, 22)

            method_return_values = self.place_graph_in_a_diagram(graph_dict_nodes, graph_dict_nodes_inds, graph_dict_edges, iri_node_str_dict)

            diagram_resize_command = method_return_values[0]
            edge_commands = method_return_values[1]
            edges_to_update = method_return_values[2]

            t_end = time.clock()

            print('algorithm computation', t_end - t_start)

            self.step(+50)

            self.generate_and_push_commands_to_the_session(iri_node_str_dict, set(), diagram_resize_command, \
                                       edge_commands, edges_to_update, AllMetadataAxioms_JAVAset)
            self.step(+50)

            detach()

            # self.generate_and_push_commands(iri_node_str_dict, set(), width_to_set_for_diagram, \
            #                                               height_to_set_for_diagram, edge_commands)
        except Exception as e:
            LOGGER.exception('OWL 2 export could not be completed')
            self.sgnErrored.emit(e)
        else:
            self.project.sgnUpdated.emit()
            self.sgnCompleted.emit()
        finally:
            # self.finished.emit()
            t_end_final = time.clock()

            print('total time taken = ', t_end_final - t_start)