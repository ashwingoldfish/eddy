# -*- coding: utf-8 -*-

##########################################################################
#                                                                        #
#  Eddy: a graphical editor for the specification of Graphol ontologies  #
#  Copyright (C) 2015 Daniele Pantaleone <pantaleone@dis.uniroma1.it>    #
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


import os

from PyQt5 import QtXml

from eddy.core.datatypes.graphol import Item
from eddy.core.datatypes.system import File
from eddy.core.exporters.common import AbstractProjectExporter
from eddy.core.functions.misc import postfix
from eddy.core.functions.fsystem import fwrite, mkdir
from eddy.core.output import getLogger
from eddy.core.project import Project


LOGGER = getLogger(__name__)


class GrapholProjectExporter(AbstractProjectExporter):
    """
    Extends AbstractProjectExporter with facilities to export the structure of a Graphol project.
    A Graphol project is stored in a directory, whose structure is the following:
    -----------------------
    - projectname/
    -   projectname.graphol     # contains information on the ontology
    -   ...
    """
    def __init__(self, project, session=None):
        """
        Initialize the project exporter.
        :type project: Project
        :type session: Session
        """
        super().__init__(project, session)

        self.document = None

        self.itemToXml = {
            Item.AttributeNode: 'attribute',
            Item.ComplementNode: 'complement',
            Item.ConceptNode: 'concept',
            Item.DatatypeRestrictionNode: 'datatype-restriction',
            Item.DisjointUnionNode: 'disjoint-union',
            Item.DomainRestrictionNode: 'domain-restriction',
            Item.EnumerationNode: 'enumeration',
            Item.FacetNode: 'facet',
            Item.IndividualNode: 'individual',
            Item.IntersectionNode: 'intersection',
            Item.PropertyAssertionNode: 'property-assertion',
            Item.RangeRestrictionNode: 'range-restriction',
            Item.RoleNode: 'role',
            Item.RoleChainNode: 'role-chain',
            Item.RoleInverseNode: 'role-inverse',
            Item.UnionNode: 'union',
            Item.ValueDomainNode: 'value-domain',
            Item.InclusionEdge: 'inclusion',
            Item.EquivalenceEdge: 'equivalence',
            Item.InputEdge: 'input',
            Item.MembershipEdge: 'membership',
        }
        
        self.exportFuncForItem = {
            Item.AttributeNode: self.exportAttributeNode,
            Item.ComplementNode: self.exportComplementNode,
            Item.ConceptNode: self.exportConceptNode,
            Item.DatatypeRestrictionNode: self.exportDatatypeRestrictionNode,
            Item.DisjointUnionNode: self.exportDisjointUnionNode,
            Item.DomainRestrictionNode: self.exportDomainRestrictionNode,
            Item.EnumerationNode: self.exportEnumerationNode,
            Item.FacetNode: self.exportFacetNode,
            Item.IndividualNode: self.exportIndividualNode,
            Item.IntersectionNode: self.exportIntersectionNode,
            Item.PropertyAssertionNode: self.exportPropertyAssertionNode,
            Item.RangeRestrictionNode: self.exportRangeRestrictionNode,
            Item.RoleNode: self.exportRoleNode,
            Item.RoleChainNode: self.exportRoleChainNode,
            Item.RoleInverseNode: self.exportRoleInverseNode,
            Item.UnionNode: self.exportUnionNode,
            Item.ValueDomainNode: self.exportValueDomainNode,
            Item.InclusionEdge: self.exportInclusionEdge,
            Item.EquivalenceEdge: self.exportEquivalenceEdge,
            Item.InputEdge: self.exportInputEdge,
            Item.MembershipEdge: self.exportMembershipEdge,
        }

        self.exportMetaFuncForItem = {
            Item.AttributeNode: self.exportAttributeMeta,
            Item.ConceptNode: self.exportPredicateMeta,
            Item.RoleNode: self.exportRoleMeta,
        }

    #############################################
    #   ONTOLOGY PREDICATES EXPORT
    #################################

    def exportPredicateMeta(self, item, name):
        """
        Export predicate metadata.
        :type item: Item
        :type name: str
        :rtype: QDomElement
        """
        meta = self.project.meta(item, name)
        element = self.document.createElement('predicate')
        element.setAttribute('type', self.itemToXml[item])
        element.setAttribute('name', name)
        description = self.document.createElement('description')
        description.appendChild(self.document.createTextNode(meta.get('description', '')))
        url = self.document.createElement('url')
        url.appendChild(self.document.createTextNode(meta.get('url', '')))
        element.appendChild(url)
        element.appendChild(description)
        return element

    def exportAttributeMeta(self, item, name):
        """
        Export attribute metadata.
        :type item: Item
        :type name: str
        :rtype: QDomElement
        """
        element = self.exportPredicateMeta(item, name)
        meta = self.project.meta(item, name)
        functional = self.document.createElement('functional')
        functional.appendChild(self.document.createTextNode(str(int(meta.get('functional', False)))))
        element.appendChild(functional)
        return element
    
    def exportRoleMeta(self, item, name):
        """
        Export role metadata.
        :type item: Item
        :type name: str
        :rtype: QDomElement
        """
        element = self.exportPredicateMeta(item, name)
        meta = self.project.meta(item, name)
        functional = self.document.createElement('functional')
        functional.appendChild(self.document.createTextNode(str(int(meta.get('functional', False)))))
        inverseFunctional = self.document.createElement('inverseFunctional')
        inverseFunctional.appendChild(self.document.createTextNode(str(int(meta.get('inverseFunctional', False)))))
        asymmetric = self.document.createElement('asymmetric')
        asymmetric.appendChild(self.document.createTextNode(str(int(meta.get('asymmetric', False)))))
        irreflexive = self.document.createElement('irreflexive')
        irreflexive.appendChild(self.document.createTextNode(str(int(meta.get('irreflexive', False)))))
        reflexive = self.document.createElement('reflexive')
        reflexive.appendChild(self.document.createTextNode(str(int(meta.get('reflexive', False)))))
        symmetric = self.document.createElement('symmetric')
        symmetric.appendChild(self.document.createTextNode(str(int(meta.get('symmetric', False)))))
        transitive = self.document.createElement('transitive')
        transitive.appendChild(self.document.createTextNode(str(int(meta.get('transitive', False)))))
        element.appendChild(functional)
        element.appendChild(inverseFunctional)
        element.appendChild(asymmetric)
        element.appendChild(irreflexive)
        element.appendChild(reflexive)
        element.appendChild(symmetric)
        element.appendChild(transitive)
        return element

    #############################################
    #   ONTOLOGY DIAGRAMS EXPORT : NODES
    #################################

    def exportAttributeNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: AttributeNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportComplementNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: ComplementNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportConceptNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: ConceptNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportDatatypeRestrictionNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: DatatypeRestrictionNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportDisjointUnionNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: DisjointUnionNode
        :rtype: QDomElement
        """
        return self.exportGenericNode(node)

    def exportDomainRestrictionNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: DomainRestrictionNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportEnumerationNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: EnumerationNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportFacetNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: FacetNode
        :rtype: QDomElement
        """
        position = node.mapToScene(node.textPos())
        label = self.document.createElement('label')
        label.setAttribute('height', node.labelA.height())
        label.setAttribute('width', node.labelA.width() + node.labelB.width())
        label.setAttribute('x', position.x())
        label.setAttribute('y', position.y())
        label.appendChild(self.document.createTextNode(node.text()))
        element = self.exportGenericNode(node)
        element.appendChild(label)
        return element

    def exportIndividualNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: IndividualNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportIntersectionNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: IntersectionNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportPropertyAssertionNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: PropertyAssertionNode
        :rtype: QDomElement
        """
        element = self.exportGenericNode(node)
        element.setAttribute('inputs', ','.join(node.inputs))
        return element

    def exportRangeRestrictionNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: RangeRestrictionNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportRoleNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: RoleNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportRoleChainNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: RoleChainNode
        :rtype: QDomElement
        """
        element = self.exportLabelNode(node)
        element.setAttribute('inputs', ','.join(node.inputs))
        return element

    def exportRoleInverseNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: RoleInverseNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportValueDomainNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: ValueDomainNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    def exportUnionNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: UnionNode
        :rtype: QDomElement
        """
        return self.exportLabelNode(node)

    #############################################
    #   ONTOLOGY DIAGRAMS EXPORT : EDGES
    #################################

    def exportInclusionEdge(self, edge):
        """
        Export the given edge into a QDomElement.
        :type edge: InclusionEdge
        :rtype: QDomElement
        """
        return self.exportGenericEdge(edge)

    def exportEquivalenceEdge(self, edge):
        """
        Export the given edge into a QDomElement.
        :type edge: EquivalenceEdge
        :rtype: QDomElement
        """
        return self.exportGenericEdge(edge)

    def exportInputEdge(self, edge):
        """
        Export the given edge into a QDomElement.
        :type edge: InputEdge
        :rtype: QDomElement
        """
        return self.exportGenericEdge(edge)

    def exportMembershipEdge(self, edge):
        """
        Export the given edge into a QDomElement.
        :type edge: MembershipEdge
        :rtype: QDomElement
        """
        return self.exportGenericEdge(edge)

    #############################################
    #   ONTOLOGY DIAGRAMS EXPORT : GENERICS
    #################################

    def exportLabelNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: AbstractNode
        :rtype: QDomElement
        """
        position = node.mapToScene(node.textPos())
        label = self.document.createElement('label')
        label.setAttribute('height', node.label.height())
        label.setAttribute('width', node.label.width())
        label.setAttribute('x', position.x())
        label.setAttribute('y', position.y())
        label.appendChild(self.document.createTextNode(node.text()))
        element = self.exportGenericNode(node)
        element.appendChild(label)
        return element

    def exportGenericEdge(self, edge):
        """
        Export the given node into a QDomElement.
        :type edge: AbstractEdge
        :rtype: QDomElement
        """
        element = self.document.createElement('edge')
        element.setAttribute('source', edge.source.id)
        element.setAttribute('target', edge.target.id)
        element.setAttribute('id', edge.id)
        element.setAttribute('type', self.itemToXml[edge.type()])

        for p in [edge.source.anchor(edge)] + edge.breakpoints + [edge.target.anchor(edge)]:
            point = self.document.createElement('point')
            point.setAttribute('x', p.x())
            point.setAttribute('y', p.y())
            element.appendChild(point)

        return element

    def exportGenericNode(self, node):
        """
        Export the given node into a QDomElement.
        :type node: AbstractNode
        :rtype: QDomElement
        """
        element = self.document.createElement('node')
        element.setAttribute('id', node.id)
        element.setAttribute('type', self.itemToXml[node.type()])
        element.setAttribute('color', node.brush().color().name())
        geometry = self.document.createElement('geometry')
        geometry.setAttribute('height', node.height())
        geometry.setAttribute('width', node.width())
        geometry.setAttribute('x', node.pos().x())
        geometry.setAttribute('y', node.pos().y())
        element.appendChild(geometry)
        return element

    #############################################
    #   MAIN EXPORT
    #################################

    def createDiagrams(self):
        """
        Create the 'diagrams' element in the QDomDocument.
        """
        section = self.document.createElement('diagrams')
        for diagram in self.project.diagrams():
            subsection = self.document.createElement('diagram')
            subsection.setAttribute('name', diagram.name)
            subsection.setAttribute('width', diagram.width())
            subsection.setAttribute('height', diagram.height())
            for node in diagram.nodes():
                func = self.exportFuncForItem[node.type()]
                subsection.appendChild(func(node))
            for edge in diagram.edges():
                func = self.exportFuncForItem[edge.type()]
                subsection.appendChild(func(edge))
            section.appendChild(subsection)
        self.document.documentElement().appendChild(section)

    def createDomDocument(self):
        """
        Create the QDomDocument where to store project information.
        """
        self.document = QtXml.QDomDocument()
        instruction = self.document.createProcessingInstruction('xml', 'version="1.0" encoding="UTF-8"')
        self.document.appendChild(instruction)
        graphol = self.document.createElement('graphol')
        graphol.setAttribute('version', '2')
        self.document.appendChild(graphol)

    def createOntology(self):
        """
        Create the 'ontology' element in the QDomDocument.
        """
        iri = self.document.createElement('iri')
        iri.appendChild(self.document.createTextNode(self.project.iri))
        name = self.document.createElement('name')
        name.appendChild(self.document.createTextNode(self.project.name))
        prefix = self.document.createElement('prefix')
        prefix.appendChild(self.document.createTextNode(self.project.prefix))
        profile = self.document.createElement('profile')
        profile.appendChild(self.document.createTextNode(self.project.profile.name()))
        section = self.document.createElement('ontology')
        section.appendChild(name)
        section.appendChild(prefix)
        section.appendChild(iri)
        section.appendChild(profile)
        self.document.documentElement().appendChild(section)

    def createPredicatesMeta(self):
        """
        Create the 'predicates' element in the QDomDocument.
        """
        section = self.document.createElement('predicates')
        for item, predicate in self.project.metas():
            func = self.exportMetaFuncForItem[item]
            meta = func(item, predicate)
            section.appendChild(meta)
        self.document.documentElement().appendChild(section)

    def createProjectFile(self):
        """
        Serialize a previously created QDomDocument to disk.
        """
        try:
            mkdir(self.project.path)
            filename = postfix(self.project.name, File.Graphol.extension)
            filepath = os.path.join(self.project.path, filename)
            fwrite(self.document.toString(2), filepath)
        except Exception as e:
            raise e
        else:
            LOGGER.info('Saved project %s to %s', self.project.name, self.project.path)

    #############################################
    #   INTERFACE
    #################################

    def export(self, *args, **kwargs):
        """
        Perform Project export to disk.
        """
        self.createDomDocument()
        self.createOntology()
        self.createPredicatesMeta()
        self.createDiagrams()
        self.createProjectFile()

    @classmethod
    def filetype(cls):
        """
        Returns the type of the file that will be used for the export.
        :return: File
        """
        return File.Graphol