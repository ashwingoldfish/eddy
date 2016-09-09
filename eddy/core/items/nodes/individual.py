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


import math

from PyQt5 import QtCore
from PyQt5 import QtGui

from eddy.core.datatypes.graphol import Identity, Item
from eddy.core.datatypes.owl import Datatype
from eddy.core.functions.misc import snapF
from eddy.core.items.nodes.common.base import AbstractResizableNode
from eddy.core.items.nodes.common.label import NodeLabel
from eddy.core.polygon import Polygon
from eddy.core.regex import RE_VALUE


class IndividualNode(AbstractResizableNode):
    """
    This class implements the 'Individual' node.
    """
    IndexLT = 0
    IndexLB = 1
    IndexBL = 2
    IndexBR = 3
    IndexRB = 4
    IndexRT = 5
    IndexTR = 6
    IndexTL = 7
    IndexEE = 8

    DefaultBrush = QtGui.QBrush(QtGui.QColor(252, 252, 252, 255))
    DefaultPen = QtGui.QPen(QtGui.QBrush(QtGui.QColor(0, 0, 0, 255)), 1.0, QtCore.Qt.SolidLine, QtCore.Qt.RoundCap, QtCore.Qt.RoundJoin)
    Identities = {Identity.Individual, Identity.Value}
    Type = Item.IndividualNode

    def __init__(self, width=60, height=60, brush=None, **kwargs):
        """
        Initialize the node.
        :type width: int
        :type height: int
        :type brush: QBrush
        """
        super(IndividualNode, self).__init__(**kwargs)

        w = max(width, 60)
        h = max(height, 60)
        brush = brush or IndividualNode.DefaultBrush
        pen = IndividualNode.DefaultPen

        createPolygon = lambda x, y: QtGui.QPolygonF([
            QtCore.QPointF(-(x / 2), -((y / (1 + math.sqrt(2))) / 2)),
            QtCore.QPointF(-(x / 2), +((y / (1 + math.sqrt(2))) / 2)),
            QtCore.QPointF(-((x / (1 + math.sqrt(2))) / 2), +(y / 2)),
            QtCore.QPointF(+((x / (1 + math.sqrt(2))) / 2), +(y / 2)),
            QtCore.QPointF(+(x / 2), +((y / (1 + math.sqrt(2))) / 2)),
            QtCore.QPointF(+(x / 2), -((y / (1 + math.sqrt(2))) / 2)),
            QtCore.QPointF(+((x / (1 + math.sqrt(2))) / 2), -(y / 2)),
            QtCore.QPointF(-((x / (1 + math.sqrt(2))) / 2), -(y / 2)),
            QtCore.QPointF(-(x / 2), -((y / (1 + math.sqrt(2))) / 2)),
        ])


        self.background = Polygon(createPolygon(w + 8, h + 8))
        self.selection = Polygon(QtCore.QRectF(-(w + 8) / 2, -(h + 8) / 2, w + 8, h + 8))
        self.polygon = Polygon(createPolygon(w, h), brush, pen)
        self.label = NodeLabel(template='individual', pos=self.center, parent=self)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.updateNode()
        self.updateTextPos()

    #############################################
    #   PROPERTIES
    #################################

    @property
    def datatype(self):
        """
        Returns the datatype associated with this node.
        :rtype: Datatype
        """
        match = RE_VALUE.match(self.text())
        if match:
            return Datatype.forValue(match.group('datatype'))
        return None

    @property
    def value(self):
        """
        Returns the value value associated with this node.
        :rtype: str
        """
        match = RE_VALUE.match(self.text())
        if match:
            return match.group('value')
        return None

    #############################################
    #   INTERFACE
    #################################

    def boundingRect(self):
        """
        Returns the shape bounding rectangle.
        :rtype: QtCore.QRectF
        """
        return self.selection.geometry()

    @staticmethod
    def compose(value, datatype):
        """
        Compose the value string.
        :type value: str
        :type datatype: Datatype
        :return: str
        """
        return '"{0}"^^{1}'.format(value.strip('"'), datatype.value)

    def copy(self, diagram):
        """
        Create a copy of the current item.
        :type diagram: Diagram
        """
        node = diagram.factory.create(self.type(), **{
            'id': self.id,
            'brush': self.brush(),
            'height': self.height(),
            'width': self.width()
        })
        node.setPos(self.pos())
        node.setText(self.text())
        node.setTextPos(node.mapFromScene(self.mapToScene(self.textPos())))
        return node

    def height(self):
        """
        Returns the height of the shape.
        :rtype: int
        """
        polygon = self.polygon.geometry()
        return polygon[self.IndexTR].y() - polygon[self.IndexBR].y()

    def identity(self):
        """
        Returns the identity of the current node.
        :rtype: Identity
        """
        match = RE_VALUE.match(self.text())
        if match:
            return Identity.Value
        return Identity.Individual

    def paint(self, painter, option, widget=None):
        """
        Paint the node in the diagram.
        :type painter: QPainter
        :type option: QStyleOptionGraphicsItem
        :type widget: QWidget
        """
        # SET THE RECT THAT NEEDS TO BE REPAINTED
        painter.setClipRect(option.exposedRect)
        # SELECTION AREA
        painter.setPen(self.selection.pen())
        painter.setBrush(self.selection.brush())
        painter.drawRect(self.selection.geometry())
        # SYNTAX VALIDATION
        painter.setRenderHint(QtGui.QPainter.Antialiasing)
        painter.setPen(self.background.pen())
        painter.setBrush(self.background.brush())
        painter.drawPolygon(self.background.geometry())
        # ITEM SHAPE
        painter.setPen(self.polygon.pen())
        painter.setBrush(self.polygon.brush())
        painter.drawPolygon(self.polygon.geometry())
        # RESIZE HANDLES
        painter.setRenderHint(QtGui.QPainter.Antialiasing)
        for polygon in self.handles:
            painter.setPen(polygon.pen())
            painter.setBrush(polygon.brush())
            painter.drawEllipse(polygon.geometry())

    def painterPath(self):
        """
        Returns the current shape as QtGui.QPainterPath (used for collision detection).
        :rtype: QPainterPath
        """
        path = QtGui.QPainterPath()
        path.addPolygon(self.polygon.geometry())
        return path

    def resize(self, mousePos):
        """
        Handle the interactive resize of the shape.
        :type mousePos: QtCore.QPointF
        """
        snap = self.session.action('toggle_grid').isChecked()
        size = self.diagram.GridSize
        moved = self.label.isMoved()

        background = self.background.geometry()
        selection = self.selection.geometry()
        polygon = self.polygon.geometry()
        
        R = QtCore.QRectF(self.boundingRect())
        D = QtCore.QPointF(0, 0)

        mbrh = 68
        mbrw = 68

        self.prepareGeometryChange()

        if self.mp_Handle == self.HandleTL:

            fromX = self.mp_Bound.left()
            fromY = self.mp_Bound.top()
            toX = fromX + mousePos.x() - self.mp_Pos.x()
            toY = fromY + mousePos.y() - self.mp_Pos.y()
            toX = snapF(toX, size, -4, snap)
            toY = snapF(toY, size, -4, snap)
            D.setX(toX - fromX)
            D.setY(toY - fromY)
            R.setLeft(toX)
            R.setTop(toY)

            ## CLAMP SIZE
            if R.width() < mbrw:
                D.setX(D.x() - mbrw + R.width())
                R.setLeft(R.left() - mbrw + R.width())
            if R.height() < mbrh:
                D.setY(D.y() - mbrh + R.height())
                R.setTop(R.top() - mbrh + R.height())

            newSideY = (R.height() - 4 * 2) / (1 + math.sqrt(2))
            newSideX = (R.width() - 4 * 2) / (1 + math.sqrt(2))
            newLeftRightBottomY = (R.y() + R.height() / 2) + newSideY / 2
            newLeftRightTopY = (R.y() + R.height() / 2) - newSideY / 2
            newTopBottomLeftX = (R.x() + R.width() / 2) - newSideX / 2
            newTopBottomRightX = (R.x() + R.width() / 2) + newSideX / 2

            selection.setLeft(R.left())
            selection.setTop(R.top())
            
            background[self.IndexLT] = QtCore.QPointF(R.left(), newLeftRightTopY)
            background[self.IndexLB] = QtCore.QPointF(R.left(), newLeftRightBottomY)
            background[self.IndexRT] = QtCore.QPointF(R.right(), newLeftRightTopY)
            background[self.IndexRB] = QtCore.QPointF(R.right(), newLeftRightBottomY)
            background[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, R.top())
            background[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, R.top())
            background[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, R.bottom())
            background[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, R.bottom())
            background[self.IndexEE] = QtCore.QPointF(R.left(), newLeftRightTopY)

            polygon[self.IndexLT] = QtCore.QPointF(R.left() + 4, newLeftRightTopY)
            polygon[self.IndexLB] = QtCore.QPointF(R.left() + 4, newLeftRightBottomY)
            polygon[self.IndexRT] = QtCore.QPointF(R.right() - 4, newLeftRightTopY)
            polygon[self.IndexRB] = QtCore.QPointF(R.right() - 4, newLeftRightBottomY)
            polygon[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, R.top() + 4)
            polygon[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, R.top() + 4)
            polygon[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, R.bottom() - 4)
            polygon[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, R.bottom() - 4)
            polygon[self.IndexEE] = QtCore.QPointF(R.left() + 4, newLeftRightTopY)

        elif self.mp_Handle == self.HandleTM:

            fromY = self.mp_Bound.top()
            toY = fromY + mousePos.y() - self.mp_Pos.y()
            toY = snapF(toY, size, -4, snap)
            D.setY(toY - fromY)
            R.setTop(toY)

            ## CLAMP SIZE
            if R.height() < mbrh:
                D.setY(D.y() - mbrh + R.height())
                R.setTop(R.top() - mbrh + R.height())

            newSide = (R.height() - 4 * 2) / (1 + math.sqrt(2))
            newLeftRightBottomY = (R.y() + R.height() / 2) + newSide / 2
            newLeftRightTopY = (R.y() + R.height() / 2) - newSide / 2
            
            selection.setTop(R.top())
            
            background[self.IndexTL] = QtCore.QPointF(background[self.IndexTL].x(), R.top())
            background[self.IndexTR] = QtCore.QPointF(background[self.IndexTR].x(), R.top())
            background[self.IndexLB] = QtCore.QPointF(background[self.IndexLB].x(), newLeftRightBottomY)
            background[self.IndexRB] = QtCore.QPointF(background[self.IndexRB].x(), newLeftRightBottomY)
            background[self.IndexLT] = QtCore.QPointF(background[self.IndexLT].x(), newLeftRightTopY)
            background[self.IndexRT] = QtCore.QPointF(background[self.IndexRT].x(), newLeftRightTopY)
            background[self.IndexEE] = QtCore.QPointF(background[self.IndexEE].x(), newLeftRightTopY)
            
            polygon[self.IndexTL] = QtCore.QPointF(polygon[self.IndexTL].x(), R.top() + 4)
            polygon[self.IndexTR] = QtCore.QPointF(polygon[self.IndexTR].x(), R.top() + 4)
            polygon[self.IndexLB] = QtCore.QPointF(polygon[self.IndexLB].x(), newLeftRightBottomY)
            polygon[self.IndexRB] = QtCore.QPointF(polygon[self.IndexRB].x(), newLeftRightBottomY)
            polygon[self.IndexLT] = QtCore.QPointF(polygon[self.IndexLT].x(), newLeftRightTopY)
            polygon[self.IndexRT] = QtCore.QPointF(polygon[self.IndexRT].x(), newLeftRightTopY)
            polygon[self.IndexEE] = QtCore.QPointF(polygon[self.IndexEE].x(), newLeftRightTopY)

        elif self.mp_Handle == self.HandleTR:

            fromX = self.mp_Bound.right()
            fromY = self.mp_Bound.top()
            toX = fromX + mousePos.x() - self.mp_Pos.x()
            toY = fromY + mousePos.y() - self.mp_Pos.y()
            toX = snapF(toX, size, +4, snap)
            toY = snapF(toY, size, -4, snap)
            D.setX(toX - fromX)
            D.setY(toY - fromY)
            R.setRight(toX)
            R.setTop(toY)

            ## CLAMP SIZE
            if R.width() < mbrw:
                D.setX(D.x() + mbrw - R.width())
                R.setRight(R.right() + mbrw - R.width())
            if R.height() < mbrh:
                D.setY(D.y() - mbrh + R.height())
                R.setTop(R.top() - mbrh + R.height())

            newSideY = (R.height() - 4 * 2) / (1 + math.sqrt(2))
            newSideX = (R.width() - 4 * 2) / (1 + math.sqrt(2))
            newLeftRightBottomY = (R.y() + R.height() / 2) + newSideY / 2
            newLeftRightTopY = (R.y() + R.height() / 2) - newSideY / 2
            newTopBottomLeftX = (R.x() + R.width() / 2) - newSideX / 2
            newTopBottomRightX = (R.x() + R.width() / 2) + newSideX / 2
            
            selection.setRight(R.right())
            selection.setTop(R.top())
            
            background[self.IndexLT] = QtCore.QPointF(R.left(), newLeftRightTopY)
            background[self.IndexLB] = QtCore.QPointF(R.left(), newLeftRightBottomY)
            background[self.IndexRT] = QtCore.QPointF(R.right(), newLeftRightTopY)
            background[self.IndexRB] = QtCore.QPointF(R.right(), newLeftRightBottomY)
            background[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, R.top())
            background[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, R.top())
            background[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, R.bottom())
            background[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, R.bottom())
            background[self.IndexEE] = QtCore.QPointF(R.left(), newLeftRightTopY)
            
            polygon[self.IndexLT] = QtCore.QPointF(R.left() + 4, newLeftRightTopY)
            polygon[self.IndexLB] = QtCore.QPointF(R.left() + 4, newLeftRightBottomY)
            polygon[self.IndexRT] = QtCore.QPointF(R.right() - 4, newLeftRightTopY)
            polygon[self.IndexRB] = QtCore.QPointF(R.right() - 4, newLeftRightBottomY)
            polygon[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, R.top() + 4)
            polygon[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, R.top() + 4)
            polygon[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, R.bottom() - 4)
            polygon[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, R.bottom() - 4)
            polygon[self.IndexEE] = QtCore.QPointF(R.left() + 4, newLeftRightTopY)

        elif self.mp_Handle == self.HandleML:

            fromX = self.mp_Bound.left()
            toX = fromX + mousePos.x() - self.mp_Pos.x()
            toX = snapF(toX, size, -4, snap)
            D.setX(toX - fromX)
            R.setLeft(toX)

            ## CLAMP SIZE
            if R.width() < mbrw:
                D.setX(D.x() - mbrw + R.width())
                R.setLeft(R.left() - mbrw + R.width())

            newSide = (R.width() - 4 * 2) / (1 + math.sqrt(2))
            newTopBottomLeftX = (R.x() + R.width() / 2) - newSide / 2
            newTopBottomRightX = (R.x() + R.width() / 2) + newSide / 2

            selection.setLeft(R.left())
            
            background[self.IndexLT] = QtCore.QPointF(R.left(), background[self.IndexLT].y())
            background[self.IndexLB] = QtCore.QPointF(R.left(), background[self.IndexLB].y())
            background[self.IndexEE] = QtCore.QPointF(R.left(), background[self.IndexEE].y())
            background[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, background[self.IndexTL].y())
            background[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, background[self.IndexTR].y())
            background[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, background[self.IndexBL].y())
            background[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, background[self.IndexBR].y())
            
            polygon[self.IndexLT] = QtCore.QPointF(R.left() + 4, polygon[self.IndexLT].y())
            polygon[self.IndexLB] = QtCore.QPointF(R.left() + 4, polygon[self.IndexLB].y())
            polygon[self.IndexEE] = QtCore.QPointF(R.left() + 4, polygon[self.IndexEE].y())
            polygon[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, polygon[self.IndexTL].y())
            polygon[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, polygon[self.IndexTR].y())
            polygon[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, polygon[self.IndexBL].y())
            polygon[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, polygon[self.IndexBR].y())

        elif self.mp_Handle == self.HandleMR:

            fromX = self.mp_Bound.right()
            toX = fromX + mousePos.x() - self.mp_Pos.x()
            toX = snapF(toX, size, +4, snap)
            D.setX(toX - fromX)
            R.setRight(toX)

            ## CLAMP SIZE
            if R.width() < mbrw:
                D.setX(D.x() + mbrw - R.width())
                R.setRight(R.right() + mbrw - R.width())

            newSide = (R.width() - 4 * 2) / (1 + math.sqrt(2))
            newTopBottomRightX = (R.x() + R.width() / 2) + newSide / 2
            newTopBottomLeftX = (R.x() + R.width() / 2) - newSide / 2
            
            selection.setRight(R.right())
            
            background[self.IndexRT] = QtCore.QPointF(R.right(), background[self.IndexRT].y())
            background[self.IndexRB] = QtCore.QPointF(R.right(), background[self.IndexRB].y())
            background[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, background[self.IndexTL].y())
            background[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, background[self.IndexTR].y())
            background[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, background[self.IndexBL].y())
            background[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, background[self.IndexBR].y())
            
            polygon[self.IndexRT] = QtCore.QPointF(R.right() - 4, polygon[self.IndexRT].y())
            polygon[self.IndexRB] = QtCore.QPointF(R.right() - 4, polygon[self.IndexRB].y())
            polygon[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, polygon[self.IndexTL].y())
            polygon[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, polygon[self.IndexTR].y())
            polygon[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, polygon[self.IndexBL].y())
            polygon[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, polygon[self.IndexBR].y())

        elif self.mp_Handle == self.HandleBL:

            fromX = self.mp_Bound.left()
            fromY = self.mp_Bound.bottom()
            toX = fromX + mousePos.x() - self.mp_Pos.x()
            toY = fromY + mousePos.y() - self.mp_Pos.y()
            toX = snapF(toX, size, -4, snap)
            toY = snapF(toY, size, +4, snap)
            D.setX(toX - fromX)
            D.setY(toY - fromY)
            R.setLeft(toX)
            R.setBottom(toY)

            ## CLAMP SIZE
            if R.width() < mbrw:
                D.setX(D.x() - mbrw + R.width())
                R.setLeft(R.left() - mbrw + R.width())
            if R.height() < mbrh:
                D.setY(D.y() + mbrh - R.height())
                R.setBottom(R.bottom() + mbrh - R.height())

            newSideY = (R.height() - 4 * 2) / (1 + math.sqrt(2))
            newSideX = (R.width() - 4 * 2) / (1 + math.sqrt(2))
            newLeftRightBottomY = (R.y() + R.height() / 2) + newSideY / 2
            newLeftRightTopY = (R.y() + R.height() / 2) - newSideY / 2
            newTopBottomLeftX = (R.x() + R.width() / 2) - newSideX / 2
            newTopBottomRightX = (R.x() + R.width() / 2) + newSideX / 2
            
            selection.setLeft(R.left())
            selection.setBottom(R.bottom())
            
            background[self.IndexLT] = QtCore.QPointF(R.left(), newLeftRightTopY)
            background[self.IndexLB] = QtCore.QPointF(R.left(), newLeftRightBottomY)
            background[self.IndexRT] = QtCore.QPointF(R.right(), newLeftRightTopY)
            background[self.IndexRB] = QtCore.QPointF(R.right(), newLeftRightBottomY)
            background[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, R.top())
            background[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, R.top())
            background[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, R.bottom())
            background[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, R.bottom())
            background[self.IndexEE] = QtCore.QPointF(R.left(), newLeftRightTopY)
            
            polygon[self.IndexLT] = QtCore.QPointF(R.left() + 4, newLeftRightTopY)
            polygon[self.IndexLB] = QtCore.QPointF(R.left() + 4, newLeftRightBottomY)
            polygon[self.IndexRT] = QtCore.QPointF(R.right() - 4, newLeftRightTopY)
            polygon[self.IndexRB] = QtCore.QPointF(R.right() - 4, newLeftRightBottomY)
            polygon[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, R.top() + 4)
            polygon[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, R.top() + 4)
            polygon[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, R.bottom() - 4)
            polygon[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, R.bottom() - 4)
            polygon[self.IndexEE] = QtCore.QPointF(R.left() + 4, newLeftRightTopY)

        elif self.mp_Handle == self.HandleBM:

            fromY = self.mp_Bound.bottom()
            toY = fromY + mousePos.y() - self.mp_Pos.y()
            toY = snapF(toY, size, +4, snap)
            D.setY(toY - fromY)
            R.setBottom(toY)

            ## CLAMP SIZE
            if R.height() < mbrh:
                D.setY(D.y() + mbrh - R.height())
                R.setBottom(R.bottom() + mbrh - R.height())

            newSide = (R.height() - 4 * 2) / (1 + math.sqrt(2))
            newLeftRightTopY = (R.y() + R.height() / 2) - newSide / 2
            newLeftRightBottomY = (R.y() + R.height() / 2) + newSide / 2
            
            selection.setBottom(R.bottom())
            
            background[self.IndexBL] = QtCore.QPointF(background[self.IndexBL].x(), R.bottom())
            background[self.IndexBR] = QtCore.QPointF(background[self.IndexBR].x(), R.bottom())
            background[self.IndexLB] = QtCore.QPointF(background[self.IndexLB].x(), newLeftRightBottomY)
            background[self.IndexRB] = QtCore.QPointF(background[self.IndexRB].x(), newLeftRightBottomY)
            background[self.IndexLT] = QtCore.QPointF(background[self.IndexLT].x(), newLeftRightTopY)
            background[self.IndexRT] = QtCore.QPointF(background[self.IndexRT].x(), newLeftRightTopY)
            background[self.IndexEE] = QtCore.QPointF(background[self.IndexEE].x(), newLeftRightTopY)
            
            polygon[self.IndexBL] = QtCore.QPointF(polygon[self.IndexBL].x(), R.bottom() - 4)
            polygon[self.IndexBR] = QtCore.QPointF(polygon[self.IndexBR].x(), R.bottom() - 4)
            polygon[self.IndexLB] = QtCore.QPointF(polygon[self.IndexLB].x(), newLeftRightBottomY)
            polygon[self.IndexRB] = QtCore.QPointF(polygon[self.IndexRB].x(), newLeftRightBottomY)
            polygon[self.IndexLT] = QtCore.QPointF(polygon[self.IndexLT].x(), newLeftRightTopY)
            polygon[self.IndexRT] = QtCore.QPointF(polygon[self.IndexRT].x(), newLeftRightTopY)
            polygon[self.IndexEE] = QtCore.QPointF(polygon[self.IndexEE].x(), newLeftRightTopY)

        elif self.mp_Handle == self.HandleBR:

            fromX = self.mp_Bound.right()
            fromY = self.mp_Bound.bottom()
            toX = fromX + mousePos.x() - self.mp_Pos.x()
            toY = fromY + mousePos.y() - self.mp_Pos.y()
            toX = snapF(toX, size, +4, snap)
            toY = snapF(toY, size, +4, snap)
            D.setX(toX - fromX)
            D.setY(toY - fromY)
            R.setRight(toX)
            R.setBottom(toY)

            ## CLAMP SIZE
            if R.width() < mbrw:
                D.setX(D.x() + mbrw - R.width())
                R.setRight(R.right() + mbrw - R.width())
            if R.height() < mbrh:
                D.setY(D.y() + mbrh - R.height())
                R.setBottom(R.bottom() + mbrh - R.height())

            newSideY = (R.height() - 4 * 2) / (1 + math.sqrt(2))
            newSideX = (R.width() - 4 * 2) / (1 + math.sqrt(2))
            newLeftRightBottomY = (R.y() + R.height() / 2) + newSideY / 2
            newLeftRightTopY = (R.y() + R.height() / 2) - newSideY / 2
            newTopBottomLeftX = (R.x() + R.width() / 2) - newSideX / 2
            newTopBottomRightX = (R.x() + R.width() / 2) + newSideX / 2

            selection.setRight(R.right())
            selection.setBottom(R.bottom())

            background[self.IndexLT] = QtCore.QPointF(R.left(), newLeftRightTopY)
            background[self.IndexLB] = QtCore.QPointF(R.left(), newLeftRightBottomY)
            background[self.IndexRT] = QtCore.QPointF(R.right(), newLeftRightTopY)
            background[self.IndexRB] = QtCore.QPointF(R.right(), newLeftRightBottomY)
            background[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, R.top())
            background[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, R.top())
            background[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, R.bottom())
            background[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, R.bottom())
            background[self.IndexEE] = QtCore.QPointF(R.left(), newLeftRightTopY)
            
            polygon[self.IndexLT] = QtCore.QPointF(R.left() + 4, newLeftRightTopY)
            polygon[self.IndexLB] = QtCore.QPointF(R.left() + 4, newLeftRightBottomY)
            polygon[self.IndexRT] = QtCore.QPointF(R.right() - 4, newLeftRightTopY)
            polygon[self.IndexRB] = QtCore.QPointF(R.right() - 4, newLeftRightBottomY)
            polygon[self.IndexTL] = QtCore.QPointF(newTopBottomLeftX, R.top() + 4)
            polygon[self.IndexTR] = QtCore.QPointF(newTopBottomRightX, R.top() + 4)
            polygon[self.IndexBL] = QtCore.QPointF(newTopBottomLeftX, R.bottom() - 4)
            polygon[self.IndexBR] = QtCore.QPointF(newTopBottomRightX, R.bottom() - 4)
            polygon[self.IndexEE] = QtCore.QPointF(R.left() + 4, newLeftRightTopY)

        self.background.setGeometry(background)
        self.selection.setGeometry(selection)
        self.polygon.setGeometry(polygon)

        self.updateNode(selected=True, handle=self.mp_Handle, anchors=(self.mp_Data, D))
        self.updateTextPos(moved=moved)

    def setIdentity(self, identity):
        """
        Set the identity of the current node.
        :type identity: Identity
        """
        pass

    def setText(self, text):
        """
        Set the label text: will additionally block label editing if a literal is being.
        :type text: str
        """
        self.label.setEditable(RE_VALUE.match(text) is None)
        self.label.setText(text)
        self.label.setAlignment(QtCore.Qt.AlignCenter)

    def setTextPos(self, pos):
        """
        Set the label position.
        :type pos: QtCore.QPointF
        """
        self.label.setPos(pos)

    def shape(self):
        """
        Returns the shape of this item as a QtGui.QPainterPath in local coordinates.
        :rtype: QPainterPath
        """
        path = QtGui.QPainterPath()
        path.addPolygon(self.polygon.geometry())
        for polygon in self.handles:
            path.addEllipse(polygon.geometry())
        return path

    def text(self):
        """
        Returns the label text.
        :rtype: str
        """
        return self.label.text()

    def textPos(self):
        """
        Returns the current label position in item coordinates.
        :rtype: QPointF
        """
        return self.label.pos()

    def updateTextPos(self, *args, **kwargs):
        """
        Update the label position.
        """
        self.label.updatePos(*args, **kwargs)

    def width(self):
        """
        Returns the width of the shape.
        :rtype: int
        """
        polygon = self.polygon.geometry()
        return polygon[self.IndexRT].x() - polygon[self.IndexLT].x()