# -*- coding: utf-8 -*-

##########################################################################
#                                                                        #
#  Grapholed: a diagramming software for the Graphol language.           #
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
#  Dipartimento di Informatica e Sistemistica "A.Ruberti" at Sapienza    #
#  University of Rome: http://www.dis.uniroma1.it/~graphol/:             #
#                                                                        #
#     - Domenico Lembo <lembo@dis.uniroma1.it>                           #
#     - Marco Console <console@dis.uniroma1.it>                          #
#     - Valerio Santarelli <santarelli@dis.uniroma1.it>                  #
#     - Domenico Fabio Savo <savo@dis.uniroma1.it>                       #
#                                                                        #
##########################################################################


from grapholed.datatypes import Font
from grapholed.functions import snapPointToGrid
from grapholed.items import ItemType
from grapholed.items.nodes.common.base import ResizableNode
from grapholed.items.nodes.common.label import Label

from PyQt5.QtCore import QPointF, QRectF, Qt
from PyQt5.QtGui import QPolygonF, QPainterPath, QPainter, QPen, QColor, QPixmap


class ValueRestrictionNode(ResizableNode):
    """
    This class implements the 'Value-Restriction' node.
    """
    indexTR = 0
    indexTL = 1
    indexBL = 2
    indexBR = 3
    indexRT = 4
    indexEE = 5

    foldSize = 12
    itemtype = ItemType.ValueRestrictionNode
    minHeight = 60
    minWidth = 140
    name = 'value restriction'
    xmlname = 'value-restriction'

    def __init__(self, width=minWidth, height=minHeight, **kwargs):
        """
        Initialize the Value Restriction node.
        :param width: the shape width.
        :param height: the shape height.
        """
        super().__init__(**kwargs)
        self.polygon = self.createPolygon(max(width, self.minWidth), max(height, self.minHeight), self.foldSize)
        self.label = Label(self.name, parent=self)
        self.label.updatePos()
        self.updateHandlesPos()

    ################################################ ITEM INTERFACE ####################################################

    def contextMenu(self):
        """
        Returns the basic nodes context menu.
        :rtype: QMenu
        """
        menu = super().contextMenu()

        collection = self.label.contextMenuAdd()
        if collection:
            menu.addSeparator()
            for action in collection:
                menu.addAction(action)

        return menu

    def copy(self, scene):
        """
        Create a copy of the current item .
        :param scene: a reference to the scene where this item is being copied from.
        """
        node = self.__class__(scene=scene,
                              id=self.id,
                              description=self.description,
                              url=self.url,
                              width=self.width(),
                              height=self.height())

        node.setPos(self.pos())
        node.setLabelText(self.labelText())
        node.setLabelPos(node.mapFromScene(self.mapToScene(self.labelPos())))

        return node

    def height(self):
        """
        Returns the height of the shape.
        :rtype: int
        """
        return self.boundingRect().height() - 2 * (self.handleSize + self.handleSpace)

    def width(self):
        """
        Returns the width of the shape.
        :rtype: int
        """
        return self.boundingRect().width() - 2 * (self.handleSize + self.handleSpace)

    ############################################### AUXILIARY METHODS ##################################################

    @staticmethod
    def createPolygon(shape_w, shape_h, fold_size):
        """
        Returns the initialized polygon according to the given width/height.
        :param shape_w: the shape width.
        :param shape_h: the shape height.
        :param fold_size: the width of the fold.
        :rtype: QPolygonF
        """
        return QPolygonF([
            QPointF(+(shape_w / 2) - fold_size, -(shape_h / 2)),  # 0
            QPointF(-(shape_w / 2), -(shape_h / 2)),              # 1
            QPointF(-(shape_w / 2), +(shape_h / 2)),              # 2
            QPointF(+(shape_w / 2), +(shape_h / 2)),              # 3
            QPointF(+(shape_w / 2), -(shape_h / 2) + fold_size),  # 4
            QPointF(+(shape_w / 2) - fold_size, -(shape_h / 2)),  # 5
        ])

    @staticmethod
    def createFold(polygon, indexTR, indexRT, foldsize):
        """
        Returns the initialized fold polygon.
        :param polygon: the initialize shape polygon.
        :param indexTR: the top-right point index.
        :param indexRT: the right-top point index.
        :param foldsize: the width of the fold.
        :rtype: QPolygonF
        """
        return QPolygonF([
            QPointF(polygon[indexTR].x(), polygon[indexTR].y()),
            QPointF(polygon[indexTR].x(), polygon[indexTR].y() + foldsize),
            QPointF(polygon[indexRT].x(), polygon[indexRT].y()),
            QPointF(polygon[indexTR].x(), polygon[indexTR].y()),
        ])

    ################################################## ITEM EXPORT #####################################################

    def asGraphol(self, document):
        """
        Export the current item in Graphol format.
        :param document: the XML document where this item will be inserted.
        :rtype: QDomElement
        """
        pos1 = self.pos()
        pos2 = self.mapToScene(self.labelPos())

        # create the root element for this node
        node = document.createElement('node')
        node.setAttribute('id', self.id)
        node.setAttribute('type', self.xmlname)

        # add node attributes
        url = document.createElement('data:url')
        url.appendChild(document.createTextNode(self.url))
        description = document.createElement('data:description')
        description.appendChild(document.createTextNode(self.description))

        # add the shape geometry
        geometry = document.createElement('shape:geometry')
        geometry.setAttribute('height', self.height())
        geometry.setAttribute('width', self.width())
        geometry.setAttribute('x', pos1.x())
        geometry.setAttribute('y', pos1.y())

        # add the shape label
        label = document.createElement('shape:label')
        label.setAttribute('height', self.label.height())
        label.setAttribute('width', self.label.width())
        label.setAttribute('x', pos2.x())
        label.setAttribute('y', pos2.y())
        label.appendChild(document.createTextNode(self.label.text()))

        node.appendChild(url)
        node.appendChild(description)
        node.appendChild(geometry)
        node.appendChild(label)

        return node

    #################################################### GEOMETRY ######################################################

    def boundingRect(self):
        """
        Returns the shape bounding rectangle.
        :rtype: QRectF
        """
        o = self.handleSize + self.handleSpace
        x = self.polygon[self.indexTL].x()
        y = self.polygon[self.indexTL].y()
        w = self.polygon[self.indexBR].x() - x
        h = self.polygon[self.indexBL].y() - y
        return QRectF(x - o, y - o, w + o * 2, h + o * 2)

    def interactiveResize(self, mousePos):
        """
        Handle the interactive resize of the shape.
        :param mousePos: the current mouse position.
        """
        offset = self.handleSize + self.handleSpace
        scene = self.scene()
        rect = self.boundingRect()
        snap = scene.settings.value('scene/snap_to_grid', False, bool)
        fold = self.foldSize
        diff = QPointF(0, 0)

        minBoundingRectW = self.minWidth + offset * 2
        minBoundingRectH = self.minHeight + offset * 2

        self.prepareGeometryChange()

        if self.handleSelected == self.handleTL:

            fromX = self.mousePressRect.left()
            fromY = self.mousePressRect.top()
            toX = fromX + mousePos.x() - self.mousePressPos.x()
            toY = fromY + mousePos.y() - self.mousePressPos.y()
            toX = snapPointToGrid(toX, scene.GridSize, -offset, snap)
            toY = snapPointToGrid(toY, scene.GridSize, -offset, snap)
            diff.setX(toX - fromX)
            diff.setY(toY - fromY)
            rect.setLeft(toX)
            rect.setTop(toY)

            ## CLAMP SIZE
            if rect.width() < minBoundingRectW:
                diff.setX(diff.x() - minBoundingRectW + rect.width())
                rect.setLeft(rect.left() - minBoundingRectW + rect.width())
            if rect.height() < minBoundingRectH:
                diff.setY(diff.y() - minBoundingRectH + rect.height())
                rect.setTop(rect.top() - minBoundingRectH + rect.height())

            self.polygon[self.indexTL] = QPointF(rect.left() + offset, rect.top() + offset)
            self.polygon[self.indexBL] = QPointF(rect.left() + offset, self.polygon[self.indexBL].y())
            self.polygon[self.indexTR] = QPointF(self.polygon[self.indexTR].x(), rect.top() + offset)
            self.polygon[self.indexRT] = QPointF(self.polygon[self.indexRT].x(), rect.top() + offset + fold)
            self.polygon[self.indexEE] = QPointF(self.polygon[self.indexEE].x(), rect.top() + offset)

        elif self.handleSelected == self.handleTM:

            fromY = self.mousePressRect.top()
            toY = fromY + mousePos.y() - self.mousePressPos.y()
            toY = snapPointToGrid(toY, scene.GridSize, -offset, snap)
            diff.setY(toY - fromY)
            rect.setTop(toY)

            ## CLAMP SIZE
            if rect.height() < minBoundingRectH:
                diff.setY(diff.y() - minBoundingRectH + rect.height())
                rect.setTop(rect.top() - minBoundingRectH + rect.height())

            self.polygon[self.indexTL] = QPointF(self.polygon[self.indexTL].x(), rect.top() + offset)
            self.polygon[self.indexTR] = QPointF(self.polygon[self.indexTR].x(), rect.top() + offset)
            self.polygon[self.indexRT] = QPointF(self.polygon[self.indexRT].x(), rect.top() + offset + fold)
            self.polygon[self.indexEE] = QPointF(self.polygon[self.indexEE].x(), rect.top() + offset)

        elif self.handleSelected == self.handleTR:

            fromX = self.mousePressRect.right()
            fromY = self.mousePressRect.top()
            toX = fromX + mousePos.x() - self.mousePressPos.x()
            toY = fromY + mousePos.y() - self.mousePressPos.y()
            toX = snapPointToGrid(toX, scene.GridSize, +offset, snap)
            toY = snapPointToGrid(toY, scene.GridSize, -offset, snap)
            diff.setX(toX - fromX)
            diff.setY(toY - fromY)
            rect.setRight(toX)
            rect.setTop(toY)

            ## CLAMP SIZE
            if rect.width() < minBoundingRectW:
                diff.setX(diff.x() + minBoundingRectW - rect.width())
                rect.setRight(rect.right() + minBoundingRectW - rect.width())
            if rect.height() < minBoundingRectH:
                diff.setY(diff.y() - minBoundingRectH + rect.height())
                rect.setTop(rect.top() - minBoundingRectH + rect.height())

            self.polygon[self.indexTL] = QPointF(self.polygon[self.indexTL].x(), rect.top() + offset)
            self.polygon[self.indexTR] = QPointF(rect.right() + offset - fold, rect.top() + offset)
            self.polygon[self.indexRT] = QPointF(rect.right() + offset, rect.top() + offset + fold)
            self.polygon[self.indexBR] = QPointF(rect.right() + offset, self.polygon[self.indexBR].y())
            self.polygon[self.indexEE] = QPointF(rect.right() + offset - fold, rect.top() + offset)

        elif self.handleSelected == self.handleML:

            fromX = self.mousePressRect.left()
            toX = fromX + mousePos.x() - self.mousePressPos.x()
            toX = snapPointToGrid(toX, scene.GridSize, -offset, snap)
            diff.setX(toX - fromX)
            rect.setLeft(toX)

            ## CLAMP SIZE
            if rect.width() < minBoundingRectW:
                diff.setX(diff.x() - minBoundingRectW + rect.width())
                rect.setLeft(rect.left() - minBoundingRectW + rect.width())

            self.polygon[self.indexTL] = QPointF(rect.left() + offset, self.polygon[self.indexTL].y())
            self.polygon[self.indexBL] = QPointF(rect.left() + offset, self.polygon[self.indexBL].y())

        elif self.handleSelected == self.handleMR:

            fromX = self.mousePressRect.right()
            toX = fromX + mousePos.x() - self.mousePressPos.x()
            toX = snapPointToGrid(toX, scene.GridSize, +offset, snap)
            diff.setX(toX - fromX)
            rect.setRight(toX)

            ## CLAMP SIZE
            if rect.width() < minBoundingRectW:
                diff.setX(diff.x() + minBoundingRectW - rect.width())
                rect.setRight(rect.right() + minBoundingRectW - rect.width())

            self.polygon[self.indexTR] = QPointF(rect.right() + offset - fold, self.polygon[self.indexTR].y())
            self.polygon[self.indexRT] = QPointF(rect.right() + offset, self.polygon[self.indexRT].y())
            self.polygon[self.indexBR] = QPointF(rect.right() + offset, self.polygon[self.indexBR].y())
            self.polygon[self.indexEE] = QPointF(rect.right() + offset - fold, self.polygon[self.indexEE].y())

        elif self.handleSelected == self.handleBL:

            fromX = self.mousePressRect.left()
            fromY = self.mousePressRect.bottom()
            toX = fromX + mousePos.x() - self.mousePressPos.x()
            toY = fromY + mousePos.y() - self.mousePressPos.y()
            toX = snapPointToGrid(toX, scene.GridSize, -offset, snap)
            toY = snapPointToGrid(toY, scene.GridSize, +offset, snap)
            diff.setX(toX - fromX)
            diff.setY(toY - fromY)
            rect.setLeft(toX)
            rect.setBottom(toY)

            ## CLAMP SIZE
            if rect.width() < minBoundingRectW:
                diff.setX(diff.x() - minBoundingRectW + rect.width())
                rect.setLeft(rect.left() - minBoundingRectW + rect.width())
            if rect.height() < minBoundingRectH:
                diff.setY(diff.y() + minBoundingRectH - rect.height())
                rect.setBottom(rect.bottom() + minBoundingRectH - rect.height())

            self.polygon[self.indexTL] = QPointF(rect.left() + offset, self.polygon[self.indexTL].y())
            self.polygon[self.indexBL] = QPointF(rect.left() + offset, rect.bottom() + offset)
            self.polygon[self.indexBR] = QPointF(self.polygon[self.indexBR].x(), rect.bottom() + offset)

        elif self.handleSelected == self.handleBM:

            fromY = self.mousePressRect.bottom()
            toY = fromY + mousePos.y() - self.mousePressPos.y()
            toY = snapPointToGrid(toY, scene.GridSize, +offset, snap)
            diff.setY(toY - fromY)
            rect.setBottom(toY)

            ## CLAMP SIZE
            if rect.height() < minBoundingRectH:
                diff.setY(diff.y() + minBoundingRectH - rect.height())
                rect.setBottom(rect.bottom() + minBoundingRectH - rect.height())

            self.polygon[self.indexBL] = QPointF(self.polygon[self.indexBL].x(), rect.bottom() + offset)
            self.polygon[self.indexBR] = QPointF(self.polygon[self.indexBR].x(), rect.bottom() + offset)

        elif self.handleSelected == self.handleBR:

            fromX = self.mousePressRect.right()
            fromY = self.mousePressRect.bottom()
            toX = fromX + mousePos.x() - self.mousePressPos.x()
            toY = fromY + mousePos.y() - self.mousePressPos.y()
            toX = snapPointToGrid(toX, scene.GridSize, +offset, snap)
            toY = snapPointToGrid(toY, scene.GridSize, +offset, snap)
            diff.setX(toX - fromX)
            diff.setY(toY - fromY)
            rect.setRight(toX)
            rect.setBottom(toY)

            ## CLAMP SIZE
            if rect.width() < minBoundingRectW:
                diff.setX(diff.x() + minBoundingRectW - rect.width())
                rect.setRight(rect.right() + minBoundingRectW - rect.width())
            if rect.height() < minBoundingRectH:
                diff.setY(diff.y() + minBoundingRectH - rect.height())
                rect.setBottom(rect.bottom() + minBoundingRectH - rect.height())

            self.polygon[self.indexBL] = QPointF(self.polygon[self.indexBL].x(), rect.bottom() + offset)
            self.polygon[self.indexBR] = QPointF(rect.right() + offset, rect.bottom() + offset)
            self.polygon[self.indexTR] = QPointF(rect.right() + offset - fold, self.polygon[self.indexTR].y())
            self.polygon[self.indexRT] = QPointF(rect.right() + offset, self.polygon[self.indexRT].y())
            self.polygon[self.indexEE] = QPointF(rect.right() + offset - fold, self.polygon[self.indexEE].y())

        self.updateHandlesPos()
        self.updateLabelPos()

        # update edge anchors
        for edge, pos in self.mousePressData.items():
            self.setAnchor(edge, pos + diff * 0.5)

    def painterPath(self):
        """
        Returns the current shape as QPainterPath (used for collision detection).
        :rtype: QPainterPath
        """
        path = QPainterPath()
        path.addPolygon(self.polygon)
        return path

    def shape(self):
        """
        Returns the shape of this item as a QPainterPath in local coordinates.
        :rtype: QPainterPath
        """
        path = QPainterPath()
        path.addPolygon(self.polygon)

        if self.isSelected():
            for shape in self.handles.values():
                path.addEllipse(shape)

        return path

    ################################################# LABEL SHORTCUTS ##################################################

    def labelPos(self):
        """
        Returns the current label position in item coordinates.
        :rtype: QPointF
        """
        return self.label.pos()

    def labelText(self):
        """
        Returns the label text.
        :rtype: str
        """
        return self.label.text()

    def setLabelPos(self, pos):
        """
        Set the label position.
        :param pos: the node position in item coordinates.
        """
        self.label.setPos(pos)

    def setLabelText(self, text):
        """
        Set the label text.
        :param text: the text value to set.
        """
        self.label.setText(text)

    def updateLabelPos(self):
        """
        Update the label position.
        """
        self.label.updatePos()

    ################################################## ITEM DRAWING ####################################################

    def paint(self, painter, option, widget=None):
        """
        Paint the node in the graphic view.
        :param painter: the active painter.
        :param option: the style option for this item.
        :param widget: the widget that is being painted on.
        """
        shapeBrush = self.shapeBrushSelected if self.isSelected() else self.shapeBrush

        # Draw the polygon
        painter.setRenderHint(QPainter.Antialiasing)
        painter.setBrush(shapeBrush)
        painter.setPen(self.shapePen)
        painter.drawPolygon(self.polygon)

        # Draw the fold
        painter.setRenderHint(QPainter.Antialiasing)
        painter.setBrush(shapeBrush)
        painter.setPen(self.shapePen)
        painter.drawPolygon(self.createFold(self.polygon, self.indexTR, self.indexRT, self.foldSize))

        self.paintHandles(painter)

    @classmethod
    def image(cls, **kwargs):
        """
        Returns an image suitable for the palette.
        :rtype: QPixmap
        """
        shape_w = 54
        shape_h = 34
        foldsize = 10

        # Initialize the pixmap
        pixmap = QPixmap(kwargs['w'], kwargs['h'])
        pixmap.fill(Qt.transparent)
        painter = QPainter(pixmap)

        # Initialize the shape
        polygon = cls.createPolygon(shape_w, shape_h, foldsize)

        # Initialize the fold
        fold = cls.createFold(polygon, cls.indexTR, cls.indexRT, foldsize)

        # Draw the polygon
        painter.setPen(QPen(QColor(0, 0, 0), 1.0, Qt.SolidLine))
        painter.setBrush(QColor(252, 252, 252))
        painter.translate(kwargs['w'] / 2, kwargs['h'] / 2)
        painter.drawPolygon(polygon)
        painter.drawPolygon(fold)

        # Draw the text within the rectangle
        painter.setFont(Font('Arial', 10, Font.Light))
        painter.drawText(polygon.boundingRect(), Qt.AlignCenter, 'value\nrestriction')

        return pixmap