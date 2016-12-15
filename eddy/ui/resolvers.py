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


from PyQt5 import QtCore
from PyQt5 import QtGui
from PyQt5 import QtWidgets

from eddy.core.common import HasWidgetSystem
from eddy.core.datatypes.qt import Font, PHCQPushButton
from eddy.core.functions.misc import isEmpty
from eddy.core.functions.signals import connect

from eddy.ui.fields import TextField


class PredicateDocumentationConflictResolver(QtWidgets.QDialog, HasWidgetSystem):
    """
    This class is used to resolve conflicts generated by the different documentation of the same predicate.
    """

    def __init__(self, item, name, current, importing, parent=None):
        """
        Initialize the project dialog.
        :type item: Item
        :type name: str
        :type current: str
        :type importing: str
        :type parent: QWidget
        """
        super().__init__(parent)

        self.item = item
        self.name = name

        #############################################
        # LEFT SIDE
        #################################

        widget = QtWidgets.QLabel(self)
        widget.setAlignment(QtCore.Qt.AlignCenter)
        widget.setFont(Font('Roboto', 14, bold=True, capitalization=QtGui.QFont.AllUppercase))
        widget.setObjectName('current_title')
        widget.setText('Current')
        self.addWidget(widget)

        widget = TextField(self)
        widget.setFixedSize(300, 200)
        widget.setFocusPolicy(QtCore.Qt.NoFocus)
        widget.setFont(Font('Roboto', 12))
        widget.setObjectName('current_documentation')
        widget.setReadOnly(True)
        widget.setValue(current)
        self.addWidget(widget)

        widget = PHCQPushButton(self)
        widget.setAutoDefault(False)
        widget.setDefault(False)
        widget.setProperty('class', 'flat blue')
        widget.setFixedWidth(32)
        widget.setIcon(QtGui.QIcon(':/icons/24/ic_keyboard_arrow_right_black'))
        widget.setIconSize(QtCore.QSize(24, 24))
        widget.setObjectName('pick_current_button')
        widget.setSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Expanding)
        connect(widget.clicked, self.doPickDocumentation)
        self.addWidget(widget)

        widget = QtWidgets.QLabel(self)
        widget.setFixedSize(QtCore.QSize(24, 24))
        widget.setPixmap(QtGui.QIcon(':/icons/24/ic_compare_arrows_black').pixmap(24))
        widget.setObjectName('compare_arrows_current_icon')
        self.addWidget(widget)

        #############################################
        # MIDDLE SIDE
        #################################

        widget = QtWidgets.QLabel(self)
        widget.setAlignment(QtCore.Qt.AlignCenter)
        widget.setFont(Font('Roboto', 14, bold=True, capitalization=QtGui.QFont.AllUppercase))
        widget.setObjectName('final_title')
        widget.setText('Final')
        self.addWidget(widget)

        widget = TextField(self)
        widget.setFixedSize(300, 200)
        widget.setFont(Font('Roboto', 12))
        widget.setObjectName('final_documentation')
        self.addWidget(widget)

        #############################################
        # RIGHT SIDE
        #################################

        widget = QtWidgets.QLabel(self)
        widget.setAlignment(QtCore.Qt.AlignCenter)
        widget.setFont(Font('Roboto', 14, bold=True, capitalization=QtGui.QFont.AllUppercase))
        widget.setObjectName('importing_title')
        widget.setText('Importing')
        self.addWidget(widget)

        widget = TextField(self)
        widget.setFixedSize(300, 200)
        widget.setFocusPolicy(QtCore.Qt.NoFocus)
        widget.setFont(Font('Roboto', 12))
        widget.setObjectName('importing_documentation')
        widget.setReadOnly(True)
        widget.setValue(importing)
        self.addWidget(widget)

        widget = PHCQPushButton(self)
        widget.setAutoDefault(False)
        widget.setDefault(False)
        widget.setProperty('class', 'flat blue')
        widget.setFixedWidth(32)
        widget.setIcon(QtGui.QIcon(':/icons/24/ic_keyboard_arrow_left_black'))
        widget.setIconSize(QtCore.QSize(24, 24))
        widget.setObjectName('pick_importing_button')
        widget.setSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Expanding)
        connect(widget.clicked, self.doPickDocumentation)
        self.addWidget(widget)

        widget = QtWidgets.QLabel(self)
        widget.setFixedSize(QtCore.QSize(24, 24))
        widget.setPixmap(QtGui.QIcon(':/icons/24/ic_compare_arrows_black').pixmap(24))
        widget.setObjectName('compare_arrows_importing_icon')
        self.addWidget(widget)

        #############################################
        # CONFIRMATION AREA
        #################################

        widget = QtWidgets.QDialogButtonBox(QtCore.Qt.Horizontal, self)
        widget.addButton(QtWidgets.QDialogButtonBox.Ok)
        widget.addButton(QtWidgets.QDialogButtonBox.Abort)
        widget.setContentsMargins(0, 4, 0, 0)
        widget.setFont(Font('Roboto', 12))
        widget.setObjectName('confirmation_box')
        connect(widget.accepted, self.accept)
        connect(widget.rejected, self.reject)
        self.addWidget(widget)

        #############################################
        # SETUP DIALOG LAYOUT
        #################################

        gridWidget = QtWidgets.QWidget(self)
        gridLayout = QtWidgets.QGridLayout(gridWidget)
        gridLayout.setContentsMargins(0, 0, 0, 0)
        gridLayout.addWidget(self.widget('current_title'), 0, 0)
        gridLayout.addWidget(self.widget('current_documentation'), 1, 0)
        gridLayout.addWidget(self.widget('final_title'), 0, 2)
        gridLayout.addWidget(self.widget('final_documentation'), 1, 2)
        gridLayout.addWidget(self.widget('importing_title'), 0, 4)
        gridLayout.addWidget(self.widget('importing_documentation'), 1, 4)
        gridLayout.addWidget(self.widget('compare_arrows_current_icon'), 0, 1, QtCore.Qt.AlignCenter)
        gridLayout.addWidget(self.widget('compare_arrows_importing_icon'), 0, 3, QtCore.Qt.AlignCenter)
        gridLayout.addWidget(self.widget('pick_current_button'), 1, 1)
        gridLayout.addWidget(self.widget('pick_importing_button'), 1, 3)

        mainLayout = QtWidgets.QVBoxLayout()
        mainLayout.addWidget(gridWidget)
        mainLayout.addWidget(self.widget('confirmation_box'))
        mainLayout.setContentsMargins(10, 10, 10, 10)

        self.setLayout(mainLayout)
        self.setFixedSize(self.sizeHint())
        self.setFont(Font('Roboto', 12))
        self.setWindowIcon(QtGui.QIcon(':/icons/128/ic_eddy'))
        self.setWindowTitle("Resolve documentation conflict for {0} '{1}'...".format(self.item.shortName, self.name))

    #############################################
    #   INTERFACE
    #################################

    def result(self):
        """
        Returns the chosen documentation string.
        :rtype: str
        """
        return self.widget('final_documentation').value()

    #############################################
    #   SLOTS
    #################################

    @QtCore.pyqtSlot()
    def accept(self):
        """
        Accepts the conflict resolution form.
        """
        source = self.widget('final_documentation')
        if isEmpty(source.value()):
            msgbox = QtWidgets.QMessageBox(self)
            msgbox.setIconPixmap(QtGui.QIcon(':/icons/48/ic_warning_black').pixmap(48))
            msgbox.setStandardButtons(QtWidgets.QMessageBox.No | QtWidgets.QMessageBox.Yes)
            msgbox.setText("No documentation specified for {0} '{1}'. "
                           "Do you want to continue?".format(self.item.shortName, self.name))
            msgbox.setWindowIcon(QtGui.QIcon(':/icons/128/ic_eddy'))
            msgbox.setWindowTitle('No documentation specified!')
            msgbox.exec_()
            if msgbox.result() == QtWidgets.QMessageBox.No:
                return
        super().accept()

    @QtCore.pyqtSlot()
    def doPickDocumentation(self):
        """
        Executed when a pick button is clicked.
        """
        source = self.widget('current_documentation')
        if self.sender() is self.widget('pick_importing_button'):
            source = self.widget('importing_documentation')
        target = self.widget('final_documentation')
        target.setValue(source.value())