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


from PyQt5 import QtCore
from PyQt5 import QtGui
from PyQt5 import QtWidgets

from eddy.core.common import HasThreadingSystem, HasWidgetSystem
from eddy.core.output import getLogger
from eddy.core.functions.signals import connect
from eddy.core.datatypes.qt import Font
from eddy.ui.fields import CheckBox


LOGGER = getLogger()


class DiagramsSelectionDialog(QtWidgets.QDialog, HasThreadingSystem, HasWidgetSystem):
    """
        Extends QtWidgets.QDialog providing the form used to select the diagrams for a specific task like speficif export/import
    """

    def __init__(self, project, session):
        """
        Initialize the form dialog.
        :type project: Project
        :type session: Session
        """
        super().__init__(session)

        self.project = project

        #############################################
        # MAIN FORM AREA
        #################################

        confirmation = QtWidgets.QDialogButtonBox(QtCore.Qt.Horizontal, self)
        confirmation.addButton(QtWidgets.QDialogButtonBox.Ok)
        confirmation.addButton(QtWidgets.QDialogButtonBox.Cancel)
        confirmation.setFont(Font('Roboto', 12))
        confirmation.setObjectName('confirmation')
        connect(confirmation.accepted, self.run)
        connect(confirmation.rejected, self.reject)
        self.addWidget(confirmation)

        confirmationLayout = QtWidgets.QHBoxLayout()
        confirmationLayout.setContentsMargins(0, 0, 0, 0)
        confirmationLayout.addWidget(self.widget('normalization'), 0, QtCore.Qt.AlignLeft)
        confirmationLayout.addWidget(self.widget('exportRichText'), 0, QtCore.Qt.AlignLeft)
        confirmationLayout.addWidget(self.widget('confirmation'), 0, QtCore.Qt.AlignRight)
        #confirmationArea = QtWidgets.QWidget()
        #confirmationArea.setLayout(confirmationLayout)

        self.addWidget(QtWidgets.QPushButton('All', self,
                                             clicked=self.doCheckDiagramMarks,
                                             objectName='btn_check_all'))
        self.addWidget(QtWidgets.QPushButton('Clear', self,
                                             clicked=self.doCheckDiagramMarks,
                                             objectName='btn_clear_all'))

        self.diagrams_selected = []
        self.diagrams = self.project.diagrams()

        self.diagrams_list = self.sort(self.diagrams)

        for diagram in self.diagrams_list:
            self.addWidget(CheckBox(diagram.name, self,
                                    enabled=True, objectName=diagram.name,
                                    checked=True, clicked=self.onDiagramCheckClicked))

        DiagramNamesLayout = QtWidgets.QGridLayout()
        DiagramNamesLayout.setColumnMinimumWidth(0, 230)
        DiagramNamesLayout.setColumnMinimumWidth(1, 230)
        DiagramNamesLayout.setColumnMinimumWidth(2, 230)

        for i, d in enumerate(self.diagrams_list):
            #print(i,'-',d.name)
            DiagramNamesLayout.addWidget(self.widget(d.name), i, 0)

        ButtonsLayout = QtWidgets.QHBoxLayout()
        ButtonsLayout.setContentsMargins(0, 6, 0, 0)
        ButtonsLayout.setAlignment(QtCore.Qt.AlignRight)
        ButtonsLayout.addWidget(self.widget('btn_clear_all'), 0, QtCore.Qt.AlignRight)
        ButtonsLayout.addWidget(self.widget('btn_check_all'), 0, QtCore.Qt.AlignRight)

        DiagramsLayout = QtWidgets.QVBoxLayout()
        DiagramsLayout.addLayout(DiagramNamesLayout)
        DiagramsLayout.addLayout(ButtonsLayout)
        DiagramsLayout.addLayout(confirmationLayout)


        self.setLayout(DiagramsLayout)
        self.setFixedSize(400,300)
        self.setFont(Font('Roboto', 12))
        self.setWindowIcon(QtGui.QIcon(':/icons/128/ic_eddy'))
        self.setWindowTitle('Diagram selection')

    def sort(self, inp_diagrams):

        diagrams_list = []

        # sort the diagrams by name
        for diagram in inp_diagrams:

            i = 0
            while (i < (len(diagrams_list))):
                element = diagrams_list[i]
                if diagram.name < element.name:
                    break
                else:
                    i = i + 1
            diagrams_list.insert(i, diagram)

        return diagrams_list

    #############################################
    #   PROPERTIES
    #################################

    @property
    def session(self):
        """
        Returns the active session (alias for DiagramsSelectionDialog.parent()).
        :rtype: Session
        """
        return self.parent()

    #############################################
    #   SLOTS
    #################################

    @QtCore.pyqtSlot()
    def onDiagramCheckClicked(self):
        """
        Executed when an diagram checkbox is clicked.
        """
        self.widget('confirmation').setEnabled(
            any(x.isChecked() for x in (self.widget(d.name) for d in self.diagrams_list)))

    @QtCore.pyqtSlot()
    def doCheckDiagramMarks(self):
        """
        Check diagrams marks according to the action that triggered the slot.
        """
        checked = self.sender() is self.widget('btn_check_all')
        for diagram in self.diagrams_list:
            checkbox = self.widget(diagram.name)
            checkbox.setChecked(checked)
        self.widget('confirmation').setEnabled(checked)

    @QtCore.pyqtSlot()
    def run(self):

        LOGGER.info('Executing Diagrams selection dialog')

        self.widget('confirmation').setEnabled(False)
        self.widget('btn_clear_all').setEnabled(False)
        self.widget('btn_check_all').setEnabled(False)

        for diagram in self.diagrams_list:
            checkbox = self.widget(diagram.name)
            #print('checkbox.isEnabled()',checkbox.isEnabled(), ' checkbox.isChecked()-', checkbox.isChecked(), ' checkbox.text()-', checkbox.text(), ' diagram.name-', diagram.name)
            checkbox.setEnabled(False)
            if checkbox.isChecked():
                self.diagrams_selected.append(diagram)

        self.accept()