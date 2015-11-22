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


from PyQt5.QtCore import Qt
from PyQt5.QtGui import QPixmap, QIcon
from PyQt5.QtWidgets import QDialog, QFormLayout, QDialogButtonBox, QMessageBox

from eddy.fields import IntEditField
from eddy.functions import isEmpty, connect


class CardinalityRestrictionForm(QDialog):
    """
    This class implements the form used to input domain/range restriction cardinalities.
    """
    def __init__(self, parent=None):
        """
        Initialize the form dialog.
        :param parent: the parent widget.
        """
        super().__init__(parent)
        self.minCardinalityValue = None
        self.maxCardinalityValue = None
        self.minCardinalityField = IntEditField(self)
        self.maxCardinalityField = IntEditField(self)
        self.minCardinalityField.setFixedWidth(80)
        self.maxCardinalityField.setFixedWidth(80)
        self.buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        self.mainLayout = QFormLayout(self)
        self.mainLayout.addRow('Min. cardinality', self.minCardinalityField)
        self.mainLayout.addRow('Max. cardinality', self.maxCardinalityField)
        self.mainLayout.addRow(self.buttonBox)
        self.setWindowTitle('Insert cardinality')
        self.setWindowIcon(QIcon(':/images/eddy'))
        self.setFixedSize(self.sizeHint())

        connect(self.buttonBox.accepted, self.validate)
        connect(self.buttonBox.rejected, self.reject)

    def validate(self):
        """
        Validate the form and trigger accept() if the form is valid.
        """
        if not isEmpty(self.minCardinalityField.text()) and not isEmpty(self.maxCardinalityField.text()):

            v1 = int(self.minCardinalityField.text())
            v2 = int(self.maxCardinalityField.text())

            if v1 > v2:
                msgbox = QMessageBox(self)
                msgbox.setIconPixmap(QPixmap(':/icons/warning'))
                msgbox.setWindowIcon(QIcon(':/images/eddy'))
                msgbox.setWindowTitle('Invalid range specified')
                msgbox.setText('Minimum cardinality {0} must be lower or equal than Maximum cardinality {1}'.format(v1, v2))
                msgbox.setStandardButtons(QMessageBox.Ok)
                msgbox.exec_()
                return

        if not isEmpty(self.minCardinalityField.text()):
            self.minCardinalityValue = int(self.minCardinalityField.text())
        if not isEmpty(self.maxCardinalityField.text()):
            self.maxCardinalityValue = int(self.maxCardinalityField.text())

        self.accept()