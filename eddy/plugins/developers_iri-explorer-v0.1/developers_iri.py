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

from eddy.core.commands.nodes_2 import CommandProjetSetIRIPrefixesNodesDict
from eddy.core.datatypes.owl import OWLStandardIRIPrefixPairsDict
from eddy.core.datatypes.qt import BrushIcon, Font
from eddy.core.functions.misc import first, clamp, isEmpty
from eddy.core.functions.signals import connect, disconnect
from eddy.core.plugin import AbstractPlugin
from eddy.core.output import getLogger

from eddy.ui.dock import DockWidget
from eddy.ui.fields import IntegerField, StringField

import sys

LOGGER = getLogger()


class DevelopersIriPlugin(AbstractPlugin):
    """
    This plugin provides the Prefix-IRI widget.
    """
    #############################################
    #   EVENTS
    #################################

    def eventFilter(self, source, event):
        """
        Filters events if this object has been installed as an event filter for the watched object.
        :type source: QObject
        :type event: QtCore.QEvent
        :rtype: bool
        """
        if event.type() == QtCore.QEvent.Resize:
            widget = source.widget()
            widget.redraw()
        return super().eventFilter(source, event)

    #############################################
    #   SLOTS
    #################################

    @QtCore.pyqtSlot()
    def onSessionReady(self):

        widget = self.widget('developers_iri')

        connect(self.project.sgnIRIPrefixNodeDictionaryUpdated, widget.FillTableWithIRIPrefixNodesDictionaryKeysAndValues)

        connect(self.project.sgnIRIPrefixesEntryModified, widget.entry_MODIFY_ok)
        connect(self.project.sgnIRIPrefixEntryAdded, widget.entry_ADD_ok)
        connect(self.project.sgnIRIPrefixEntryRemoved, widget.entry_REMOVE_OK)
        connect(self.project.sgnIRIPrefixesEntryIgnored, widget.entry_NOT_OK)

        widget.run()

    @QtCore.pyqtSlot(QtWidgets.QMdiSubWindow)
    def onSubWindowActivated(self, subwindow):
        """
        Executed when the active subwindow changes.
        :type subwindow: MdiSubWindow
        """
        pass

    #############################################
    #   HOOKS
    #################################

    def dispose(self):
        """
        Executed whenever the plugin is going to be destroyed.
        """
        # DISCONNECT FROM ACTIVE SESSION
        self.debug('Disconnecting from active session')
        disconnect(self.session.sgnReady, self.onSessionReady)
        disconnect(self.session.mdi.subWindowActivated, self.onSubWindowActivated)


        # REMOVE DOCKING AREA WIDGET MENU ENTRY
        self.debug('Removing docking area widget toggle from "view" menu')
        menu = self.session.menu('view')
        menu.removeAction(self.widget('iri_dock').toggleViewAction())

        # UNINSTALL THE PALETTE DOCK WIDGET
        self.debug('Uninstalling docking area widget')
        self.session.removeDockWidget(self.widget('iri_dock'))

    def start(self):
        """
        Perform initialization tasks for the plugin.
        """
        # INITIALIZE THE WIDGET
        self.debug('Creating iri widget')
        widget = IriWidget(self)
        widget.setObjectName('developers_iri')
        self.addWidget(widget)

        # CREATE DOCKING AREA WIDGET
        self.debug('Creating docking area widget')
        widget = DockWidget('developers_Prefix-Iri Explorer', QtGui.QIcon(':/icons/18/ic_info_outline_black'), self.session)
        widget.installEventFilter(self)
        widget.setAllowedAreas(QtCore.Qt.LeftDockWidgetArea | QtCore.Qt.RightDockWidgetArea)
        widget.setObjectName('iri_dock')
        widget.setWidget(self.widget('developers_iri'))
        self.addWidget(widget)

        # CREATE ENTRY IN VIEW MENU
        self.debug('Creating docking area widget toggle in "view" menu')
        menu = self.session.menu('view')
        menu.addAction(self.widget('iri_dock').toggleViewAction())

        # INSTALL DOCKING AREA WIDGET
        self.debug('Installing docking area widget')
        self.session.addDockWidget(QtCore.Qt.RightDockWidgetArea, self.widget('iri_dock'))

        # CONFIGURE SIGNAL/SLOTS
        self.debug('Connecting to active session')
        connect(self.session.sgnReady, self.onSessionReady)
        connect(self.session.mdi.subWindowActivated, self.onSubWindowActivated)


class Header(QtWidgets.QLabel):
    """
    This class implements the header of properties section.
    """
    def __init__(self, *args):
        """
        Initialize the header.
        """
        super().__init__(*args)
        self.setAlignment(QtCore.Qt.AlignCenter)
        self.setFixedHeight(24)
        self.setFont(Font('Roboto', 12))


class IriWidget(QtWidgets.QScrollArea):
    """
    This class implements the information box widget.
    """

    def __init__(self, plugin):
        """
        Initialize the info box.
        :type plugin: Info
        """
        super().__init__(plugin.session)

        self.plugin = plugin

        self.mainLayout = QtWidgets.QVBoxLayout(self)
        self.mainLayout.setAlignment(QtCore.Qt.AlignTop)
        self.mainLayout.setContentsMargins(0, 0, 0, 0)
        self.mainLayout.setSpacing(0)

        #############

        self.tableheader_prefixes = Header('Prefix', self)
        self.tableheader_iri = Header(' IRI  ', self)
        self.tableheader_nodes = Header('Nodes ', self)

        """
        self.horizontalbox = QtWidgets.QHBoxLayout(self)   #to be added to main layout
        self.horizontalbox.setAlignment(QtCore.Qt.AlignTop)
        self.horizontalbox.setContentsMargins(0, 0, 0, 0)
        self.horizontalbox.setSpacing(0)
        self.horizontalbox.addWidget(self.tableheader_iri)
        self.horizontalbox.addWidget(self.tableheader_prefixes)
        self.horizontalbox.addWidget(self.tableheader_nodes)
        """
        #############

        self.entry_status = QtWidgets.QStatusBar()

        """
        self.slider = QtWidgets.QSlider()

        self.slider.setCursor(QtGui.QCursor())
        self.slider.setEnabled(True)
        self.slider.setRange(1,100)
        self.slider.setValue(12)
        self.slider.setTickPosition(QtWidgets.QSlider.TicksBothSides)
        self.slider.setTracking(True)
        self.slider.setTickInterval(1)
        self.slider.setMouseTracking(True)
        self.slider.setTracking(True)
        """
        """
        self.entry_button = QtWidgets.QPushButton()
        self.entry_button.setText('+++')
        self.remove_entry_button = QtWidgets.QPushButton()
        self.remove_entry_button.setText('---')
        self.modify_entry_button = QtWidgets.QPushButton()
        self.modify_entry_button.setText('M')
        """
        self.test_IRI_button = QtWidgets.QPushButton()
        self.test_IRI_button.setText('T')
        self.dictionary_display_button = QtWidgets.QPushButton()
        self.dictionary_display_button.setText('D')
        self.hide_or_show_nodes_button = QtWidgets.QPushButton()
        self.hide_or_show_nodes_button.setText('*')

        self.buttons_layout = QtWidgets.QHBoxLayout(self)
        self.buttons_layout.setAlignment(QtCore.Qt.AlignTop)
        self.buttons_layout.setContentsMargins(0, 0, 0, 0)
        self.buttons_layout.setSpacing(0)
        #self.buttons_layout.addWidget(self.entry_button)
        #self.buttons_layout.addWidget(self.remove_entry_button)
        #self.buttons_layout.addWidget(self.modify_entry_button)
        #self.buttons_layout.addWidget(self.test_IRI_button)
        self.buttons_layout.addWidget(self.dictionary_display_button)
        self.buttons_layout.addWidget(self.hide_or_show_nodes_button)

        #connect(self.entry_button.pressed, self.button_add)
        #connect(self.remove_entry_button.pressed, self.button_remove)
        connect(self.dictionary_display_button.pressed, self.display_IRIPrefixesNodesDict)
        connect(self.hide_or_show_nodes_button.pressed, self.hide_or_show_nodes)
        connect(self.test_IRI_button.pressed, self.test_IRI)
        #connect(self.modify_entry_button.pressed, self.process_entry_from_textboxes_for_button_modify)

        #connect(self.slider.sliderMoved, self.slider_moved)

        self.prefix_input_box = StringField(self)
        self.prefix_input_box.setPlaceholderText('Enter Prefix')
        self.prefix_input_box.setAcceptDrops(False)
        self.prefix_input_box.setClearButtonEnabled(True)
        self.prefix_input_box.setFixedHeight(30)

        self.iri_input_box = StringField(self)
        self.iri_input_box.setPlaceholderText('Enter IRI')
        self.iri_input_box.setAcceptDrops(False)
        self.iri_input_box.setClearButtonEnabled(True)
        self.iri_input_box.setFixedHeight(30)

        self.verticalbox = QtWidgets.QVBoxLayout(self)  # to be added to main layout
        self.verticalbox.setAlignment(QtCore.Qt.AlignTop)
        self.verticalbox.setContentsMargins(0, 0, 0, 0)
        self.verticalbox.setSpacing(0)
        #self.verticalbox.addWidget(self.iri_input_box)
        #self.verticalbox.addWidget(self.prefix_input_box)
        self.verticalbox.addLayout(self.buttons_layout)
        #self.verticalbox.addWidget(self.entry_button)
        #self.verticalbox.addWidget(self.remove_entry_button)
        #self.verticalbox.addWidget(self.dictionary_display_button)
        #self.verticalbox.addWidget(self.slider)
        self.verticalbox.addWidget(self.entry_status)

        #############

        self.table = QtWidgets.QTableWidget(self)
        self.table.setContentsMargins(0, 0, 0, 0)
        self.table.horizontalHeader().setVisible(False)
        self.table.verticalHeader().setVisible(False)
        self.table.setMinimumWidth(self.width())
        self.table.setMinimumHeight(self.height()-self.dictionary_display_button.height())

        connect(self.table.cellPressed, self.try_to_edit_cell)

        self.horizontalbox_3 = QtWidgets.QHBoxLayout(self)    #to be added to main layout
        self.horizontalbox_3.setAlignment(QtCore.Qt.AlignTop)
        self.horizontalbox_3.setContentsMargins(0, 0, 0, 0)
        self.horizontalbox_3.setSpacing(0)
        self.horizontalbox_3.addWidget(self.table)

        #############

        self.mainLayout.addLayout(self.verticalbox)
        #self.mainLayout.addLayout(self.horizontalbox)
        self.mainLayout.addLayout(self.horizontalbox_3)

        #############

        self.setContentsMargins(0, 0, 0, 0)
        self.setMinimumSize(QtCore.QSize(216, 120))
        self.setHorizontalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(QtCore.Qt.ScrollBarAsNeeded)
        self.setWidgetResizable(True)

        self.setStyleSheet("""
        IriWidget {
          background: #FFFFFF;
        }
        IriWidget Header {
          background: #5A5050;
          padding-left: 4px;
          color: #FFFFFF;
        }
        """)

        scrollbar = self.verticalScrollBar()
        scrollbar.installEventFilter(self)

        self.ENTRY_MODIFY_OK_var = set()
        self.ENTRY_REMOVE_OK_var = set()
        self.ENTRY_ADD_OK_var = set()
        self.ENTRY_IGNORE_var = set()

        self.ADD_OR_REMOVE = None

        self.SHOW_NODES = True

        self.ITEM_ACTIVATED = None

    #############################################
    #   PROPERTIES
    #################################

    @property
    def project(self):
        """
        Returns the reference to the active project.
        :rtype: Session
        """
        return self.session.project

    @property
    def session(self):
        """
        Returns the reference to the active session.
        :rtype: Session
        """
        return self.plugin.parent()

    #############################################
    #   EVENTS
    #################################

    def eventFilter(self, source, event):
        """
        Filter incoming events.
        :type source: QObject
        :type event: QtCore.QEvent
        """
        if source is self.verticalScrollBar():
            if event.type() in {QtCore.QEvent.Show, QtCore.QEvent.Hide}:
                self.redraw()
        return super().eventFilter(source, event)

    ###############################
    #
    ###############################
    @QtCore.pyqtSlot(str, str, str, str)
    def entry_MODIFY_ok(self,iri_from,prefix_from,iri_to,prefix_to):

        self.ENTRY_MODIFY_OK_var.add(True)

        self.entry_status.showMessage('Successfully modified',10000)
        print('entry_ADD_ok(self): ',iri_from,',',prefix_from,',',iri_to,',',prefix_to)

    @QtCore.pyqtSlot(str, str, str)
    def entry_ADD_ok(self,iri,prefix,message):

        self.ENTRY_ADD_OK_var.add(True)
        self.entry_status.showMessage(message,10000)
        print('entry_ADD_ok(self): ',iri,',',prefix,',',message)

    @QtCore.pyqtSlot(str, str, str)
    def entry_REMOVE_OK(self,iri,prefix,message):

        self.ENTRY_REMOVE_OK_var.add(True)
        self.entry_status.showMessage(message, 10000)
        print('entry_REMOVE_ok(self): ',iri,',',prefix,',',message)

    @QtCore.pyqtSlot(str, str, str)
    def entry_NOT_OK(self,iri,prefixes,message):

        self.ENTRY_IGNORE_var.add(True)
        self.entry_status.showMessage(message, 10000)
        print('entry_NOT_OK(self): ',iri,',',prefixes,',',message)

    @QtCore.pyqtSlot(int,int)
    def try_to_edit_cell(self,r,c):

        print(r,'-',c)
        self.table.editItem(self.table.item(r,c))

    def test_IRI(self):

        iri_inp = self.iri_input_box.text()
        res = self.project.check_validity_of_IRI(iri_inp)
        print('IRI_valid',res)

    def display_IRIPrefixesNodesDict(self):

        self.project.print_dictionary(self.project.IRI_prefixes_nodes_dict)

    def FillTableWithStandardData(self):

        for iri in self.project.IRI_prefixes_nodes_dict.keys():
            if iri in OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict.keys():
                item_iri = QtWidgets.QTableWidgetItem()
                item_iri.setText(iri)
                item_iri.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                item_iri.setBackground(QtGui.QBrush(QtGui.QColor(50,50,205,50)))
                self.table.setItem(self.table.rowCount() - 1, 0, item_iri)

                prefixes = self.project.IRI_prefixes_nodes_dict[iri][0]
                item_prefixes = QtWidgets.QTableWidgetItem()
                item_prefixes.setText(str(prefixes))
                item_prefixes.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                item_prefixes.setBackground(QtGui.QBrush(QtGui.QColor(50,50,205,50)))
                self.table.setItem(self.table.rowCount() - 1, 1, item_prefixes)

                if self.SHOW_NODES is True:

                    nodes = self.project.IRI_prefixes_nodes_dict[iri][1]
                    item_nodes = QtWidgets.QTableWidgetItem()
                    nds_ids = set()
                    for n in nodes:
                        nds_ids.add(n.id_with_diag)
                    item_nodes.setText(str(nds_ids))
                    item_nodes.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                    item_nodes.setBackground(QtGui.QBrush(QtGui.QColor(50,50,205,50)))
                    self.table.setItem(self.table.rowCount() - 1, 2, item_nodes)

                    properties = self.project.IRI_prefixes_nodes_dict[iri][2]
                    item_properties = QtWidgets.QTableWidgetItem()
                    item_properties.setText(str(properties))
                    item_properties.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                    self.table.setItem(self.table.rowCount() - 1, 3, item_properties)

                self.table.setRowCount(self.table.rowCount() + 1)

        iri = self.project.iri
        item_iri = QtWidgets.QTableWidgetItem()
        item_iri.setText(iri)
        item_iri.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
        item_iri.setBackground(QtGui.QBrush(QtGui.QColor(205, 50, 50, 50)))
        self.table.setItem(self.table.rowCount() - 1, 0, item_iri)

        prefixes = self.project.IRI_prefixes_nodes_dict[self.project.iri][0]
        item_prefixes = QtWidgets.QTableWidgetItem()
        item_prefixes.setText(str(prefixes))
        item_prefixes.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
        item_prefixes.setBackground(QtGui.QBrush(QtGui.QColor(205, 50, 50, 50)))
        self.table.setItem(self.table.rowCount() - 1, 1, item_prefixes)

        if self.SHOW_NODES is True:
            nodes = self.project.IRI_prefixes_nodes_dict[self.project.iri][1]
            item_nodes = QtWidgets.QTableWidgetItem()
            nds_ids = set()
            for n in nodes:
                nds_ids.add(n.id_with_diag)
            item_nodes.setText(str(nds_ids))
            item_nodes.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
            item_nodes.setBackground(QtGui.QBrush(QtGui.QColor(205, 50, 50, 50)))
            self.table.setItem(self.table.rowCount() - 1, 2, item_nodes)

            properties = self.project.IRI_prefixes_nodes_dict[self.project.iri][2]
            item_properties = QtWidgets.QTableWidgetItem()
            item_properties.setText(str(properties))
            item_properties.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
            self.table.setItem(self.table.rowCount() - 1, 3, item_properties)

        self.table.setRowCount(self.table.rowCount() + 1)

    #not used
    def update_table_row_containing_iri(self,iri_inp):

        for r in range(0, self.table.rowCount()):
            item = self.table.item(r, 0)
            if item.text() == iri_inp:
                # iri_inp in both table and dictionary
                if iri_inp in self.project.IRI_prefixes_nodes_dict.keys():
                    new_prefixes = self.project.IRI_prefixes_nodes_dict[iri_inp][0]
                    self.table.item(r, 1).setText(str(new_prefixes))
                    if self.table.columnCount() == 3:
                        new_nodes = self.project.IRI_prefixes_nodes_dict[iri_inp][1]
                        nds_ids = set()
                        for n in new_nodes:
                            nds_ids.add(n.id_with_diag)
                        self.table.item(r, 2).setText(str(nds_ids))
                # iri_inp in table and absent in dictionary
                else:
                    self.table.removeRow(r+1)
                    self.table.setRowCount(self.table.rowCount() - 1)

        if iri_inp in self.project.IRI_prefixes_nodes_dict.keys():
            flag = False
            for r in range(0, self.table.rowCount()):
                item = self.table.item(r, 0)
                if item.text() == iri_inp:
                    flag = True
            if flag is False:
                # iri_inp in dictionary and absent in table
                self.table.setRowCount(self.table.rowCount() + 1)

                item_iri = QtWidgets.QTableWidgetItem()
                item_iri.setText(iri_inp)
                item_iri.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                self.table.setItem(self.table.rowCount() - 1, 0, item_iri)

                prefixes = self.project.IRI_prefixes_nodes_dict[iri_inp][0]
                item_prefixes = QtWidgets.QTableWidgetItem()
                item_prefixes.setText(str(prefixes))
                item_prefixes.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                self.table.setItem(self.table.rowCount() - 1, 1, item_prefixes)

                if self.SHOW_NODES is True:
                    nodes = self.project.IRI_prefixes_nodes_dict[iri_inp][1]
                    item_nodes = QtWidgets.QTableWidgetItem()
                    nds_ids = set()
                    for n in nodes:
                        nds_ids.add(n.id_with_diag)
                    item_nodes.setText(str(nds_ids))
                    item_nodes.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                    self.table.setItem(self.table.rowCount() - 1, 2, item_nodes)
            else:
                # iri_inp in both table and dictionary (case already coverted above)
                pass

    @QtCore.pyqtSlot(str,str,str)
    def FillTableWithIRIPrefixNodesDictionaryKeysAndValues(self,iri_to_update,nodes_to_update,diag_name):

        #if (iri_to_update is None) and (nodes_to_update is None):
        # print('>>>  FillTableWithIRIPrefixNodesDictionaryKeysAndValues')
        # first delete all entries from the dictionary id present
        # add standard IRIs
        # add key value pairs from dict
        self.table.clear()
        self.table.setRowCount(1)

        if self.SHOW_NODES is True:
            self.table.setColumnCount(4)
        else:
            self.table.setColumnCount(2)

        header_iri = QtWidgets.QTableWidgetItem()
        header_iri.setText('IRI')
        header_iri.setFont(Font('Roboto', 15, bold=True))
        header_iri.setTextAlignment(QtCore.Qt.AlignCenter)
        header_iri.setBackground(QtGui.QBrush(QtGui.QColor(90, 80, 80, 200)))
        header_iri.setForeground(QtGui.QBrush(QtGui.QColor(255, 255, 255, 255)))
        self.table.setItem(self.table.rowCount() - 1, 0, header_iri)

        header_prefixes = QtWidgets.QTableWidgetItem()
        header_prefixes.setText('PREFIXES')
        header_prefixes.setFont(Font('Roboto', 15, bold=True))
        header_prefixes.setTextAlignment(QtCore.Qt.AlignCenter)
        header_prefixes.setBackground(QtGui.QBrush(QtGui.QColor(90, 80, 80, 200)))
        header_prefixes.setForeground(QtGui.QBrush(QtGui.QColor(255, 255, 255, 255)))
        self.table.setItem(self.table.rowCount() - 1, 1, header_prefixes)

        if self.SHOW_NODES is True:
            header_nodes = QtWidgets.QTableWidgetItem()
            header_nodes.setText('NODES')
            header_nodes.setFont(Font('Roboto', 15, bold=True))
            header_nodes.setTextAlignment(QtCore.Qt.AlignCenter)
            header_nodes.setBackground(QtGui.QBrush(QtGui.QColor(90, 80, 80, 200)))
            header_nodes.setForeground(QtGui.QBrush(QtGui.QColor(255, 255, 255, 255)))
            self.table.setItem(self.table.rowCount() - 1, 2, header_nodes)

            header_properties = QtWidgets.QTableWidgetItem()
            header_properties.setText('PROPERTIES')
            header_properties.setFont(Font('Roboto', 15, bold=True))
            header_properties.setTextAlignment(QtCore.Qt.AlignCenter)
            header_properties.setBackground(QtGui.QBrush(QtGui.QColor(90, 80, 80, 200)))
            header_properties.setForeground(QtGui.QBrush(QtGui.QColor(255, 255, 255, 255)))
            self.table.setItem(self.table.rowCount() - 1, 3, header_properties)

        self.table.setRowCount(self.table.rowCount() + 1)

        self.FillTableWithStandardData()

        for iri in sorted(self.project.IRI_prefixes_nodes_dict.keys()):

            if iri in OWLStandardIRIPrefixPairsDict.std_IRI_prefix_dict.keys():
                continue
            if iri == self.project.iri:
                continue

            item_iri = QtWidgets.QTableWidgetItem()
            item_iri.setText(iri)
            item_iri.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
            self.table.setItem(self.table.rowCount() - 1, 0, item_iri)

            prefixes = self.project.IRI_prefixes_nodes_dict[iri][0]
            item_prefixes = QtWidgets.QTableWidgetItem()
            item_prefixes.setText(str(prefixes))
            item_prefixes.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
            self.table.setItem(self.table.rowCount() - 1, 1, item_prefixes)

            if self.SHOW_NODES is True:
                nodes = self.project.IRI_prefixes_nodes_dict[iri][1]
                item_nodes = QtWidgets.QTableWidgetItem()
                nds_ids = set()
                for n in nodes:
                    nds_ids.add(n.id_with_diag)
                item_nodes.setText(str(nds_ids))
                item_nodes.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                self.table.setItem(self.table.rowCount() - 1, 2, item_nodes)

                properties = self.project.IRI_prefixes_nodes_dict[iri][2]
                item_properties = QtWidgets.QTableWidgetItem()
                item_properties.setText(str(properties))
                item_properties.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
                self.table.setItem(self.table.rowCount() - 1, 3, item_properties)

            self.table.setRowCount(self.table.rowCount() + 1)
        self.table.setRowCount(self.table.rowCount() - 1)

        """
            #print('>>>  FillTableWithIRIPrefixNodesDictionaryKeysAndValues      END')
        elif(iri_to_update is not None) and (nodes_to_update is None):
            self.update_table_row_containing_iri(iri_to_update)
        elif(iri_to_update is not None) and (nodes_to_update is not None):
            self.update_table_row_containing_iri(iri_to_update)
        """
        self.redraw()

    def hide_or_show_nodes(self):

        if self.SHOW_NODES is True:
            self.SHOW_NODES = False
        else:
            self.SHOW_NODES = True

        self.FillTableWithIRIPrefixNodesDictionaryKeysAndValues(None,None,None)

    def button_add(self):

        self.ADD_OR_REMOVE = 'add'
        self.process_entry_from_textboxes_for_button_add_or_remove()
        self.ADD_OR_REMOVE = None

    def button_remove(self):

        self.ADD_OR_REMOVE = 'remove'
        self.process_entry_from_textboxes_for_button_add_or_remove()
        self.ADD_OR_REMOVE = None

    def convert_prefixes_in_table_to_list(self,prefixes_str):

        if prefixes_str is None:
            return None

        prefixes_list = []

        if (prefixes_str[0] == '[') and (prefixes_str[len(prefixes_str)-1] == ']'):
            prefixes_str = prefixes_str[1:len(prefixes_str)-1]
        else:
            pass

        prefixes_str_split = prefixes_str.split(', ')

        for prefix_raw in prefixes_str_split:
            if (prefix_raw[0] == '\'') and (prefix_raw[len(prefix_raw)-1] == '\''):
                prefix = prefix_raw[1:len(prefix_raw) - 1]
                if prefix != '':
                    prefixes_list.add(prefix)

        #print('return prefixes_list',prefixes_list)
        return prefixes_list

    def process_entry_from_textboxes_for_button_add_or_remove(self):

        self.ENTRY_ADD_OK_var = set()
        self.ENTRY_REMOVE_OK_var = set()
        self.ENTRY_IGNORE_var = set()

        prefixes = []
        prefixes_inp = self.prefix_input_box.text().strip()
        prefixes_raw = prefixes_inp.split(',')
        for p in prefixes_raw:
            if p.strip() != '':
                prefixes.append(p.strip())

        iri = self.iri_input_box.text().strip()

        self.iri_input_box.clear()
        self.prefix_input_box.clear()

        if iri == '':
            print('iri field is empty')
            self.entry_status.showMessage('iri field is empty', 10000)
            return

        Duplicate_IRI_prefixes_nodes_dict_1 = self.project.copy_IRI_prefixes_nodes_dictionaries(
            self.project.IRI_prefixes_nodes_dict, dict())

        Duplicate_IRI_prefixes_nodes_dict_2 = self.project.copy_IRI_prefixes_nodes_dictionaries(
            self.project.IRI_prefixes_nodes_dict, dict())

        process = False

        if len(prefixes) > 0:
            for prefix in prefixes:
                if self.ADD_OR_REMOVE == 'remove':
                    #self.project.removeIRIPrefixEntry(Duplicate_IRI_prefixes_nodes_dict_1, iri, prefix)
                    self.project.addORremoveIRIPrefixEntry(Duplicate_IRI_prefixes_nodes_dict_1, iri, prefix, 'remove_entry')
                    if (False in self.ENTRY_REMOVE_OK_var) or (True in self.ENTRY_IGNORE_var):
                        LOGGER.error('transaction was not executed correctly; problem with a prefix/IRI')
                        return
                    else:
                        process = True
                elif self.ADD_OR_REMOVE == 'add':
                    #self.project.addIRIPrefixEntry(Duplicate_IRI_prefixes_nodes_dict_1, iri, prefix)
                    self.project.addORremoveIRIPrefixEntry(Duplicate_IRI_prefixes_nodes_dict_1, iri, prefix, 'add_entry')
                    if (False in self.ENTRY_ADD_OK_var) or (True in self.ENTRY_IGNORE_var) :
                        LOGGER.error('transaction was not executed correctly; problem with a prefix/IRI')
                        return
                    else:
                        process = True
                else:
                    pass
        else:
            if self.ADD_OR_REMOVE == 'remove':
                #self.project.removeIRIPrefixEntry(Duplicate_IRI_prefixes_nodes_dict_1, iri, None)
                self.project.addORremoveIRIPrefixEntry(Duplicate_IRI_prefixes_nodes_dict_1, iri, None, 'remove_entry')
                if (False in self.ENTRY_REMOVE_OK_var) or (True in self.ENTRY_IGNORE_var):
                    LOGGER.error('transaction was not executed correctly; problem with IRI')
                    return
                else:
                    process = True
            elif self.ADD_OR_REMOVE == 'add':
                #self.project.addIRIPrefixEntry(Duplicate_IRI_prefixes_nodes_dict_1, iri, None)
                self.project.addORremoveIRIPrefixEntry(Duplicate_IRI_prefixes_nodes_dict_1, iri, None, 'add_entry')
                if (False in self.ENTRY_ADD_OK_var) or (True in self.ENTRY_IGNORE_var):
                    LOGGER.error('transaction was not executed correctly; problem with IRI')
                    return
                else:
                    process = True
            else:
                pass

        if process is True:
            self.session.undostack.push(CommandProjetSetIRIPrefixesNodesDict(self.project,\
                                        Duplicate_IRI_prefixes_nodes_dict_2,Duplicate_IRI_prefixes_nodes_dict_1, [iri], None))

        self.ENTRY_ADD_OK_var = set()
        self.ENTRY_REMOVE_OK_var = set()
        self.ENTRY_IGNORE_var = set()

    def process_entry_from_textboxes_for_button_modify(self):

        self.ENTRY_MODIFY_OK_var = set()
        self.ENTRY_IGNORE_var = set()

        items_selected = []

        for r in range(0, self.table.rowCount()):
            for c in range(0, 2):
                item = self.table.item(r, c)
                if item.isSelected():
                    #print(item.text(), ' is selected')
                    items_selected.append(item)

        range_of_rows = set()

        for i in items_selected:
            range_of_rows.add(i.row())

        if len(range_of_rows) > 1:
            self.entry_status.showMessage('please modify 1 IRI-Prefix pair at a time')
        elif len(range_of_rows) == 1:

            prefixes_input_box_set = set()
            prefixes_inp = self.prefix_input_box.text().strip()
            prefixes_raw = prefixes_inp.split(',')
            for p in prefixes_raw:
                if p.strip() != '':
                    prefixes_input_box_set.add(p.strip())

            iri_input_box = self.iri_input_box.text().strip()

            condition_IRI_item_selected_A = (items_selected[0].column() == 0)
            condition_prefixes_item_selected_A = (items_selected[0].column() == 1)

            if len(items_selected) == 2:
                condition_IRI_item_selected_B = (items_selected[1].column() == 0)
                condition_prefixes_item_selected_B = (items_selected[1].column() == 1)
            else:
                condition_IRI_item_selected_B = False
                condition_prefixes_item_selected_B = False

            condition_IRI_item_selected = (condition_IRI_item_selected_A or condition_IRI_item_selected_B)
            condition_prefixes_item_selected = (
                condition_prefixes_item_selected_A or condition_prefixes_item_selected_B)
            condition_iri_input_box_is_empty = (iri_input_box == '')
            condition_prefixes_input_box_is_empty = (len(prefixes_input_box_set) == 0)

            item_iri = None
            item_prefixes = None

            if condition_IRI_item_selected_A is True:
                item_iri = items_selected[0].text()
            else:  # condition_IRI_item_selected_A is False
                if condition_IRI_item_selected_B is True:
                    item_iri = items_selected[1].text()

            if condition_prefixes_item_selected_A is True:
                item_prefixes = items_selected[0].text()
            else:  # condition_prefixes_item_selected_A is False
                if condition_prefixes_item_selected_B is True:
                    item_prefixes = items_selected[1].text()

            item_prefixes_list = self.convert_prefixes_in_table_to_list(item_prefixes)

            """
            print('item_iri',item_iri)
            print('prefixes_input_box_set', item_prefixes_set)
            print('iri_input_box', iri_input_box)
            print('prefixes_input_box_set', prefixes_input_box_set)

            return
            """
            # caseX1  None->* | *-> None
            if (condition_iri_input_box_is_empty) and (condition_prefixes_input_box_is_empty):
                self.entry_status.showMessage(
                    'Please enter IRI and/or prefix in the respective text fields to modify', 10000)
                return

            # caseX2  prefix(es) -> IRI' | IRI -> prefix(es)'
            if ((len(items_selected) == 1)):
                if (condition_prefixes_item_selected and not condition_iri_input_box_is_empty) or \
                        (condition_IRI_item_selected and not condition_prefixes_input_box_is_empty):
                    self.entry_status.showMessage('IRI cannot be modified to Prefixes or vice versa', 10000)
                    return

            Duplicate_IRI_prefixes_nodes_dict_1 = self.project.copy_IRI_prefixes_nodes_dictionaries(self.project.IRI_prefixes_nodes_dict, dict())
            Duplicate_IRI_prefixes_nodes_dict_2 = self.project.copy_IRI_prefixes_nodes_dictionaries(self.project.IRI_prefixes_nodes_dict, dict())

            process = False

            iris_to_be_updated = []

            # case1
            if (condition_IRI_item_selected is True) and (condition_prefixes_item_selected is False):
                print('case1')
                if not condition_iri_input_box_is_empty:
                    if condition_prefixes_input_box_is_empty is True:
                        # Case1.1     IRI->IRI'         if iri==iri' no need for a transaction
                        if (item_iri == iri_input_box):
                            print('case1.1')
                            self.entry_status.showMessage('IRIs in selected cell and input box are the same. Nothing to change', 10000)
                            return

                        self.project.modifyIRIPrefixesEntry(item_iri,None,iri_input_box,None,Duplicate_IRI_prefixes_nodes_dict_1)
                        iris_to_be_updated.append(item_iri)
                        iris_to_be_updated.append(iri_input_box)

                        if(False in self.ENTRY_MODIFY_OK_var) or (True in self.ENTRY_IGNORE_var):
                            LOGGER.error('transaction was not executed correctly; problem with a prefix/IRI')
                            return
                        else:
                            process = True
                    else:
                        # Case1.2     IRI->[IRI',prefix(es)']   IRI=IRI' | IRI!=IRI'
                        #$$$$$$
                        print('case1.2')

                        self.project.modifyIRIPrefixesEntry(item_iri,None,iri_input_box,prefixes_input_box_set,Duplicate_IRI_prefixes_nodes_dict_1)
                        iris_to_be_updated.append(item_iri)
                        iris_to_be_updated.append(iri_input_box)

                        if (False in self.ENTRY_MODIFY_OK_var) or (True in self.ENTRY_IGNORE_var):
                            LOGGER.error('transaction was not executed correctly; problem with a prefix/IRI')
                            return
                        else:
                            process = True

            # case2
            if (condition_prefixes_item_selected is True) and (condition_IRI_item_selected is False):
                print('case2')
                if not condition_prefixes_input_box_is_empty:
                    if condition_iri_input_box_is_empty is True:
                        print('case2.1')
                        # case2.1     prefix(es)->prefix(es)'          if prefix(es)==prefix(es)' no need for a transaction
                        if (item_prefixes_list.issubset(prefixes_input_box_set) and prefixes_input_box_set.issubset(item_prefixes_list)):
                            self.entry_status.showMessage(
                                'prefix(es) in selected cell and input box are the same. Nothing to change', 10000)
                            return

                        self.project.modifyIRIPrefixesEntry(None,item_prefixes_list,None,prefixes_input_box_set,Duplicate_IRI_prefixes_nodes_dict_1)

                        for iri_key in Duplicate_IRI_prefixes_nodes_dict_1.keys():
                            prefixes_for_iri_key = Duplicate_IRI_prefixes_nodes_dict_1[iri_key][0]
                            C1 = prefixes_for_iri_key.issubset(item_prefixes_list) and item_prefixes_list.issubset(prefixes_for_iri_key)
                            C2 = prefixes_for_iri_key.issubset(prefixes_input_box_set) and prefixes_input_box_set.issubset(prefixes_for_iri_key)
                            if C1 or C2:
                                iris_to_be_updated.append(iri_key)

                        if (False in self.ENTRY_MODIFY_OK_var) or (True in self.ENTRY_IGNORE_var):
                            LOGGER.error('transaction was not executed correctly; problem with a prefix/IRI')
                            return
                        else:
                            process = True
                    else:
                        print('case2.2')
                        # case2.2     prefix(es)->[IRI',prefix(es)']   prefix->[IRI',prefix(es)'] is an invalid transaction
                        self.entry_status.showMessage('prefix->[IRI\',prefix(es)\'] is an invalid transaction', 10000)
                        return

            # case3
            if (condition_prefixes_item_selected is True) and (condition_IRI_item_selected is True):
                print('case3')
                if (condition_iri_input_box_is_empty is False) and (condition_prefixes_input_box_is_empty is True):
                    # case3.1       [IRI,prefix(es)] -> [IRI']
                    print('case3.1')

                    self.project.modifyIRIPrefixesEntry(item_iri,item_prefixes_list,iri_input_box,None,Duplicate_IRI_prefixes_nodes_dict_1)
                    iris_to_be_updated.append(item_iri)
                    iris_to_be_updated.append(iri_input_box)

                    if (False in self.ENTRY_MODIFY_OK_var) or (True in self.ENTRY_IGNORE_var):
                        LOGGER.error('transaction was not executed correctly; problem with a prefix/IRI')
                        return
                    else:
                        process = True
                elif (condition_iri_input_box_is_empty is True) and (condition_prefixes_input_box_is_empty is False):
                    # case3.2       [IRI,prefix(es)] -> [prefix(es)']       if prefix==prefix' no need for a transaction
                    print('case3.2')
                    if (item_prefixes_list.issubset(prefixes_input_box_set) and prefixes_input_box_set.issubset(item_prefixes_list)):
                        self.entry_status.showMessage(
                            'prefix(es) in selected cell and input box are the same. Nothing to change', 10000)
                        return

                    self.project.modifyIRIPrefixesEntry(item_iri,item_prefixes_list,None,prefixes_input_box_set,Duplicate_IRI_prefixes_nodes_dict_1)
                    iris_to_be_updated.append(item_iri)

                    for iri_key in Duplicate_IRI_prefixes_nodes_dict_1.keys():
                        prefixes_for_iri_key = Duplicate_IRI_prefixes_nodes_dict_1[iri_key][0]
                        C2 = prefixes_for_iri_key.issubset(prefixes_input_box_set) and prefixes_input_box_set.issubset(
                            prefixes_for_iri_key)
                        if C2:
                            iris_to_be_updated.append(iri_key)

                    if (False in self.ENTRY_MODIFY_OK_var) or (True in self.ENTRY_IGNORE_var):
                        LOGGER.error('transaction was not executed correctly; problem with a prefix/IRI')
                        return
                    else:
                        process = True
                elif (condition_iri_input_box_is_empty is False) and (
                            condition_prefixes_input_box_is_empty is False):
                    # case3.3       [IRI,prefix(es)] -> [IRI',prefix(es)']   if prefix(es)==prefix(es)' and iri==iri' no need for a transaction
                    print('case3.3')
                    if (item_prefixes_list.issubset(prefixes_input_box_set) and prefixes_input_box_set.issubset(item_prefixes_list)) and (item_iri == iri_input_box):
                        self.entry_status.showMessage('IRI and prefix(es) in selected cell and input box are the same. Nothing to change', 10000)
                        return

                    self.project.modifyIRIPrefixesEntry(item_iri,item_prefixes_list,iri_input_box,prefixes_input_box_set,Duplicate_IRI_prefixes_nodes_dict_1)
                    iris_to_be_updated.append(item_iri)
                    iris_to_be_updated.append(iri_input_box)

                    if (False in self.ENTRY_MODIFY_OK_var) or (True in self.ENTRY_IGNORE_var):
                        LOGGER.error('transaction was not executed correctly; problem with a prefix/IRI')
                        return
                    else:
                        process = True
                else:
                    # already covered in caseX1
                    pass

            print('before pushing to stack')

            #self.project.print_dictionary(Duplicate_IRI_prefixes_nodes_dict_1)

            print('before pushing to stack END')

            if process is True:
                self.session.undostack.push(CommandProjetSetIRIPrefixesNodesDict(self.project, \
                            Duplicate_IRI_prefixes_nodes_dict_2, Duplicate_IRI_prefixes_nodes_dict_1, iris_to_be_updated, None))

            self.iri_input_box.clear()
            self.prefix_input_box.clear()

        else:
            self.entry_status.showMessage('please select the cells in the table to modify', 10000)

        self.ENTRY_MODIFY_OK_var = set()
        self.ENTRY_IGNORE_var = set()

    #############################################
    #   INTERFACE
    #################################

    def redraw(self):
        """
        Redraw the content of the widget.
        """
        if self.SHOW_NODES is True:
            self.table.setColumnCount(4)
        else:
            self.table.setColumnCount(2)

        width = self.width()
        scrollbar = self.verticalScrollBar()
        if scrollbar.isVisible():
            width -= scrollbar.width()
        #sizeHint = self.table.sizeHint()
        #height = sizeHint.height()
        height_of_other_objects = (self.dictionary_display_button.height() + self.entry_status.height()+\
                                  self.iri_input_box.height() + self.prefix_input_box.height())
        height = (self.height()) - (height_of_other_objects)
        self.table.setFixedWidth(width)
        #self.table.setFixedHeight(clamp(height, 0))
        self.table.setMinimumHeight(height)

        if self.SHOW_NODES is True:
            self.table.setColumnWidth(0,self.width()/4)
            self.table.setColumnWidth(1,self.width()/4)
            self.table.setColumnWidth(2,self.width()/4)
            self.table.setColumnWidth(3, self.width() / 4)
        else:
            self.table.setColumnWidth(0, 2*self.width() / 3)
            self.table.setColumnWidth(1, self.width() / 3)

        for r in range(0,self.table.rowCount()):
            self.table.resizeRowToContents(r)

    @QtCore.pyqtSlot()
    def run(self):
        """
        Set the current stacked widget.
        """
        self.FillTableWithIRIPrefixNodesDictionaryKeysAndValues(None,None,None)
        self.redraw()









