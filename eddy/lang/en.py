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
#     - Marco Console <console@dis.uniroma1.it>                          #
#                                                                        #
##########################################################################


#############################################
# ACTIONS
#################################

ACTION_ABOUT_N = 'About {0}'
ACTION_BRING_TO_FRONT_N = 'Bring to Front'
ACTION_CENTER_DIAGRAM_N = 'Center diagram'
ACTION_CLOSE_PROJECT_N = 'Close project'
ACTION_COMPOSE_PROPERTY_DOMAIN_N = 'Property Domain'
ACTION_COMPOSE_PROPERTY_RANGE_N = 'Propery Range'
ACTION_COPY_N = 'Copy'
ACTION_CUT_N = 'Cut'
ACTION_DELETE_N = 'Delete'
ACTION_DIAG_WEBSITE_N = 'DIAG - Sapienza University'
ACTION_EDGE_SWAP_N = 'Swap'
ACTION_EXPORT_N = 'Export...'
ACTION_GRAPHOL_WEBSITE_N = 'Visit Graphol website'
ACTION_IMPORT_N = 'Import...'
ACTION_NEW_DIAGRAM_N = 'New diagram...'
ACTION_OPEN_N = 'Open...'
ACTION_OPEN_DIAGRAM_PROPERTIES_N = 'Properties...'
ACTION_OPEN_NODE_PROPERTIES_N = 'Properties...'
ACTION_OPEN_PREFERENCES_N = 'Preferences'
ACTION_PASTE_N = 'Paste'
ACTION_PRINT_N = 'Print...'
ACTION_QUIT_N = 'Quit'
ACTION_REFACTOR_NAME_N = 'Rename...'
ACTION_RELOCATE_LABEL_N = 'Relocate label'
ACTION_REMOVE_EDGE_BREAKPOINT_N = 'Remove breakpoint'
ACTION_SAVE_N = 'Save'
ACTION_SAVE_AS_N = 'Save As...'
ACTION_SELECT_ALL_N = 'Select all'
ACTION_SEND_TO_BACK_N = 'Send to Back'
ACTION_SET_VALUE_RESTRICTION_N = 'Select restriction...'
ACTION_SNAP_TO_GRID_N = 'Snap to grid'
ACTION_SYNTAX_CHECK_N = 'Run syntax validation'
ACTION_TOGGLE_EDGE_COMPLETE_N = 'Complete'

ACTION_ABOUT_S = 'About {0}'
ACTION_BRING_TO_FRONT_S = 'Bring selected items to front'
ACTION_CENTER_DIAGRAM_S = 'Center the active diagram'
ACTION_CLOSE_PROJECT_S = 'Close the current project'
ACTION_COPY_S = 'Copy selected items'
ACTION_CUT_S = 'Cut selected items'
ACTION_DELETE_S = 'Delete selected items'
ACTION_EXPORT_S = 'Export the current project'
ACTION_IMPORT_S = 'Import a document in the current project'
ACTION_NEW_DIAGRAM_S = 'Create a new diagram'
ACTION_OPEN_S = 'Open a diagram and add it to the current project'
ACTION_PASTE_S = 'Paste previously copied items'
ACTION_PRINT_S = 'Print the current project'
ACTION_QUIT_S = 'Quit {0}'
ACTION_SAVE_S = 'Save the current project'
ACTION_SAVE_AS = 'Create a copy of the active diagram'
ACTION_SELECT_ALL_S = 'Select all items in the active diagram'
ACTION_SEND_TO_BACK_S = 'Send selected items to back'
ACTION_SNAP_TO_GRID_S = 'Snap diagram elements to the grid'
ACTION_SYNTAX_CHECK_S = 'Run syntax validation on the current project'

#############################################
# MENUS
#################################

MENU_COMPOSE = 'Compose'
MENU_EDIT = 'Edit'
MENU_FILE = 'File'
MENU_HELP = 'Help'
MENU_REFACTOR = 'Refactor'
MENU_REFACTOR_BRUSH = 'Select color'
MENU_SET_BRUSH = 'Select color'
MENU_SET_DATATYPE = 'Select type'
MENU_SET_INDIVIDUAL_AS = 'Set as'
MENU_SET_PROPERTY_RESTRICTION = 'Select restriction'
MENU_SET_SPECIAL = 'Special type'
MENU_SWITCH_OPERATOR = 'Switch to'
MENU_TOOLS = 'Tools'
MENU_VIEW = 'View'

#############################################
# UNDO / REDO COMMANDS
#################################

COMMAND_DIAGRAM_CENTER = 'center diagram'
COMMAND_DIAGRAM_RESIZE = 'resize diagram'
COMMAND_EDGE_ADD = 'add {0}'
COMMAND_EDGE_ANCHOR_MOVE = 'move {0} anchor point'
COMMAND_EDGE_BREAKPOINT_ADD = 'add {0} breakpoint'
COMMAND_EDGE_BREAKPOINT_MOVE = 'move {0} breakpoint'
COMMAND_EDGE_BREAKPOINT_REMOVE = 'remove {0} breakpoint'
COMMAND_EDGE_SWAP = 'swap {0}'
COMMAND_EDGE_SWAP_MULTI = 'swap {0} edges'
COMMAND_EDGE_TOGGLE_COMPLETE = 'toggle {0} completness'
COMMAND_EDGE_TOGGLE_COMPLETE_MULTI = 'toggle completness for {0} edges'
COMMAND_ITEM_TRANSLATE = 'move {0} item{1}'
COMMAND_ITEM_REMOVE = 'remove {0}'
COMMAND_ITEM_REMOVE_MULTI = 'remove {0} items'
COMMAND_NODE_ADD = 'add {0}'
COMMAND_NODE_CHANGE_INPUTS_ORDER = 'change {0} inputs order'
COMMAND_NODE_CHANGE_META = 'change {0} metadata'
COMMAND_NODE_EDIT_LABEL = 'edit {0} label'
COMMAND_NODE_MOVE = 'move {0}'
COMMAND_NODE_MOVE_LABEL = 'move {0} label'
COMMAND_NODE_MOVE_MULTI = 'move {0} nodes'
COMMAND_NODE_OPERATOR_SWITCH = 'switch {0} to {1}'
COMMAND_NODE_RESIZE = 'resize {0}'
COMMAND_NODE_SET_BRUSH = 'set {0} brush on {1} node{2}'
COMMAND_NODE_SET_DEPTH = 'change {0} depth'

#############################################
# SYNTAX VALIDATION
#################################

SYNTAX_SELF_CONNECTION = 'Self connection is not valid'

SYNTAX_INCLUSION_VALUE_DOMAIN_MISSING_ATOMIC = 'Inclusion between value-domain expressions must include at least an atomic datatype'
SYNTAX_INCLUSION_CHAIN_AS_TARGET = 'Role chain nodes cannot be target of a Role inclusion'
SYNTAX_INCLUSION_CHAIN_AS_SOURCE_INVALID_TARGET = 'Inclusion between {0} and {0} is forbidden'
SYNTAX_INCLUSION_COMPLEMENT_INVALID_SOURCE = 'Invalid source for {0} inclusion: {1}'
SYNTAX_INCLUSION_NO_GRAPHOL_EXPRESSION = 'Type mismatch: inclusion must involve two graphol expressions'
SYNTAX_INCLUSION_NOT_COMPATIBLE = 'Type mismatch: {0} and {1} are not compatible'
SYNTAX_INCLUSION_TYPE_MISMATCH = 'Type mismatch: inclusion between {0} and {1}'

SYNTAX_INPUT_COMPLEMENT_INVALID_EXPRESSION = 'Invalid negative {0} expression'
SYNTAX_INPUT_DATA_TOO_MANY_DATATYPE = 'Too many value-domain nodes in input to datatype restriction node'
SYNTAX_INPUT_DATA_DATATYPE_MISMATCH = 'Datatype mismatch: restriction between {0} and {0}'
SYNTAX_INPUT_DR_INVALID_QUALIFIED_RESTRICTION = 'Invalid restriction ({0}) for qualified restriction'
SYNTAX_INPUT_DR_INVALID_QUALIFIED_RESTRICTION_OPERANDS = 'Invalid inputs ({0} + {1}) for qualified restriction'
SYNTAX_INPUT_DR_ATTRIBUTE_NO_SELF = 'Attributes do not have self'
SYNTAX_INPUT_ENUMERATION_INVALID_TARGET_IDENTITY = 'Target node has an invalid identity: {0}'
SYNTAX_INPUT_PROP_ASSERTION_TOO_MANY_INSTANCES = 'Too many instances in input to {0}'
SYNTAX_INPUT_PROP_ASSERTION_TOO_MANY_VALUES = 'Too many values in input to {0}'
SYNTAX_INPUT_VALUE_RESTRICTION_SOURCE = 'Invalid target: {0}'
SYNTAX_INPUT_INVALID_COMPOSITION = 'Type mismatch: {0} between {1} and {2}'
SYNTAX_INPUT_INVALID_OPERAND = 'Invalid input to {0}: {1}'
SYNTAX_INPUT_INVALID_TARGET = 'Input edges can only target constructor nodes'
SYNTAX_INPUT_TOO_MANY_OPERANDS = 'Too many inputs to {0}'

SYNTAX_MEMBERSHIP_INVALID_ASSERTION_TARGET = 'Invalid target for {0} assertion: {1}'
SYNTAX_MEMBERSHIP_INVALID_SOURCE = 'Invalid source for membership edge: {0}'
SYNTAX_MEMBERSHIP_INVALID_TARGET = 'Invalid target for membership edge: {0}'


#############################################
# DOCK WIDGETS
#################################

DOCK_ONTOLOGY_EXPLORER = 'Ontology Explorer'
DOCK_INFO = 'Info'
DOCK_OVERVIEW = 'Overview'
DOCK_PALETTE = 'Palette'
DOCK_PROJECT_EXPLORER = 'Project Explorer'

#############################################
# ONTOLOGY EXPLORER
#################################

ONTO_EXPLORER_SEARCH_PLACEHOLDER = 'Search...'

#############################################
# DIAGRAM CREATE / EDIT / LOAD
#################################

DIAGRAM_CAPTION_ALREADY_EXISTS = "Diagram '{0}' already exists!"
DIAGRAM_CAPTION_NAME_NOT_VALID = "'{0}' is not a valid diagram name!"
DIAGRAM_CREATION_FAILED_WINDOW_TITLE = 'Diagram creation failed!'
DIAGRAM_CREATION_FAILED_MESSAGE = 'Eddy could not create the specified diagram: {0}!'
DIAGRAM_LOAD_FAILED_WINDOW_TITLE = 'Diagram load failed!'
DIAGRAM_LOAD_FAILED_MESSAGE = 'Eddy could not load the specified diagram: {0}!'
DIAGRAM_LOCATION_LABEL = 'Location'
DIAGRAM_NAME_LABEL = 'Name'
DIAGRAM_WINDOW_TITLE_NEW = 'New diagram'

#############################################
# PROJECT DIALOG
#################################

PROJECT_CAPTION_ALREADY_EXISTS = "Project '{0}' already exists!"
PROJECT_CAPTION_NAME_NOT_VALID = "'{0}' is not a valid project name!"
PROJECT_IRI_LABEL = 'IRI'
PROJECT_NAME_LABEL = 'Name'
PROJECT_PREFIX_LABEL = 'Prefix'
PROJECT_LOCATION_LABEL = 'Location'
PROJECT_WINDOW_TITLE = 'New project'

#############################################
# WELCOME SCREEN
#################################

WELCOME_ACTION_BUG_REPORT = 'Report a bug'
WELCOME_ACTION_VISIT_EDDY_HOME = 'GitHub repository'
WELCOME_ACTION_VISIT_GRAPHOL_WEBSITE = 'Visit Graphol website'
WELCOME_APP_VERSION = 'Version: {0}'
WELCOME_BTN_HELP = 'Help'
WELCOME_BTN_NEW_PROJECT = 'Create new project'
WELCOME_BTN_OPEN_PROJECT = 'Open project'
WELCOME_WINDOW_TITLE = 'Welcome to {0}'

#############################################
# WORKSPACE_DIALOG
#################################

WORKSPACE_CREATION_FAILED_WINDOW_TITLE = 'Workspace setup failed!'
WORKSPACE_CREATION_FAILED_MESSAGE = 'Eddy could not create the specified workspace: {0}!'
WORKSPACE_EDIT_FIELD_PREFIX = 'Workspace'
WORKSPACE_WINDOW_TITLE = 'Configure workspace'
WORKSPACE_HEAD_TITLE = 'Select a workspace'
WORKSPACE_HEAD_DESCRIPTION = 'Eddy stores your projects in a directory called workspace.\n' \
                             'Please choose a workspace directory to use.'