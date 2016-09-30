
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


from abc import ABCMeta

from enum import IntEnum, Enum


class Enum_(Enum):
    """
    Extends Enum class providing some additional methods.
    """
    __metaclass__  = ABCMeta

    @classmethod
    def forValue(cls, value):
        """
        Returns the Enum_ entry matching the given value.
        :type value: str
        :rtype: Enum_
        """
        if isinstance(value, Enum_):
            return value
        for x in cls:
            if x.value.strip() == value.strip():
                return x
        return None


class IntEnum_(IntEnum):
    """
    Extends IntEnum class providing some additional methods.
    """
    __metaclass__  = ABCMeta

    @classmethod
    def forValue(cls, value):
        """
        Returns the IntEnum_ entry matching the given value.
        :type value: str
        :rtype: IntEnum_
        """
        for x in cls:
            if x.value == value:
                return x
        return None

    @classmethod
    def forValue(cls, value):
        """
        Returns the IntEnum_ entry matching the given value.
        :type value: T <= int | str | Item
        :rtype: IntEnum_
        """
        if isinstance(value, IntEnum_):
            return value
        value = int(value)
        for x in cls:
            if x.value == value:
                return x
        return None