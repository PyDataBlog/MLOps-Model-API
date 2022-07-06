#
#   Copyright (C) 2014-15 Chris Copsey
#   
#   This file is part of ccFOAM.
#
#   ccFOAM is free software: you can redistribute it and/or modify it under the
#   terms of the GNU General Public License as published by the Free Software
#   Foundation, either version 3 of the License, or (at your option) any later
#   version.
#
#   ccFOAM is distributed in the hope that it will be useful, but WITHOUT ANY
#   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
#   details.
#
#   You should have received a copy of the GNU General Public License along
#   with ccFOAM.  If not, see <http://www.gnu.org/licenses/>.
#

set(SOURCES
  derivedFvPatchFields/energyRegionCoupled/energyRegionCoupledFvPatchScalarField.C
)

set(LIBRARIES
  libcompressibleTransportModels
  libfluidThermophysicalModels
  libturbulenceModels
  libcompressibleTurbulenceModels
  libfiniteVolume
  libmeshTools
)

include_directories(.)
include(../../src/OpenFOAM/include.cmake)
include(../../src/OSspecific/POSIX/include.cmake)
include(../../src/finiteVolume/include.cmake)
include(../../src/transportModels/compressible/include.cmake)
include(../../src/TurbulenceModels/turbulenceModels/include.cmake)
include(../../src/TurbulenceModels/compressible/include.cmake)
include(../../src/thermophysicalModels/basic/include.cmake)
include(../../src/meshTools/include.cmake)
include(include.cmake)

add_library(libregionCoupled ${SOURCES})
set_target_properties(libregionCoupled PROPERTIES PREFIX "")

target_link_libraries(libregionCoupled ${LIBRARIES})

install(TARGETS libregionCoupled DESTINATION lib OPTIONAL)
