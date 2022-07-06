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
  uncoupledKinematicParcelFoam.C
)

set(LIBRARIES
  libOpenFOAM
  liblagrangian
  liblagrangianIntermediate
  liblagrangianTurbulence
  libthermophysicalFunctions
  libcompressibleTransportModels
  libfluidThermophysicalModels
  libspecie
  libradiationModels
  libturbulenceModels
  libcompressibleTurbulenceModels
  libfiniteVolume
  libmeshTools
  libregionModels
  libsurfaceFilmModels
)

include_directories(.)
include(../../../../src/OpenFOAM/include.cmake)
include(../../../../src/OSspecific/POSIX/include.cmake)
include(../../../../src/finiteVolume/include.cmake)
include(../../../../src/lagrangian/basic/include.cmake)
include(../../../../src/lagrangian/intermediate/include.cmake)
include(../../../../src/thermophysicalModels/specie/include.cmake)
include(../../../../src/transportModels/compressible/include.cmake)
include(../../../../src/thermophysicalModels/basic/include.cmake)
include(../../../../src/thermophysicalModels/reactionThermo/include.cmake)
include(../../../../src/thermophysicalModels/radiation/include.cmake)
include(../../../../src/TurbulenceModels/turbulenceModels/include.cmake)
include(../../../../src/TurbulenceModels/compressible/include.cmake)
include(../../../../src/meshTools/include.cmake)
include(../../../../src/regionModels/regionModel/include.cmake)
include(../../../../src/regionModels/surfaceFilmModels/include.cmake)

add_executable(uncoupledKinematicParcelFoam ${SOURCES})

target_link_libraries(uncoupledKinematicParcelFoam ${LIBRARIES})

install(TARGETS uncoupledKinematicParcelFoam DESTINATION applications OPTIONAL)
