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

include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/derivedFvPatchFields/greyDiffusiveRadiation)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/derivedFvPatchFields/greyDiffusiveViewFactor)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/derivedFvPatchFields/MarshakRadiation)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/derivedFvPatchFields/MarshakRadiationFixedTemperature)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/derivedFvPatchFields/radiationCoupledBase)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/derivedFvPatchFields/wideBandDiffusiveRadiation)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/include)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/fvDOM/absorptionCoeffs)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/fvDOM/blackBodyEmission)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/fvDOM/fvDOM)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/fvDOM/radiativeIntensityRay)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/noRadiation)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/opaqueSolid)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/P1)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/radiationModel)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/radiationModel/viewFactor)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/absorptionEmissionModel/absorptionEmissionModel)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/absorptionEmissionModel/binaryAbsorptionEmission)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/absorptionEmissionModel/constantAbsorptionEmission)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/absorptionEmissionModel/greyMeanAbsorptionEmission)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/absorptionEmissionModel/greyMeanSolidAbsorptionEmission)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/absorptionEmissionModel/noAbsorptionEmission)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/absorptionEmissionModel/wideBandAbsorptionEmission)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/scatterModel/constantScatter)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/scatterModel/noScatter)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/scatterModel/scatterModel)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/sootModel/mixtureFractionSoot)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/sootModel/noSoot)
include_directories(${PROJECT_SOURCE_DIR}/src/thermophysicalModels/radiationModels/submodels/sootModel/sootModel)
