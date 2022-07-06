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

include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/pointPatchFields/derived/sixDoFRigidBodyDisplacement)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/pointPatchFields/derived/uncoupledSixDoFRigidBodyDisplacement)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/constraints/axis)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/constraints/line)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/constraints/orientation)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/constraints/plane)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/constraints/point)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/constraints/sixDoFRigidBodyMotionConstraint)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/restraints/linearAxialAngularSpring)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/restraints/linearDamper)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/restraints/linearSpring)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/restraints/sixDoFRigidBodyMotionRestraint)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/restraints/sphericalAngularDamper)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/restraints/sphericalAngularSpring)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotion/restraints/tabulatedAxialAngularSpring)
include_directories(${PROJECT_SOURCE_DIR}/src/sixDoFRigidBodyMotion/sixDoFRigidBodyMotionSolver)
