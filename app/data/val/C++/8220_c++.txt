#include "cellinfo.h"

CellInfo::CellInfo(Position pos, bool filled, bool canTravelXNeg, bool canTravelXPos,
	               bool canTravelYNeg, bool canTravelYPos, bool canTravelZNeg, bool canTravelZPos)
                   : itsPosition(pos) {
	itsCanTravel[NO_DIRECTION] = filled;
	itsCanTravel[XNEG] = canTravelXNeg;
	itsCanTravel[XPOS] = canTravelXPos;
	itsCanTravel[YNEG] = canTravelYNeg;
	itsCanTravel[YPOS] = canTravelYPos;
	itsCanTravel[ZNEG] = canTravelZNeg;
	itsCanTravel[ZPOS] = canTravelZPos;
}

Position CellInfo::getPosition() {
	return itsPosition;
}

bool CellInfo::canTravel(Direction dir) {
	return itsCanTravel[dir];
}
bool CellInfo::canTravel() {
	return itsCanTravel[NO_DIRECTION];
}
bool CellInfo::canTravelXNeg() {
	return itsCanTravel[XNEG];
}
bool CellInfo::canTravelXPos() {
	return itsCanTravel[XPOS];
}
bool CellInfo::canTravelYNeg() {
	return itsCanTravel[YNEG];
}
bool CellInfo::canTravelYPos() {
	return itsCanTravel[YPOS];
}
bool CellInfo::canTravelZNeg() {
	return itsCanTravel[ZNEG];
}
bool CellInfo::canTravelZPos() {
	return itsCanTravel[ZPOS];
}