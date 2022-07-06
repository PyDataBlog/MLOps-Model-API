package rover

import coordinates.Coordinates
import directions.rover.{NorthDirection, RoverDirection}

case class RoverPosition(roverCoordinates: Coordinates = Coordinates(0,0), roverDirection: RoverDirection = NorthDirection)