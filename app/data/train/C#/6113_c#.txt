using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sciencetific_Calc
{
    class Bishop : Piece
    {
        public override bool Move(Coordinate oldCoordinate, Coordinate newCoordinate)
        {
            bool checkMove = false;

            checkMove = VerifyDiagonals(oldCoordinate, newCoordinate, checkMove);

            return checkMove;
        }

        public static bool VerifyDiagonals(Coordinate oldCoordinate, Coordinate newCoordinate, bool checkMove)
        {
            List<Coordinate> pieceRoute = new List<Coordinate>();

            if (newCoordinate.XCoordinate - oldCoordinate.XCoordinate == newCoordinate.YCoordinate - oldCoordinate.YCoordinate)
            {
                if (newCoordinate.XCoordinate > oldCoordinate.XCoordinate && newCoordinate.YCoordinate > oldCoordinate.YCoordinate)
                {
                    int newY = oldCoordinate.YCoordinate + 1;
                    int newX = oldCoordinate.XCoordinate + 1;

                    while (true)
                    {
                        pieceRoute.Add(new Coordinate() { XCoordinate = newX, YCoordinate = newY });
                        if (newY == newCoordinate.YCoordinate)
                        {
                            break;
                        }
                        newY++;
                        newX++;
                    }
                }

                else if (newCoordinate.XCoordinate < oldCoordinate.XCoordinate && newCoordinate.YCoordinate < oldCoordinate.YCoordinate)
                {
                    int newY = oldCoordinate.YCoordinate - 1;
                    int newX = oldCoordinate.XCoordinate - 1;

                    while (true)
                    {
                        pieceRoute.Add(new Coordinate() { XCoordinate = newX, YCoordinate = newY });
                        if (newY == newCoordinate.YCoordinate)
                        {
                            break;
                        }
                        newY--;
                        newX--;
                    }
                }
            }

            else if (oldCoordinate.XCoordinate - newCoordinate.XCoordinate == newCoordinate.YCoordinate - oldCoordinate.YCoordinate)
            {
                if (newCoordinate.XCoordinate > oldCoordinate.XCoordinate && oldCoordinate.YCoordinate > newCoordinate.YCoordinate)
                {
                    int newY = oldCoordinate.YCoordinate - 1;
                    int newX = oldCoordinate.XCoordinate + 1;

                    while (true)
                    {
                        pieceRoute.Add(new Coordinate() { XCoordinate = newX, YCoordinate = newY });
                        if (newY == newCoordinate.YCoordinate)
                        {
                            break;
                        }
                        newY--;
                        newX++;
                    }
                }
                else
                {
                    int newY = oldCoordinate.YCoordinate + 1;
                    int newX = oldCoordinate.XCoordinate - 1;

                    while (true)
                    {
                        pieceRoute.Add(new Coordinate() { XCoordinate = newX, YCoordinate = newY });
                        if (newY == newCoordinate.YCoordinate)
                        {
                            break;
                        }
                        newY++;
                        newX--;
                    }
                }
            }

            foreach (Coordinate coordinate in pieceRoute)
            {
                List<PieceTracker> pieceListAtCoordinate = (from piece in chessBoard.game.PieceList
                                                            where piece.Coordinate.XCoordinate.Equals(coordinate.XCoordinate)
                                                            & piece.Coordinate.YCoordinate.Equals(coordinate.YCoordinate)
                                                            select piece).ToList();

                if (pieceListAtCoordinate.Count == 0)
                {
                    checkMove = true;
                }

                else if (pieceListAtCoordinate.Count != 0)
                {
                    if (newCoordinate.XCoordinate == coordinate.XCoordinate && newCoordinate.YCoordinate == coordinate.YCoordinate)
                    {
                        checkMove = true;
                    }

                    else
                    {
                        checkMove = false;
                        break;
                    }
                }

            }
            return checkMove;
        }
    }
}
