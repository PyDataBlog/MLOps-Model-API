using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Matrix
{
    public class Matrix<T>
         where T : struct
    {
        private T[,] matrix;

        public Matrix(int x, int y)
        {
            matrix = new T[x, y];
        }

        public int LengthX
        {
            get
            {
                return this.matrix.GetLength(0);
            }
        }
        public int LengthY
        {
            get
            {
                return this.matrix.GetLength(1);
            }
        }
        public T this [int x, int y]
        {
            get
            {
                if (matrix.GetLength(0) > x &&  matrix.GetLength(1) > y)
                {
                    return this.matrix[x, y];    
                }
                else
                {
                    throw new IndexOutOfRangeException();
                }
            }
            set
            {
                if (matrix.GetLength(0) > x && matrix.GetLength(1) > y)
                {
                    this.matrix[x, y] = value;
                }
                else
                {
                    throw new IndexOutOfRangeException();
                }
            }
        }

        private static bool IsMatricesTheSameSize(Matrix<T> matrix1, Matrix<T> matrix2)
        {
            if (matrix1.matrix.GetLength(0) == matrix2.matrix.GetLength(0) && matrix1.matrix.GetLength(1) == matrix2.matrix.GetLength(1))
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        private static bool IsMultiplicationPossible(Matrix<T> matrix1, Matrix<T> matrix2)
        {
            if (matrix1.LengthX == matrix2.LengthY && matrix1.LengthY == matrix2.LengthX)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        public static Matrix<T> operator +(Matrix<T> matrix1, Matrix<T> matrix2)
        {
            Matrix<T> newMatrix = new Matrix<T>(matrix1.LengthX, matrix1.LengthY);
            dynamic matrice1 = matrix1;
            dynamic matrice2 = matrix2;

            if (Matrix<T>.IsMatricesTheSameSize(matrix1, matrix2))
            {
                for (int i = 0; i < matrix1.LengthX; i++)
                {
                    for (int j = 0; j < matrix1.LengthY; j++)
                    {
                        newMatrix[i, j] = matrice1[i, j] + matrice2[i, j];
                    }
                }
            }
            else
            {
                throw new InvalidOperationException();
            }

            return newMatrix;
        }

        public static Matrix<T> operator -(Matrix<T> matrix1, Matrix<T> matrix2)
        {
            Matrix<T> newMatrix = new Matrix<T>(matrix1.LengthX, matrix1.LengthY);
            dynamic matrice1 = matrix1;
            dynamic matrice2 = matrix2;

            if (Matrix<T>.IsMatricesTheSameSize(matrix1, matrix2))
            {
                for (int i = 0; i < matrix1.LengthX; i++)
                {
                    for (int j = 0; j < matrix1.LengthY; j++)
                    {
                        newMatrix[i, j] = matrice1[i, j] - matrice2[i, j];
                    }
                }
            }
            else
            {
                throw new InvalidOperationException();
            }

            return newMatrix;
        }

        public static Matrix<T> operator *(Matrix<T> matrix1, Matrix<T> matrix2)
        {
            Matrix<T> newMatrix = new Matrix<T>(matrix1.LengthX, matrix2.LengthY);
            dynamic matrice1 = matrix1;
            dynamic matrice2 = matrix2;

            if (Matrix<T>.IsMultiplicationPossible(matrix1, matrix2))
            {
                for (int i = 0; i < matrix1.LengthX; i++)
                {
                    for (int j = 0; j < matrix1.LengthY; j++)
                    {
                        newMatrix[i, j] = matrice1[i, j] * matrice2[j, i];
                    }
                }
            }
            else
            {
                throw new InvalidOperationException();
            }

            return newMatrix;
        }

        public static bool operator true(T elementOfMetrix)
        {
            return elementOfMetrix.Equals(default(T));
        }
    }
}
