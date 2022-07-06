/* 
 * File:   CRSMatrix.cpp
 * Author: chachalaca
 * 
 * Created on 19. duben 2013, 20:19
 */

#include "CRSMatrix.h"


#include <iostream>
#include <stdexcept>
#include <stdlib.h>

using namespace std;

/**
 
 */

CRSMatrix::CRSMatrix(int size) {
        i.assign(size+1, 0);
}

CRSMatrix::CRSMatrix(const CRSMatrix& orig) {
}

CRSMatrix::~CRSMatrix() {
}

/**
 * 
 * @param
 * @return 
 */
CRSMatrix* CRSMatrix::dump() {
    
    for (int it=0; it<aij.size(); it++)
		{
                cout << " [" << it << "] ";
		cout << aij.at(it) << ", ";
                
		}
    cout << "\n\n";
    
    for (int it=0; it< j.size(); it++)
		{
                cout << " [" << it << "] ";
		cout << j.at(it) << ", ";
                
		}
    cout << "\n\n";
    
    for (int it=0; it< i.size(); it++)
		{
                cout << " [" << it << "] ";
		cout << i.at(it) << ", ";
                
		}
    cout << "\n\n";

    return this;
}

/**
 * Vraci rozmer matice
 * @param void
 * @return int size
 */
int CRSMatrix::getSize() {    
    return i.size()-1;
}

/**
 * Nastavi prvek x(radek) y(sloupec) na zadanou hodnotu. 
 * @param int x radkovy index
 * @param int y sloupcovy index
 * @param int value
 * @return CRSMatrix* fluent interface
 * @throws std::exception
 * @throws std::invalid_argument
 */
CRSMatrix* CRSMatrix::setElement(int x, int y, int value) {
    if(x >= this->getSize()) {
        throw std::invalid_argument( "neplatny rozsah v argumentu x " );
    }
    if(y >= this->getSize()) {
        throw std::invalid_argument( "neplatny rozsah v argumentu y " );
    }
    int pos;
    int *positionPointer;
    positionPointer = &pos;
    
    if(value == 0) {
        this->removeElement(x,y);
        return this;
    }
    if(this->findPosition(x, y, positionPointer)) {
        aij.at(*positionPointer) = value;    
        return this;
    }
    else {
        aij.insert((aij.begin().operator +(*positionPointer)), value );
        j.insert( (j.begin().operator +(*positionPointer)), y );
        
        if(x < i.size()-1) {                
            for (int it = x+1; it < i.size(); it++) {
                i.at(it)++;
            }
        }
        return this;
    }
    
}

/**
 * Ziska hodnotu prvku matice x(radek) y(sloupec)
 * @param int x radkovy index
 * @param int y sloupcovy index
 * @return int value
 * @throws std::invalid_argument
 */
int CRSMatrix::getElement(int x, int y) {
    if(x >= this->getSize()) {
        throw std::invalid_argument( "neplatny rozsah v argumentu x " );
    }
    if(y >= this->getSize()) {
        throw std::invalid_argument( "neplatny rozsah v argumentu y " );
    }
    int pos;
    int *positionPointer;
    positionPointer = &pos;
    
    if(this->findPosition(x, y, positionPointer)) {
        return aij.at(*positionPointer);
    }
    else {
        return 0;
    }
           
}

CRSMatrix* CRSMatrix::removeElement(int x, int y) {
    int pos;
    int *positionPointer;
    positionPointer = &pos;
    
    if(this->findPosition(x, y, positionPointer)) {
        aij.erase((aij.begin().operator +(*positionPointer)));
        j.erase( (j.begin().operator +(*positionPointer)));
        
        if(x < i.size()-1) {                
            for (int it = x+1; it < i.size(); it++) {
                i.at(it)--;
            }
        }
        return this;
    }
    else {
        return this;
    }
}

/**
 * Zleva matici vynasobi zadany vektor a vrati vysledek.
 * @param std::vector<int> v
 * @return std::vector<int> result
 * @throws std::invalid_argument
 */
std::vector<int> CRSMatrix::matrixVectorProduct(std::vector<int> v) {
    if(v.size() != this->getSize()) {
        throw std::invalid_argument( "std::vector v musi byt rozmeru jako matice." );
    }
    std::vector<int> result;
    result.assign(v.size(), 0);
    
    int it;
    for (it = 0; it < this->getSize(); it++) {
        for (int it2 = i.at(it); it2 < i.at(it+1); it2++) {
            result.at(it) += aij.at(it2)*v.at(j.at(it2));
        }
    }
    
    return result;
}

/**
 * K matici pricte zadanou matici.
 * @param CRSMatrix* m
 * @return CRSMatrix* fluent interface
 * @throws std::invalid_argument
 */
CRSMatrix* CRSMatrix::matrixAddition(CRSMatrix* m) {
    if(m->getSize() != this->getSize()) {
        throw std::invalid_argument( "obe matice musi byt stejnych rozmeru" );
    }
    for (int it = 0; it < this->getSize(); it++) {
        for (int it2 = 0; it2 < this->getSize(); it2++) {
            this->setElement(it, it2, this->getElement(it, it2) + m->getElement(it, it2));
        }
    }
    return this;
}

/**
 * Matici zprava vynasobi zadanou matici
 * @param CRSMatrix* m
 * @return CRSMatrix* fluent interface
 * @throws std::invalid_argument
 */
CRSMatrix* CRSMatrix::matrixMatrixProduct(CRSMatrix* m) {
    if(m->getSize() != this->getSize()) {
        throw std::invalid_argument( "obe matice musi byt stejnych rozmeru" );
    }
    int sum;
    CRSMatrix *cache;    
    cache = new CRSMatrix(this->getSize());
    
    for (int it = 0; it < this->getSize(); it++) {
        for (int it2 = 0; it2 < this->getSize(); it2++) {
            sum = 0;
            for (int it3 = 0; it3 < this->getSize(); it3++) {
                sum += (this->getElement(it, it3) * m->getElement(it3, it2));
            }
            cache->setElement(it, it2, sum);
        }
    }
    
    for (int it = 0; it < this->getSize(); it++) {
        for (int it2 = 0; it2 < this->getSize(); it2++) {
            this->setElement(it, it2, cache->getElement(it, it2));
        }
    }
    
    free(cache);
    
    return this;
}

/**
 * Vraci true pokud byl prvek v CRS nalezen - je nelulovy. Do ukazatele zapise jeho pozici v j a aij.
 * Pokud neexistuje, vraci false a do ukazatele zapise pozici pro vlozeni.
 * @param int x radkovy index
 * @param int y sloupcovy index
 * @param int* positionPointer ukazatel na pozici
 * @return bool
 */
bool CRSMatrix::findPosition(int x, int y, int* positionPointer) {
        for (int it = i.at(x); it < i.at(x+1); it++) {
            if(j.at(it) == y) {
                *positionPointer = it;
                return true;
            }
            if(j.at(it) > y) {
                *positionPointer = it;
                return false;
            }        
        }
        *positionPointer = i.at(x+1);
        return false;
 
}



