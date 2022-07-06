#include "resistor.h"

Resistor::Resistor(){
    this->setResistencia(0);
}

Resistor::Resistor(float resistencia_inicial)
{
    this->setResistencia(resistencia_inicial);
}

Resistor Resistor::operator+ (Resistor r2)
{
    Resistor resistorToReturn(this->getResistencia() + r2.getResistencia());
    return resistorToReturn;
}

Resistor Resistor::operator|| (Resistor r2)
{
    float resistencia_paralelo =
          (this->getResistencia()*r2.getResistencia())
            /(this->getResistencia()+r2.getResistencia());

    return Resistor(resistencia_paralelo);
}
