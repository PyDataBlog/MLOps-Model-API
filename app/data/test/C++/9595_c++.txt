#include "Derivative.h"

using namespace rraLink;

#include "Derivative_p.h"
DerivativePrivate::DerivativePrivate()
{
    preValue = 0;
    caption = "Derivative";
}

Derivative::Derivative():
    Link(new DerivativePrivate())
{
}

Derivative::Derivative(DerivativePrivate* d_ptr):
    Link(d_ptr)
{
}

double Derivative::out(double in)
{
    D(Derivative)
    double dt = (in - d->preValue) / d->step;
    d->preValue = in;
    return dt;
}

void Derivative::reset()
{
    D(Derivative)
    d->preValue = 0;
}
