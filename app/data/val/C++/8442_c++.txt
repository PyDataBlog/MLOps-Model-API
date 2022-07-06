// Generated from /POI/java/org/apache/poi/ss/formula/functions/Sumx2my2.java

#pragma once

#include <fwd-POI.hpp>
#include <org/apache/poi/ss/formula/functions/fwd-POI.hpp>
#include <java/lang/Object.hpp>
#include <org/apache/poi/ss/formula/functions/XYNumericFunction_Accumulator.hpp>

struct default_init_tag;

class poi::ss::formula::functions::Sumx2my2_1
    : public virtual ::java::lang::Object
    , public virtual XYNumericFunction_Accumulator
{

public:
    typedef ::java::lang::Object super;
    double accumulate(double x, double y) override;

    // Generated
    Sumx2my2_1();
    static ::java::lang::Class *class_();

private:
    virtual ::java::lang::Class* getClass0();
    friend class Sumx2my2;
};
