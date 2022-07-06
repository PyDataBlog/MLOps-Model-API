// Generated from /POI/java/org/apache/poi/hssf/record/RecordFactory.java
#include <org/apache/poi/hssf/record/RecordFactory_ReflectionMethodRecordCreator.hpp>

#include <java/lang/Class.hpp>
#include <java/lang/ClassCastException.hpp>
#include <java/lang/IllegalAccessException.hpp>
#include <java/lang/IllegalArgumentException.hpp>
#include <java/lang/NullPointerException.hpp>
#include <java/lang/Object.hpp>
#include <java/lang/RuntimeException.hpp>
#include <java/lang/String.hpp>
#include <java/lang/Throwable.hpp>
#include <java/lang/reflect/InvocationTargetException.hpp>
#include <java/lang/reflect/Method.hpp>
#include <org/apache/poi/hssf/record/Record.hpp>
#include <org/apache/poi/hssf/record/RecordInputStream.hpp>
#include <org/apache/poi/util/RecordFormatException.hpp>
#include <ObjectArray.hpp>

template<typename T, typename U>
static T java_cast(U* u)
{
    if(!u) return static_cast<T>(nullptr);
    auto t = dynamic_cast<T>(u);
    if(!t) throw new ::java::lang::ClassCastException();
    return t;
}

template<typename T>
static T* npc(T* t)
{
    if(!t) throw new ::java::lang::NullPointerException();
    return t;
}

poi::hssf::record::RecordFactory_ReflectionMethodRecordCreator::RecordFactory_ReflectionMethodRecordCreator(const ::default_init_tag&)
    : super(*static_cast< ::default_init_tag* >(0))
{
    clinit();
}

poi::hssf::record::RecordFactory_ReflectionMethodRecordCreator::RecordFactory_ReflectionMethodRecordCreator(::java::lang::reflect::Method* m) 
    : RecordFactory_ReflectionMethodRecordCreator(*static_cast< ::default_init_tag* >(0))
{
    ctor(m);
}

void poi::hssf::record::RecordFactory_ReflectionMethodRecordCreator::ctor(::java::lang::reflect::Method* m)
{
    super::ctor();
    _m = m;
}

poi::hssf::record::Record* poi::hssf::record::RecordFactory_ReflectionMethodRecordCreator::create(RecordInputStream* in)
{
    auto args = (new ::java::lang::ObjectArray({static_cast< ::java::lang::Object* >(in)}));
    try {
        return java_cast< Record* >(npc(_m)->invoke(nullptr, args));
    } catch (::java::lang::IllegalArgumentException* e) {
        throw new ::java::lang::RuntimeException(static_cast< ::java::lang::Throwable* >(e));
    } catch (::java::lang::IllegalAccessException* e) {
        throw new ::java::lang::RuntimeException(static_cast< ::java::lang::Throwable* >(e));
    } catch (::java::lang::reflect::InvocationTargetException* e) {
        throw new ::poi::util::RecordFormatException(u"Unable to construct record instance"_j, npc(e)->getTargetException());
    }
}

java::lang::Class* poi::hssf::record::RecordFactory_ReflectionMethodRecordCreator::getRecordClass()
{
    return java_cast< ::java::lang::Class* >(npc(_m)->getDeclaringClass());
}

extern java::lang::Class *class_(const char16_t *c, int n);

java::lang::Class* poi::hssf::record::RecordFactory_ReflectionMethodRecordCreator::class_()
{
    static ::java::lang::Class* c = ::class_(u"org.apache.poi.hssf.record.RecordFactory.ReflectionMethodRecordCreator", 70);
    return c;
}

java::lang::Class* poi::hssf::record::RecordFactory_ReflectionMethodRecordCreator::getClass0()
{
    return class_();
}

