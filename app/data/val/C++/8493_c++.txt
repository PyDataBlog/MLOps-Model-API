// Generated from /Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/rt.jar
#include <java/util/TreeMap_DescendingKeySpliterator.hpp>

extern void unimplemented_(const char16_t* name);
java::util::TreeMap_DescendingKeySpliterator::TreeMap_DescendingKeySpliterator(const ::default_init_tag&)
    : super(*static_cast< ::default_init_tag* >(0))
{
    clinit();
}

java::util::TreeMap_DescendingKeySpliterator::TreeMap_DescendingKeySpliterator(TreeMap* tree, TreeMap_Entry* origin, TreeMap_Entry* fence, int32_t side, int32_t est, int32_t expectedModCount)
    : TreeMap_DescendingKeySpliterator(*static_cast< ::default_init_tag* >(0))
{
    ctor(tree, origin, fence, side, est, expectedModCount);
}


void ::java::util::TreeMap_DescendingKeySpliterator::ctor(TreeMap* tree, TreeMap_Entry* origin, TreeMap_Entry* fence, int32_t side, int32_t est, int32_t expectedModCount)
{ /* stub */
    /* super::ctor(); */
    unimplemented_(u"void ::java::util::TreeMap_DescendingKeySpliterator::ctor(TreeMap* tree, TreeMap_Entry* origin, TreeMap_Entry* fence, int32_t side, int32_t est, int32_t expectedModCount)");
}

int32_t java::util::TreeMap_DescendingKeySpliterator::characteristics()
{ /* stub */
    unimplemented_(u"int32_t java::util::TreeMap_DescendingKeySpliterator::characteristics()");
    return 0;
}

void java::util::TreeMap_DescendingKeySpliterator::forEachRemaining(::java::util::function::Consumer* action)
{ /* stub */
    unimplemented_(u"void java::util::TreeMap_DescendingKeySpliterator::forEachRemaining(::java::util::function::Consumer* action)");
}

bool java::util::TreeMap_DescendingKeySpliterator::tryAdvance(::java::util::function::Consumer* action)
{ /* stub */
    unimplemented_(u"bool java::util::TreeMap_DescendingKeySpliterator::tryAdvance(::java::util::function::Consumer* action)");
    return 0;
}

java::util::TreeMap_DescendingKeySpliterator* java::util::TreeMap_DescendingKeySpliterator::trySplit()
{ /* stub */
    unimplemented_(u"java::util::TreeMap_DescendingKeySpliterator* java::util::TreeMap_DescendingKeySpliterator::trySplit()");
    return 0;
}

extern java::lang::Class *class_(const char16_t *c, int n);

java::lang::Class* java::util::TreeMap_DescendingKeySpliterator::class_()
{
    static ::java::lang::Class* c = ::class_(u"java.util.TreeMap.DescendingKeySpliterator", 42);
    return c;
}

int64_t java::util::TreeMap_DescendingKeySpliterator::estimateSize()
{
    return TreeMap_TreeMapSpliterator::estimateSize();
}

java::lang::Class* java::util::TreeMap_DescendingKeySpliterator::getClass0()
{
    return class_();
}

