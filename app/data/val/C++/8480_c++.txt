// Generated from /POI/java/org/apache/poi/hssf/record/chart/ChartTitleFormatRecord.java
#include <org/apache/poi/hssf/record/chart/ChartTitleFormatRecord.hpp>

#include <java/lang/ArrayStoreException.hpp>
#include <java/lang/NullPointerException.hpp>
#include <java/lang/String.hpp>
#include <java/lang/StringBuffer.hpp>
#include <org/apache/poi/hssf/record/RecordInputStream.hpp>
#include <org/apache/poi/hssf/record/chart/ChartTitleFormatRecord_CTFormat.hpp>
#include <org/apache/poi/util/LittleEndianOutput.hpp>
#include <Array.hpp>
#include <ObjectArray.hpp>
#include <SubArray.hpp>

template<typename ComponentType, typename... Bases> struct SubArray;
namespace poi
{
    namespace hssf
    {
        namespace record
        {
            namespace chart
            {
typedef ::SubArray< ::poi::hssf::record::chart::ChartTitleFormatRecord_CTFormat, ::java::lang::ObjectArray > ChartTitleFormatRecord_CTFormatArray;
            } // chart
        } // record
    } // hssf
} // poi

template<typename T>
static T* npc(T* t)
{
    if(!t) throw new ::java::lang::NullPointerException();
    return t;
}

poi::hssf::record::chart::ChartTitleFormatRecord::ChartTitleFormatRecord(const ::default_init_tag&)
    : super(*static_cast< ::default_init_tag* >(0))
{
    clinit();
}

poi::hssf::record::chart::ChartTitleFormatRecord::ChartTitleFormatRecord(::poi::hssf::record::RecordInputStream* in) 
    : ChartTitleFormatRecord(*static_cast< ::default_init_tag* >(0))
{
    ctor(in);
}

constexpr int16_t poi::hssf::record::chart::ChartTitleFormatRecord::sid;

void poi::hssf::record::chart::ChartTitleFormatRecord::ctor(::poi::hssf::record::RecordInputStream* in)
{
    super::ctor();
    auto nRecs = npc(in)->readUShort();
    _formats = new ChartTitleFormatRecord_CTFormatArray(nRecs);
    for (auto i = int32_t(0); i < nRecs; i++) {
        _formats->set(i, new ChartTitleFormatRecord_CTFormat(in));
    }
}

void poi::hssf::record::chart::ChartTitleFormatRecord::serialize(::poi::util::LittleEndianOutput* out)
{
    npc(out)->writeShort(npc(_formats)->length);
    for (auto i = int32_t(0); i < npc(_formats)->length; i++) {
        npc((*_formats)[i])->serialize(out);
    }
}

int32_t poi::hssf::record::chart::ChartTitleFormatRecord::getDataSize()
{
    return int32_t(2) + ChartTitleFormatRecord_CTFormat::ENCODED_SIZE * npc(_formats)->length;
}

int16_t poi::hssf::record::chart::ChartTitleFormatRecord::getSid()
{
    return sid;
}

int32_t poi::hssf::record::chart::ChartTitleFormatRecord::getFormatCount()
{
    return npc(_formats)->length;
}

void poi::hssf::record::chart::ChartTitleFormatRecord::modifyFormatRun(int16_t oldPos, int16_t newLen)
{
    auto shift = int32_t(0);
    for (auto i = int32_t(0); i < npc(_formats)->length; i++) {
        auto ctf = (*_formats)[i];
        if(shift != 0) {
            npc(ctf)->setOffset(npc(ctf)->getOffset() + shift);
        } else if(oldPos == npc(ctf)->getOffset() && i < npc(_formats)->length - int32_t(1)) {
            auto nextCTF = (*_formats)[i + int32_t(1)];
            shift = newLen - (npc(nextCTF)->getOffset() - npc(ctf)->getOffset());
        }
    }
}

java::lang::String* poi::hssf::record::chart::ChartTitleFormatRecord::toString()
{
    auto buffer = new ::java::lang::StringBuffer();
    npc(buffer)->append(u"[CHARTTITLEFORMAT]\n"_j);
    npc(npc(npc(buffer)->append(u"    .format_runs       = "_j))->append(npc(_formats)->length))->append(u"\n"_j);
    for (auto i = int32_t(0); i < npc(_formats)->length; i++) {
        auto ctf = (*_formats)[i];
        npc(npc(buffer)->append(u"       .char_offset= "_j))->append(npc(ctf)->getOffset());
        npc(npc(buffer)->append(u",.fontidx= "_j))->append(npc(ctf)->getFontIndex());
        npc(buffer)->append(u"\n"_j);
    }
    npc(buffer)->append(u"[/CHARTTITLEFORMAT]\n"_j);
    return npc(buffer)->toString();
}

extern java::lang::Class *class_(const char16_t *c, int n);

java::lang::Class* poi::hssf::record::chart::ChartTitleFormatRecord::class_()
{
    static ::java::lang::Class* c = ::class_(u"org.apache.poi.hssf.record.chart.ChartTitleFormatRecord", 55);
    return c;
}

int32_t poi::hssf::record::chart::ChartTitleFormatRecord::serialize(int32_t offset, ::int8_tArray* data)
{
    return super::serialize(offset, data);
}

int8_tArray* poi::hssf::record::chart::ChartTitleFormatRecord::serialize()
{
    return super::serialize();
}

java::lang::Class* poi::hssf::record::chart::ChartTitleFormatRecord::getClass0()
{
    return class_();
}

