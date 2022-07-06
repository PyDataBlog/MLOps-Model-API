// Generated from /POI/java/org/apache/poi/hssf/record/BoundSheetRecord.java
#include <org/apache/poi/hssf/record/BoundSheetRecord.hpp>

#include <java/lang/ArrayStoreException.hpp>
#include <java/lang/NullPointerException.hpp>
#include <java/lang/String.hpp>
#include <java/lang/StringBuffer.hpp>
#include <java/util/Arrays.hpp>
#include <java/util/Comparator.hpp>
#include <java/util/List.hpp>
#include <org/apache/poi/hssf/record/BoundSheetRecord_1.hpp>
#include <org/apache/poi/hssf/record/Record.hpp>
#include <org/apache/poi/hssf/record/RecordBase.hpp>
#include <org/apache/poi/hssf/record/RecordInputStream.hpp>
#include <org/apache/poi/hssf/record/StandardRecord.hpp>
#include <org/apache/poi/ss/util/WorkbookUtil.hpp>
#include <org/apache/poi/util/BitField.hpp>
#include <org/apache/poi/util/BitFieldFactory.hpp>
#include <org/apache/poi/util/HexDump.hpp>
#include <org/apache/poi/util/LittleEndian.hpp>
#include <org/apache/poi/util/LittleEndianConsts.hpp>
#include <org/apache/poi/util/LittleEndianOutput.hpp>
#include <org/apache/poi/util/StringUtil.hpp>
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
typedef ::SubArray< ::poi::hssf::record::RecordBase, ::java::lang::ObjectArray > RecordBaseArray;
typedef ::SubArray< ::poi::hssf::record::Record, RecordBaseArray > RecordArray;
typedef ::SubArray< ::poi::hssf::record::StandardRecord, RecordArray > StandardRecordArray;
typedef ::SubArray< ::poi::hssf::record::BoundSheetRecord, StandardRecordArray > BoundSheetRecordArray;
        } // record
    } // hssf
} // poi

template<typename T>
static T* npc(T* t)
{
    if(!t) throw new ::java::lang::NullPointerException();
    return t;
}

poi::hssf::record::BoundSheetRecord::BoundSheetRecord(const ::default_init_tag&)
    : super(*static_cast< ::default_init_tag* >(0))
{
    clinit();
}

poi::hssf::record::BoundSheetRecord::BoundSheetRecord(::java::lang::String* sheetname) 
    : BoundSheetRecord(*static_cast< ::default_init_tag* >(0))
{
    ctor(sheetname);
}

poi::hssf::record::BoundSheetRecord::BoundSheetRecord(RecordInputStream* in) 
    : BoundSheetRecord(*static_cast< ::default_init_tag* >(0))
{
    ctor(in);
}

constexpr int16_t poi::hssf::record::BoundSheetRecord::sid;

poi::util::BitField*& poi::hssf::record::BoundSheetRecord::hiddenFlag()
{
    clinit();
    return hiddenFlag_;
}
poi::util::BitField* poi::hssf::record::BoundSheetRecord::hiddenFlag_;

poi::util::BitField*& poi::hssf::record::BoundSheetRecord::veryHiddenFlag()
{
    clinit();
    return veryHiddenFlag_;
}
poi::util::BitField* poi::hssf::record::BoundSheetRecord::veryHiddenFlag_;

void poi::hssf::record::BoundSheetRecord::ctor(::java::lang::String* sheetname)
{
    super::ctor();
    field_2_option_flags = 0;
    setSheetname(sheetname);
}

void poi::hssf::record::BoundSheetRecord::ctor(RecordInputStream* in)
{
    super::ctor();
    auto buf = new ::int8_tArray(::poi::util::LittleEndianConsts::INT_SIZE);
    npc(in)->readPlain(buf, int32_t(0), npc(buf)->length);
    field_1_position_of_BOF = ::poi::util::LittleEndian::getInt(buf);
    field_2_option_flags = npc(in)->readUShort();
    auto field_3_sheetname_length = npc(in)->readUByte();
    field_4_isMultibyteUnicode = npc(in)->readByte();
    if(isMultibyte()) {
        field_5_sheetname = npc(in)->readUnicodeLEString(field_3_sheetname_length);
    } else {
        field_5_sheetname = npc(in)->readCompressedUnicode(field_3_sheetname_length);
    }
}

void poi::hssf::record::BoundSheetRecord::setPositionOfBof(int32_t pos)
{
    field_1_position_of_BOF = pos;
}

void poi::hssf::record::BoundSheetRecord::setSheetname(::java::lang::String* sheetName)
{
    ::poi::ss::util::WorkbookUtil::validateSheetName(sheetName);
    field_5_sheetname = sheetName;
    field_4_isMultibyteUnicode = ::poi::util::StringUtil::hasMultibyte(sheetName) ? int32_t(1) : int32_t(0);
}

int32_t poi::hssf::record::BoundSheetRecord::getPositionOfBof()
{
    return field_1_position_of_BOF;
}

bool poi::hssf::record::BoundSheetRecord::isMultibyte()
{
    return (field_4_isMultibyteUnicode & int32_t(1)) != 0;
}

java::lang::String* poi::hssf::record::BoundSheetRecord::getSheetname()
{
    return field_5_sheetname;
}

java::lang::String* poi::hssf::record::BoundSheetRecord::toString()
{
    auto buffer = new ::java::lang::StringBuffer();
    npc(buffer)->append(u"[BOUNDSHEET]\n"_j);
    npc(npc(npc(buffer)->append(u"    .bof        = "_j))->append(::poi::util::HexDump::intToHex(getPositionOfBof())))->append(u"\n"_j);
    npc(npc(npc(buffer)->append(u"    .options    = "_j))->append(::poi::util::HexDump::shortToHex(field_2_option_flags)))->append(u"\n"_j);
    npc(npc(npc(buffer)->append(u"    .unicodeflag= "_j))->append(::poi::util::HexDump::byteToHex(field_4_isMultibyteUnicode)))->append(u"\n"_j);
    npc(npc(npc(buffer)->append(u"    .sheetname  = "_j))->append(field_5_sheetname))->append(u"\n"_j);
    npc(buffer)->append(u"[/BOUNDSHEET]\n"_j);
    return npc(buffer)->toString();
}

int32_t poi::hssf::record::BoundSheetRecord::getDataSize()
{
    return int32_t(8) + npc(field_5_sheetname)->length() * (isMultibyte() ? int32_t(2) : int32_t(1));
}

void poi::hssf::record::BoundSheetRecord::serialize(::poi::util::LittleEndianOutput* out)
{
    npc(out)->writeInt(getPositionOfBof());
    npc(out)->writeShort(field_2_option_flags);
    auto name = field_5_sheetname;
    npc(out)->writeByte(npc(name)->length());
    npc(out)->writeByte(field_4_isMultibyteUnicode);
    if(isMultibyte()) {
        ::poi::util::StringUtil::putUnicodeLE(name, out);
    } else {
        ::poi::util::StringUtil::putCompressedUnicode(name, out);
    }
}

int16_t poi::hssf::record::BoundSheetRecord::getSid()
{
    return sid;
}

bool poi::hssf::record::BoundSheetRecord::isHidden()
{
    return npc(hiddenFlag_)->isSet(field_2_option_flags);
}

void poi::hssf::record::BoundSheetRecord::setHidden(bool hidden)
{
    field_2_option_flags = npc(hiddenFlag_)->setBoolean(field_2_option_flags, hidden);
}

bool poi::hssf::record::BoundSheetRecord::isVeryHidden()
{
    return npc(veryHiddenFlag_)->isSet(field_2_option_flags);
}

void poi::hssf::record::BoundSheetRecord::setVeryHidden(bool veryHidden)
{
    field_2_option_flags = npc(veryHiddenFlag_)->setBoolean(field_2_option_flags, veryHidden);
}

poi::hssf::record::BoundSheetRecordArray* poi::hssf::record::BoundSheetRecord::orderByBofPosition(::java::util::List* boundSheetRecords)
{
    clinit();
    auto bsrs = new BoundSheetRecordArray(npc(boundSheetRecords)->size());
    npc(boundSheetRecords)->toArray_(static_cast< ::java::lang::ObjectArray* >(bsrs));
    ::java::util::Arrays::sort(bsrs, BOFComparator_);
    return bsrs;
}

java::util::Comparator*& poi::hssf::record::BoundSheetRecord::BOFComparator()
{
    clinit();
    return BOFComparator_;
}
java::util::Comparator* poi::hssf::record::BoundSheetRecord::BOFComparator_;

extern java::lang::Class *class_(const char16_t *c, int n);

java::lang::Class* poi::hssf::record::BoundSheetRecord::class_()
{
    static ::java::lang::Class* c = ::class_(u"org.apache.poi.hssf.record.BoundSheetRecord", 43);
    return c;
}

void poi::hssf::record::BoundSheetRecord::clinit()
{
    super::clinit();
    static bool in_cl_init = false;
struct clinit_ {
    clinit_() {
        in_cl_init = true;
        hiddenFlag_ = ::poi::util::BitFieldFactory::getInstance(1);
        veryHiddenFlag_ = ::poi::util::BitFieldFactory::getInstance(2);
        BOFComparator_ = new BoundSheetRecord_1();
    }
};

    if(!in_cl_init) {
        static clinit_ clinit_instance;
    }
}

int32_t poi::hssf::record::BoundSheetRecord::serialize(int32_t offset, ::int8_tArray* data)
{
    return super::serialize(offset, data);
}

int8_tArray* poi::hssf::record::BoundSheetRecord::serialize()
{
    return super::serialize();
}

java::lang::Class* poi::hssf::record::BoundSheetRecord::getClass0()
{
    return class_();
}

