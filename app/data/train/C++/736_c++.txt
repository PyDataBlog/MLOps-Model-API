// Generated from /POI/java/org/apache/poi/hssf/record/DBCellRecord.java

#pragma once

#include <fwd-POI.hpp>
#include <java/lang/fwd-POI.hpp>
#include <org/apache/poi/hssf/record/fwd-POI.hpp>
#include <org/apache/poi/util/fwd-POI.hpp>
#include <org/apache/poi/hssf/record/StandardRecord.hpp>
#include <java/lang/Cloneable.hpp>

struct default_init_tag;

class poi::hssf::record::DBCellRecord final
    : public StandardRecord
    , public ::java::lang::Cloneable
{

public:
    typedef StandardRecord super;
    static constexpr int16_t sid { int16_t(215) };
    static constexpr int32_t BLOCK_SIZE { int32_t(32) };

private:
    int32_t field_1_row_offset {  };
    ::int16_tArray* field_2_cell_offsets {  };
protected:
    void ctor(int32_t rowOffset, ::int16_tArray* cellOffsets);
    void ctor(RecordInputStream* in);

public:
    ::java::lang::String* toString() override;
    void serialize(::poi::util::LittleEndianOutput* out) override;

public: /* protected */
    int32_t getDataSize() override;

public:
    int16_t getSid() override;
    DBCellRecord* clone() override;

    // Generated

public: /* package */
    DBCellRecord(int32_t rowOffset, ::int16_tArray* cellOffsets);

public:
    DBCellRecord(RecordInputStream* in);
protected:
    DBCellRecord(const ::default_init_tag&);


public:
    static ::java::lang::Class *class_();
    int32_t serialize(int32_t offset, ::int8_tArray* data);
    ::int8_tArray* serialize();

private:
    virtual ::java::lang::Class* getClass0();
    friend class DBCellRecord_Builder;
};
