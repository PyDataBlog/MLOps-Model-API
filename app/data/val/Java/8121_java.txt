/*########################################################################
 *#                                                                      #
 *#                      Copyright (c) 2014 by                           #
 *#          Shanghai Stock Exchange (SSE), Shanghai, China              #
 *#                       All rights reserved.                           #
 *#                                                                      #
 *########################################################################
 */
package sse.ngts.common.plugin.step.business;

import sse.ngts.common.plugin.step.*;
import sse.ngts.common.plugin.step.field.*;

public class QuoteCancel extends Message {
    private static final long serialVersionUID = 20130819;
	public static final String MSGTYPE = "Z";

    public QuoteCancel() {
        super();
        getHeader().setField(new MsgType(MSGTYPE));
    }

    public QuoteCancel(int[] fieldOrder) {
        super(fieldOrder);
        getHeader().setField(new MsgType(MSGTYPE));
    }

    public void set(OrderQty value) {
        setField(value);
    }

    public OrderQty get(OrderQty value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public OrderQty getOrderQty() throws FieldNotFound {
        OrderQty value = new OrderQty();
        getField(value);
        return value;
    }

    public boolean isSet(OrderQty field) {
        return isSetField(field);
    }

    public boolean isSetOrderQty() {
        return isSetField(OrderQty.FIELD);
    }

    public void set(OrigClOrdID value) {
        setField(value);
    }

    public OrigClOrdID get(OrigClOrdID value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public OrigClOrdID getOrigClOrdID() throws FieldNotFound {
        OrigClOrdID value = new OrigClOrdID();
        getField(value);
        return value;
    }

    public boolean isSet(OrigClOrdID field) {
        return isSetField(field);
    }

    public boolean isSetOrigClOrdID() {
        return isSetField(OrigClOrdID.FIELD);
    }

    public void set(SecurityID value) {
        setField(value);
    }

    public SecurityID get(SecurityID value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public SecurityID getSecurityID() throws FieldNotFound {
        SecurityID value = new SecurityID();
        getField(value);
        return value;
    }

    public boolean isSet(SecurityID field) {
        return isSetField(field);
    }

    public boolean isSetSecurityID() {
        return isSetField(SecurityID.FIELD);
    }

    public void set(Side value) {
        setField(value);
    }

    public Side get(Side value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public Side getSide() throws FieldNotFound {
        Side value = new Side();
        getField(value);
        return value;
    }

    public boolean isSet(Side field) {
        return isSetField(field);
    }

    public boolean isSetSide() {
        return isSetField(Side.FIELD);
    }

    public void set(Text value) {
        setField(value);
    }

    public Text get(Text value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public Text getText() throws FieldNotFound {
        Text value = new Text();
        getField(value);
        return value;
    }

    public boolean isSet(Text field) {
        return isSetField(field);
    }

    public boolean isSetText() {
        return isSetField(Text.FIELD);
    }

    public void set(TradeDate value) {
        setField(value);
    }

    public TradeDate get(TradeDate value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public TradeDate getTradeDate() throws FieldNotFound {
        TradeDate value = new TradeDate();
        getField(value);
        return value;
    }

    public boolean isSet(TradeDate field) {
        return isSetField(field);
    }

    public boolean isSetTradeDate() {
        return isSetField(TradeDate.FIELD);
    }

    public void set(QuoteID value) {
        setField(value);
    }

    public QuoteID get(QuoteID value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public QuoteID getQuoteID() throws FieldNotFound {
        QuoteID value = new QuoteID();
        getField(value);
        return value;
    }

    public boolean isSet(QuoteID field) {
        return isSetField(field);
    }

    public boolean isSetQuoteID() {
        return isSetField(QuoteID.FIELD);
    }

    public void set(OrderQty2 value) {
        setField(value);
    }

    public OrderQty2 get(OrderQty2 value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public OrderQty2 getOrderQty2() throws FieldNotFound {
        OrderQty2 value = new OrderQty2();
        getField(value);
        return value;
    }

    public boolean isSet(OrderQty2 field) {
        return isSetField(field);
    }

    public boolean isSetOrderQty2() {
        return isSetField(OrderQty2.FIELD);
    }

    public void set(NoPartyIDs value) {
        setField(value);
    }

    public NoPartyIDs get(NoPartyIDs value) throws FieldNotFound {
        getField(value);
        return value;
    }

    public NoPartyIDs getNoPartyIDs() throws FieldNotFound {
        NoPartyIDs value = new NoPartyIDs();
        getField(value);
        return value;
    }

    public boolean isSet(NoPartyIDs field) {
        return isSetField(field);
    }

    public boolean isSetNoPartyIDs() {
        return isSetField(NoPartyIDs.FIELD);
    }

}
