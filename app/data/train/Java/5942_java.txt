/*
 * Copyright 2004-2013 the Seasar Foundation and the Others.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */
package org.docksidestage.mysql.dbflute.allcommon;

import java.util.*;

import org.dbflute.exception.ClassificationNotFoundException;
import org.dbflute.jdbc.Classification;
import org.dbflute.jdbc.ClassificationCodeType;
import org.dbflute.jdbc.ClassificationMeta;
import org.dbflute.jdbc.ClassificationUndefinedHandlingType;
import org.dbflute.optional.OptionalThing;
import static org.dbflute.util.DfTypeUtil.emptyStrings;

/**
 * The definition of classification.
 * @author DBFlute(AutoGenerator)
 */
public interface CDef extends Classification {

    /**
     * 会員が受けられるサービスのランクを示す
     */
    public enum ServiceRank implements CDef {
        /** PLATINUM: platinum rank */
        Platinum("PLT", "PLATINUM", emptyStrings())
        ,
        /** GOLD: gold rank */
        Gold("GLD", "GOLD", emptyStrings())
        ,
        /** SILVER: silver rank */
        Silver("SIL", "SILVER", emptyStrings())
        ,
        /** BRONZE: bronze rank */
        Bronze("BRZ", "BRONZE", emptyStrings())
        ,
        /** PLASTIC: plastic rank (deprecated: テーブル区分値の非推奨要素指定のテストのため) */
        @Deprecated
        Plastic("PLS", "PLASTIC", emptyStrings())
        ;
        private static final Map<String, ServiceRank> _codeClsMap = new HashMap<String, ServiceRank>();
        private static final Map<String, ServiceRank> _nameClsMap = new HashMap<String, ServiceRank>();
        static {
            for (ServiceRank value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private ServiceRank(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.ServiceRank; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<ServiceRank> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof ServiceRank) { return OptionalThing.of((ServiceRank)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<ServiceRank> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static ServiceRank codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof ServiceRank) { return (ServiceRank)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static ServiceRank nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<ServiceRank> listAll() {
            return new ArrayList<ServiceRank>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<ServiceRank> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: ServiceRank." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<ServiceRank> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<ServiceRank> clsList = new ArrayList<ServiceRank>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<ServiceRank> groupOf(String groupName) {
            return new ArrayList<ServiceRank>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * mainly region of member address
     */
    public enum Region implements CDef {
        /** アメリカ */
        アメリカ("1", "アメリカ", emptyStrings())
        ,
        /** カナダ */
        カナダ("2", "カナダ", emptyStrings())
        ,
        /** 中国 */
        中国("3", "中国", emptyStrings())
        ,
        /** 千葉 */
        千葉("4", "千葉", emptyStrings())
        ;
        private static final Map<String, Region> _codeClsMap = new HashMap<String, Region>();
        private static final Map<String, Region> _nameClsMap = new HashMap<String, Region>();
        static {
            for (Region value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private Region(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.Region; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<Region> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof Region) { return OptionalThing.of((Region)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<Region> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static Region codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof Region) { return (Region)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static Region nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<Region> listAll() {
            return new ArrayList<Region>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<Region> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: Region." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<Region> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<Region> clsList = new ArrayList<Region>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<Region> groupOf(String groupName) {
            return new ArrayList<Region>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * reason for member withdrawal
     */
    public enum WithdrawalReason implements CDef {
        /** SIT: サイトが使いにくいから */
        Sit("SIT", "SIT", emptyStrings())
        ,
        /** PRD: 商品に魅力がないから */
        Prd("PRD", "PRD", emptyStrings())
        ,
        /** FRT: フリテンだから */
        Frt("FRT", "FRT", emptyStrings())
        ,
        /** OTH: その他理由 */
        Oth("OTH", "OTH", emptyStrings())
        ;
        private static final Map<String, WithdrawalReason> _codeClsMap = new HashMap<String, WithdrawalReason>();
        private static final Map<String, WithdrawalReason> _nameClsMap = new HashMap<String, WithdrawalReason>();
        static {
            for (WithdrawalReason value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private WithdrawalReason(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.WithdrawalReason; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<WithdrawalReason> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof WithdrawalReason) { return OptionalThing.of((WithdrawalReason)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<WithdrawalReason> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static WithdrawalReason codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof WithdrawalReason) { return (WithdrawalReason)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static WithdrawalReason nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<WithdrawalReason> listAll() {
            return new ArrayList<WithdrawalReason>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<WithdrawalReason> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: WithdrawalReason." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<WithdrawalReason> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<WithdrawalReason> clsList = new ArrayList<WithdrawalReason>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<WithdrawalReason> groupOf(String groupName) {
            return new ArrayList<WithdrawalReason>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * 支払方法
     */
    public enum PaymentMethod implements CDef {
        /** 手渡し: Face-to-Faceの手渡しで商品と交換 */
        ByHand("HAN", "手渡し", emptyStrings())
        ,
        /** 銀行振込: 銀行振込で確認してから商品発送 */
        BankTransfer("BAK", "銀行振込", emptyStrings())
        ,
        /** クレジットカード: クレジットカードの番号を教えてもらう */
        CreditCard("CRC", "クレジットカード", emptyStrings())
        ;
        private static final Map<String, PaymentMethod> _codeClsMap = new HashMap<String, PaymentMethod>();
        private static final Map<String, PaymentMethod> _nameClsMap = new HashMap<String, PaymentMethod>();
        static {
            for (PaymentMethod value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private PaymentMethod(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.PaymentMethod; }

        /**
         * Is the classification in the group? <br>
         * 最も推奨されている方法 <br>
         * The group elements:[ByHand]
         * @return The determination, true or false.
         */
        public boolean isRecommended() {
            return ByHand.equals(this);
        }

        public boolean inGroup(String groupName) {
            if ("recommended".equals(groupName)) { return isRecommended(); }
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<PaymentMethod> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof PaymentMethod) { return OptionalThing.of((PaymentMethod)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<PaymentMethod> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static PaymentMethod codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof PaymentMethod) { return (PaymentMethod)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static PaymentMethod nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<PaymentMethod> listAll() {
            return new ArrayList<PaymentMethod>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<PaymentMethod> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            if ("recommended".equalsIgnoreCase(groupName)) { return listOfRecommended(); }
            throw new ClassificationNotFoundException("Unknown classification group: PaymentMethod." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<PaymentMethod> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<PaymentMethod> clsList = new ArrayList<PaymentMethod>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * 最も推奨されている方法 <br>
         * The group elements:[ByHand]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<PaymentMethod> listOfRecommended() {
            return new ArrayList<PaymentMethod>(Arrays.asList(ByHand));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<PaymentMethod> groupOf(String groupName) {
            if ("recommended".equals(groupName)) { return listOfRecommended(); }
            return new ArrayList<PaymentMethod>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * the test of reference variable in grouping map
     */
    public enum GroupingReference implements CDef {
        /** LAND_NAME */
        LAND_NAME("LND", "LAND_NAME", emptyStrings())
        ,
        /** SEA_NAME */
        SEA_NAME("SEA", "SEA_NAME", emptyStrings())
        ,
        /** IKSPIARY_NAME */
        IKSPIARY_NAME("IKS", "IKSPIARY_NAME", emptyStrings())
        ,
        /** AMPHI_NAME */
        AMPHI_NAME("AMP", "AMPHI_NAME", emptyStrings())
        ;
        private static final Map<String, GroupingReference> _codeClsMap = new HashMap<String, GroupingReference>();
        private static final Map<String, GroupingReference> _nameClsMap = new HashMap<String, GroupingReference>();
        static {
            for (GroupingReference value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private GroupingReference(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.GroupingReference; }

        /**
         * Is the classification in the group? <br>
         * サービスが利用できる会員 <br>
         * The group elements:[LAND_NAME, SEA_NAME]
         * @return The determination, true or false.
         */
        public boolean isServiceAvailable() {
            return LAND_NAME.equals(this) || SEA_NAME.equals(this);
        }

        /**
         * Is the classification in the group? <br>
         * The group elements:[LAND_NAME, SEA_NAME, IKSPIARY_NAME]
         * @return The determination, true or false.
         */
        public boolean isServicePlus() {
            return LAND_NAME.equals(this) || SEA_NAME.equals(this) || IKSPIARY_NAME.equals(this);
        }

        /**
         * Is the classification in the group? <br>
         * The group elements:[AMPHI_NAME, LAND_NAME, SEA_NAME, IKSPIARY_NAME]
         * @return The determination, true or false.
         */
        public boolean isNestedPlus() {
            return AMPHI_NAME.equals(this) || LAND_NAME.equals(this) || SEA_NAME.equals(this) || IKSPIARY_NAME.equals(this);
        }

        /**
         * Is the classification in the group? <br>
         * The group elements:[IKSPIARY_NAME]
         * @return The determination, true or false.
         */
        public boolean isOneDef() {
            return IKSPIARY_NAME.equals(this);
        }

        /**
         * Is the classification in the group? <br>
         * The group elements:[LAND_NAME, SEA_NAME, IKSPIARY_NAME]
         * @return The determination, true or false.
         */
        public boolean isDupRef() {
            return LAND_NAME.equals(this) || SEA_NAME.equals(this) || IKSPIARY_NAME.equals(this);
        }

        public boolean inGroup(String groupName) {
            if ("serviceAvailable".equals(groupName)) { return isServiceAvailable(); }
            if ("servicePlus".equals(groupName)) { return isServicePlus(); }
            if ("nestedPlus".equals(groupName)) { return isNestedPlus(); }
            if ("oneDef".equals(groupName)) { return isOneDef(); }
            if ("dupRef".equals(groupName)) { return isDupRef(); }
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<GroupingReference> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof GroupingReference) { return OptionalThing.of((GroupingReference)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<GroupingReference> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static GroupingReference codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof GroupingReference) { return (GroupingReference)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static GroupingReference nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<GroupingReference> listAll() {
            return new ArrayList<GroupingReference>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<GroupingReference> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            if ("serviceAvailable".equalsIgnoreCase(groupName)) { return listOfServiceAvailable(); }
            if ("servicePlus".equalsIgnoreCase(groupName)) { return listOfServicePlus(); }
            if ("nestedPlus".equalsIgnoreCase(groupName)) { return listOfNestedPlus(); }
            if ("oneDef".equalsIgnoreCase(groupName)) { return listOfOneDef(); }
            if ("dupRef".equalsIgnoreCase(groupName)) { return listOfDupRef(); }
            throw new ClassificationNotFoundException("Unknown classification group: GroupingReference." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<GroupingReference> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<GroupingReference> clsList = new ArrayList<GroupingReference>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * サービスが利用できる会員 <br>
         * The group elements:[LAND_NAME, SEA_NAME]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<GroupingReference> listOfServiceAvailable() {
            return new ArrayList<GroupingReference>(Arrays.asList(LAND_NAME, SEA_NAME));
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * The group elements:[LAND_NAME, SEA_NAME, IKSPIARY_NAME]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<GroupingReference> listOfServicePlus() {
            return new ArrayList<GroupingReference>(Arrays.asList(LAND_NAME, SEA_NAME, IKSPIARY_NAME));
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * The group elements:[AMPHI_NAME, LAND_NAME, SEA_NAME, IKSPIARY_NAME]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<GroupingReference> listOfNestedPlus() {
            return new ArrayList<GroupingReference>(Arrays.asList(AMPHI_NAME, LAND_NAME, SEA_NAME, IKSPIARY_NAME));
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * The group elements:[IKSPIARY_NAME]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<GroupingReference> listOfOneDef() {
            return new ArrayList<GroupingReference>(Arrays.asList(IKSPIARY_NAME));
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * The group elements:[LAND_NAME, SEA_NAME, IKSPIARY_NAME]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<GroupingReference> listOfDupRef() {
            return new ArrayList<GroupingReference>(Arrays.asList(LAND_NAME, SEA_NAME, IKSPIARY_NAME));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<GroupingReference> groupOf(String groupName) {
            if ("serviceAvailable".equals(groupName)) { return listOfServiceAvailable(); }
            if ("servicePlus".equals(groupName)) { return listOfServicePlus(); }
            if ("nestedPlus".equals(groupName)) { return listOfNestedPlus(); }
            if ("oneDef".equals(groupName)) { return listOfOneDef(); }
            if ("dupRef".equals(groupName)) { return listOfDupRef(); }
            return new ArrayList<GroupingReference>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * The test of relation reference
     */
    public enum SelfReference implements CDef {
        /** foo801 */
        Foo801("801", "foo801", emptyStrings())
        ,
        /** foo811 */
        Foo811("811", "foo811", emptyStrings())
        ,
        /** bar802: 0 */
        Bar802("802", "bar802", emptyStrings())
        ,
        /** baz803: 0 */
        Baz803("803", "baz803", emptyStrings())
        ,
        /** bar812: 0 */
        Bar812("812", "bar812", emptyStrings())
        ,
        /** baz813: 0 */
        Baz813("813", "baz813", emptyStrings())
        ;
        private static final Map<String, SelfReference> _codeClsMap = new HashMap<String, SelfReference>();
        private static final Map<String, SelfReference> _nameClsMap = new HashMap<String, SelfReference>();
        static {
            for (SelfReference value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private SelfReference(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.SelfReference; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<SelfReference> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof SelfReference) { return OptionalThing.of((SelfReference)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<SelfReference> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static SelfReference codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof SelfReference) { return (SelfReference)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static SelfReference nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<SelfReference> listAll() {
            return new ArrayList<SelfReference>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<SelfReference> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: SelfReference." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<SelfReference> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<SelfReference> clsList = new ArrayList<SelfReference>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<SelfReference> groupOf(String groupName) {
            return new ArrayList<SelfReference>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * The test of top only
     */
    public enum TopCommentOnly implements CDef {
        ;
        private static final Map<String, TopCommentOnly> _codeClsMap = new HashMap<String, TopCommentOnly>();
        private static final Map<String, TopCommentOnly> _nameClsMap = new HashMap<String, TopCommentOnly>();
        static {
            for (TopCommentOnly value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private TopCommentOnly(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.TopCommentOnly; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<TopCommentOnly> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof TopCommentOnly) { return OptionalThing.of((TopCommentOnly)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<TopCommentOnly> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static TopCommentOnly codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof TopCommentOnly) { return (TopCommentOnly)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static TopCommentOnly nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<TopCommentOnly> listAll() {
            return new ArrayList<TopCommentOnly>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<TopCommentOnly> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: TopCommentOnly." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<TopCommentOnly> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<TopCommentOnly> clsList = new ArrayList<TopCommentOnly>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<TopCommentOnly> groupOf(String groupName) {
            return new ArrayList<TopCommentOnly>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * the test of sub-item map for implicit classification
     */
    public enum SubItemImplicit implements CDef {
        /** Aaa: means foo */
        Foo("FOO", "Aaa", emptyStrings())
        ,
        /** Bbb: means bar */
        Bar("BAR", "Bbb", emptyStrings())
        ;
        private static final Map<String, SubItemImplicit> _codeClsMap = new HashMap<String, SubItemImplicit>();
        private static final Map<String, SubItemImplicit> _nameClsMap = new HashMap<String, SubItemImplicit>();
        static {
            for (SubItemImplicit value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private static final Map<String, Map<String, Object>> _subItemMapMap = new HashMap<String, Map<String, Object>>();
        static {
            {
                Map<String, Object> subItemMap = new HashMap<String, Object>();
                subItemMap.put("regularStringItem", "value1<tag>");
                subItemMap.put("regularNumberItem", "123");
                subItemMap.put("regularVariousItem", "list:{\n    ; reg\n    ; var\n    ; ite\n}");
                subItemMap.put("listItem", "list:{\n    ; aa\n    ; bb\n    ; cc\n}");
                _subItemMapMap.put(Foo.code(), Collections.unmodifiableMap(subItemMap));
            }
            {
                Map<String, Object> subItemMap = new HashMap<String, Object>();
                subItemMap.put("regularStringItem", "value2<teg>");
                subItemMap.put("regularNumberItem", "456");
                subItemMap.put("regularVariousItem", "map:{\n    ; reg = var\n    ; ous = ite\n}");
                subItemMap.put("mapItem", "map:{\n    ; key11 = value11\n}");
                subItemMap.put("containsLine", "va\nlue");
                _subItemMapMap.put(Bar.code(), Collections.unmodifiableMap(subItemMap));
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private SubItemImplicit(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return _subItemMapMap.get(code()); }
        public ClassificationMeta meta() { return CDef.DefMeta.SubItemImplicit; }

        public String regularStringItem() {
            return (String)subItemMap().get("regularStringItem");
        }

        public String regularNumberItem() {
            return (String)subItemMap().get("regularNumberItem");
        }

        public Object regularVariousItem() {
            return subItemMap().get("regularVariousItem");
        }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<SubItemImplicit> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof SubItemImplicit) { return OptionalThing.of((SubItemImplicit)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<SubItemImplicit> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static SubItemImplicit codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof SubItemImplicit) { return (SubItemImplicit)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static SubItemImplicit nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<SubItemImplicit> listAll() {
            return new ArrayList<SubItemImplicit>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<SubItemImplicit> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: SubItemImplicit." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<SubItemImplicit> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<SubItemImplicit> clsList = new ArrayList<SubItemImplicit>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<SubItemImplicit> groupOf(String groupName) {
            return new ArrayList<SubItemImplicit>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * the test of sub-item map for table classification
     */
    public enum SubItemTable implements CDef {
        /** 正式会員: 正式な会員としてサイトサービスが利用可能 */
        正式会員("FML", "正式会員", emptyStrings())
        ,
        /** 退会会員: 退会が確定した会員でサイトサービスはダメ */
        退会会員("WDL", "退会会員", emptyStrings())
        ,
        /** 仮会員: 入会直後のステータスで一部のサイトサービスが利用可能 */
        仮会員("PRV", "仮会員", emptyStrings())
        ;
        private static final Map<String, SubItemTable> _codeClsMap = new HashMap<String, SubItemTable>();
        private static final Map<String, SubItemTable> _nameClsMap = new HashMap<String, SubItemTable>();
        static {
            for (SubItemTable value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private static final Map<String, Map<String, Object>> _subItemMapMap = new HashMap<String, Map<String, Object>>();
        static {
            {
                Map<String, Object> subItemMap = new HashMap<String, Object>();
                subItemMap.put("key1", "1");
                subItemMap.put("key2", "正式会員");
                subItemMap.put("key3", null);
                _subItemMapMap.put(正式会員.code(), Collections.unmodifiableMap(subItemMap));
            }
            {
                Map<String, Object> subItemMap = new HashMap<String, Object>();
                subItemMap.put("key1", "2");
                subItemMap.put("key2", "退会会員");
                subItemMap.put("key3", null);
                _subItemMapMap.put(退会会員.code(), Collections.unmodifiableMap(subItemMap));
            }
            {
                Map<String, Object> subItemMap = new HashMap<String, Object>();
                subItemMap.put("key1", "3");
                subItemMap.put("key2", "仮会員");
                subItemMap.put("key3", null);
                _subItemMapMap.put(仮会員.code(), Collections.unmodifiableMap(subItemMap));
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private SubItemTable(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return _subItemMapMap.get(code()); }
        public ClassificationMeta meta() { return CDef.DefMeta.SubItemTable; }

        public String key1() {
            return (String)subItemMap().get("key1");
        }

        public String key2() {
            return (String)subItemMap().get("key2");
        }

        public String key3() {
            return (String)subItemMap().get("key3");
        }

        /**
         * Is the classification in the group? <br>
         * サービスが利用できる会員 <br>
         * The group elements:[正式会員, 仮会員]
         * @return The determination, true or false.
         */
        public boolean isServiceAvailable() {
            return 正式会員.equals(this) || 仮会員.equals(this);
        }

        /**
         * Is the classification in the group? <br>
         * The group elements:[退会会員]
         * @return The determination, true or false.
         */
        public boolean isLastestStatus() {
            return 退会会員.equals(this);
        }

        public boolean inGroup(String groupName) {
            if ("serviceAvailable".equals(groupName)) { return isServiceAvailable(); }
            if ("lastestStatus".equals(groupName)) { return isLastestStatus(); }
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<SubItemTable> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof SubItemTable) { return OptionalThing.of((SubItemTable)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<SubItemTable> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static SubItemTable codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof SubItemTable) { return (SubItemTable)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static SubItemTable nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<SubItemTable> listAll() {
            return new ArrayList<SubItemTable>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<SubItemTable> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            if ("serviceAvailable".equalsIgnoreCase(groupName)) { return listOfServiceAvailable(); }
            if ("lastestStatus".equalsIgnoreCase(groupName)) { return listOfLastestStatus(); }
            throw new ClassificationNotFoundException("Unknown classification group: SubItemTable." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<SubItemTable> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<SubItemTable> clsList = new ArrayList<SubItemTable>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * サービスが利用できる会員 <br>
         * The group elements:[正式会員, 仮会員]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<SubItemTable> listOfServiceAvailable() {
            return new ArrayList<SubItemTable>(Arrays.asList(正式会員, 仮会員));
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * The group elements:[退会会員]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<SubItemTable> listOfLastestStatus() {
            return new ArrayList<SubItemTable>(Arrays.asList(退会会員));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<SubItemTable> groupOf(String groupName) {
            if ("serviceAvailable".equals(groupName)) { return listOfServiceAvailable(); }
            if ("lastestStatus".equals(groupName)) { return listOfLastestStatus(); }
            return new ArrayList<SubItemTable>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * boolean classification for boolean column
     */
    public enum BooleanFlg implements CDef {
        /** Checked: means yes */
        True("true", "Checked", emptyStrings())
        ,
        /** Unchecked: means no */
        False("false", "Unchecked", emptyStrings())
        ;
        private static final Map<String, BooleanFlg> _codeClsMap = new HashMap<String, BooleanFlg>();
        private static final Map<String, BooleanFlg> _nameClsMap = new HashMap<String, BooleanFlg>();
        static {
            for (BooleanFlg value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private BooleanFlg(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.BooleanFlg; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<BooleanFlg> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof BooleanFlg) { return OptionalThing.of((BooleanFlg)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<BooleanFlg> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static BooleanFlg codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof BooleanFlg) { return (BooleanFlg)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static BooleanFlg nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<BooleanFlg> listAll() {
            return new ArrayList<BooleanFlg>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<BooleanFlg> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: BooleanFlg." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<BooleanFlg> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<BooleanFlg> clsList = new ArrayList<BooleanFlg>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<BooleanFlg> groupOf(String groupName) {
            return new ArrayList<BooleanFlg>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * master type of variant relation (biz-many-to-one)
     */
    public enum VariantRelationMasterType implements CDef {
        /** FooCls */
        FooCls("FOO", "FooCls", emptyStrings())
        ,
        /** BarCls */
        BarCls("BAR", "BarCls", emptyStrings())
        ,
        /** QuxCls */
        QuxCls("QUX", "QuxCls", emptyStrings())
        ,
        /** CorgeCls */
        CorgeCls("CORGE", "CorgeCls", emptyStrings())
        ;
        private static final Map<String, VariantRelationMasterType> _codeClsMap = new HashMap<String, VariantRelationMasterType>();
        private static final Map<String, VariantRelationMasterType> _nameClsMap = new HashMap<String, VariantRelationMasterType>();
        static {
            for (VariantRelationMasterType value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private VariantRelationMasterType(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.VariantRelationMasterType; }

        /**
         * Is the classification in the group? <br>
         * Foo or Bar or Qux <br>
         * The group elements:[FooCls, BarCls, QuxCls]
         * @return The determination, true or false.
         */
        public boolean isFooBarQux() {
            return FooCls.equals(this) || BarCls.equals(this) || QuxCls.equals(this);
        }

        public boolean inGroup(String groupName) {
            if ("fooBarQux".equals(groupName)) { return isFooBarQux(); }
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<VariantRelationMasterType> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof VariantRelationMasterType) { return OptionalThing.of((VariantRelationMasterType)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<VariantRelationMasterType> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static VariantRelationMasterType codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof VariantRelationMasterType) { return (VariantRelationMasterType)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static VariantRelationMasterType nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<VariantRelationMasterType> listAll() {
            return new ArrayList<VariantRelationMasterType>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<VariantRelationMasterType> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            if ("fooBarQux".equalsIgnoreCase(groupName)) { return listOfFooBarQux(); }
            throw new ClassificationNotFoundException("Unknown classification group: VariantRelationMasterType." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<VariantRelationMasterType> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<VariantRelationMasterType> clsList = new ArrayList<VariantRelationMasterType>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * Foo or Bar or Qux <br>
         * The group elements:[FooCls, BarCls, QuxCls]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<VariantRelationMasterType> listOfFooBarQux() {
            return new ArrayList<VariantRelationMasterType>(Arrays.asList(FooCls, BarCls, QuxCls));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<VariantRelationMasterType> groupOf(String groupName) {
            if ("fooBarQux".equals(groupName)) { return listOfFooBarQux(); }
            return new ArrayList<VariantRelationMasterType>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * qux type of variant relation (biz-many-to-one)
     */
    public enum VariantRelationQuxType implements CDef {
        /** Qua */
        Qua("Qua", "Qua", emptyStrings())
        ,
        /** Que */
        Que("Que", "Que", emptyStrings())
        ,
        /** Quo */
        Quo("Quo", "Quo", emptyStrings())
        ;
        private static final Map<String, VariantRelationQuxType> _codeClsMap = new HashMap<String, VariantRelationQuxType>();
        private static final Map<String, VariantRelationQuxType> _nameClsMap = new HashMap<String, VariantRelationQuxType>();
        static {
            for (VariantRelationQuxType value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private VariantRelationQuxType(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.VariantRelationQuxType; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<VariantRelationQuxType> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof VariantRelationQuxType) { return OptionalThing.of((VariantRelationQuxType)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<VariantRelationQuxType> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static VariantRelationQuxType codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof VariantRelationQuxType) { return (VariantRelationQuxType)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static VariantRelationQuxType nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<VariantRelationQuxType> listAll() {
            return new ArrayList<VariantRelationQuxType>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<VariantRelationQuxType> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: VariantRelationQuxType." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<VariantRelationQuxType> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<VariantRelationQuxType> clsList = new ArrayList<VariantRelationQuxType>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<VariantRelationQuxType> groupOf(String groupName) {
            return new ArrayList<VariantRelationQuxType>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * merged
     */
    public enum QuxCls implements CDef {
        /** Merged: merged qux element */
        Merged("MRG", "Merged", emptyStrings())
        ,
        /** QuxOne: QuxOne */
        QuxOne("Q01", "QuxOne", emptyStrings())
        ,
        /** QuxTwo: QuxTwo */
        QuxTwo("Q02", "QuxTwo", emptyStrings())
        ,
        /** QuxThree: QuxThree */
        QuxThree("Q03", "QuxThree", emptyStrings())
        ;
        private static final Map<String, QuxCls> _codeClsMap = new HashMap<String, QuxCls>();
        private static final Map<String, QuxCls> _nameClsMap = new HashMap<String, QuxCls>();
        static {
            for (QuxCls value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private QuxCls(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.QuxCls; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<QuxCls> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof QuxCls) { return OptionalThing.of((QuxCls)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<QuxCls> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static QuxCls codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof QuxCls) { return (QuxCls)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static QuxCls nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<QuxCls> listAll() {
            return new ArrayList<QuxCls>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<QuxCls> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: QuxCls." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<QuxCls> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<QuxCls> clsList = new ArrayList<QuxCls>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<QuxCls> groupOf(String groupName) {
            return new ArrayList<QuxCls>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * delimiter; &amp; endBrace} &amp; path\foo\bar
     */
    public enum EscapedDfpropCls implements CDef {
        /** First: delimiter &amp; rear escape char */
        First(";@\\", "First", emptyStrings())
        ,
        /** Second: escape char &amp; endBrace &amp; delimiter */
        Second("\\};", "Second", emptyStrings())
        ,
        /** Third: startBrace &amp; equal &amp; endBrace */
        Third("{=}", "Third", emptyStrings())
        ;
        private static final Map<String, EscapedDfpropCls> _codeClsMap = new HashMap<String, EscapedDfpropCls>();
        private static final Map<String, EscapedDfpropCls> _nameClsMap = new HashMap<String, EscapedDfpropCls>();
        static {
            for (EscapedDfpropCls value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private EscapedDfpropCls(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.EscapedDfpropCls; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<EscapedDfpropCls> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof EscapedDfpropCls) { return OptionalThing.of((EscapedDfpropCls)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<EscapedDfpropCls> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static EscapedDfpropCls codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof EscapedDfpropCls) { return (EscapedDfpropCls)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static EscapedDfpropCls nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<EscapedDfpropCls> listAll() {
            return new ArrayList<EscapedDfpropCls>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<EscapedDfpropCls> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: EscapedDfpropCls." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<EscapedDfpropCls> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<EscapedDfpropCls> clsList = new ArrayList<EscapedDfpropCls>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<EscapedDfpropCls> groupOf(String groupName) {
            return new ArrayList<EscapedDfpropCls>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * /*IF pmb.yourTop&#42;/&gt;&lt;&amp;
     */
    public enum EscapedJavaDocCls implements CDef {
        /** First: /*IF pmb.yourFooComment&#42;/&gt;&lt;&amp; */
        First("FOO", "First", emptyStrings())
        ,
        /** Second: /*IF pmb.yourBarComment&#42;/&gt;&lt;&amp; */
        Second("BAR", "Second", emptyStrings())
        ;
        private static final Map<String, EscapedJavaDocCls> _codeClsMap = new HashMap<String, EscapedJavaDocCls>();
        private static final Map<String, EscapedJavaDocCls> _nameClsMap = new HashMap<String, EscapedJavaDocCls>();
        static {
            for (EscapedJavaDocCls value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private EscapedJavaDocCls(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.EscapedJavaDocCls; }

        /**
         * Is the classification in the group? <br>
         * /*IF pmb.yourGroup&#42;/&gt;&lt;&amp; <br>
         * The group elements:[First, Second]
         * @return The determination, true or false.
         */
        public boolean isLineGroup() {
            return First.equals(this) || Second.equals(this);
        }

        public boolean inGroup(String groupName) {
            if ("lineGroup".equals(groupName)) { return isLineGroup(); }
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<EscapedJavaDocCls> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof EscapedJavaDocCls) { return OptionalThing.of((EscapedJavaDocCls)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<EscapedJavaDocCls> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static EscapedJavaDocCls codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof EscapedJavaDocCls) { return (EscapedJavaDocCls)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static EscapedJavaDocCls nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<EscapedJavaDocCls> listAll() {
            return new ArrayList<EscapedJavaDocCls>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<EscapedJavaDocCls> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            if ("lineGroup".equalsIgnoreCase(groupName)) { return listOfLineGroup(); }
            throw new ClassificationNotFoundException("Unknown classification group: EscapedJavaDocCls." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<EscapedJavaDocCls> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<EscapedJavaDocCls> clsList = new ArrayList<EscapedJavaDocCls>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * /*IF pmb.yourGroup&#42;/&gt;&lt;&amp; <br>
         * The group elements:[First, Second]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<EscapedJavaDocCls> listOfLineGroup() {
            return new ArrayList<EscapedJavaDocCls>(Arrays.asList(First, Second));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<EscapedJavaDocCls> groupOf(String groupName) {
            if ("lineGroup".equals(groupName)) { return listOfLineGroup(); }
            return new ArrayList<EscapedJavaDocCls>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * 6
     */
    public enum EscapedNumberInitialCls implements CDef {
        /** 1Foo */
        N1Foo("1FO", "1Foo", emptyStrings())
        ,
        /** 3Bar */
        N3Bar("3BA", "3Bar", emptyStrings())
        ,
        /** 7Qux */
        N7Qux("7QU", "7Qux", emptyStrings())
        ,
        /** Corge9 */
        Corge9("CO9", "Corge9", emptyStrings())
        ;
        private static final Map<String, EscapedNumberInitialCls> _codeClsMap = new HashMap<String, EscapedNumberInitialCls>();
        private static final Map<String, EscapedNumberInitialCls> _nameClsMap = new HashMap<String, EscapedNumberInitialCls>();
        static {
            for (EscapedNumberInitialCls value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private EscapedNumberInitialCls(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.EscapedNumberInitialCls; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<EscapedNumberInitialCls> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof EscapedNumberInitialCls) { return OptionalThing.of((EscapedNumberInitialCls)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<EscapedNumberInitialCls> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static EscapedNumberInitialCls codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof EscapedNumberInitialCls) { return (EscapedNumberInitialCls)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static EscapedNumberInitialCls nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<EscapedNumberInitialCls> listAll() {
            return new ArrayList<EscapedNumberInitialCls>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<EscapedNumberInitialCls> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: EscapedNumberInitialCls." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<EscapedNumberInitialCls> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<EscapedNumberInitialCls> clsList = new ArrayList<EscapedNumberInitialCls>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<EscapedNumberInitialCls> groupOf(String groupName) {
            return new ArrayList<EscapedNumberInitialCls>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * top first line            top second line            top third line
     */
    public enum LineSepCommentCls implements CDef {
        /** First: foo first line            foo second line */
        First("FOO", "First", emptyStrings())
        ,
        /** Second: bar first line            bar second line */
        Second("BAR", "Second", emptyStrings())
        ;
        private static final Map<String, LineSepCommentCls> _codeClsMap = new HashMap<String, LineSepCommentCls>();
        private static final Map<String, LineSepCommentCls> _nameClsMap = new HashMap<String, LineSepCommentCls>();
        static {
            for (LineSepCommentCls value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private LineSepCommentCls(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.LineSepCommentCls; }

        /**
         * Is the classification in the group? <br>
         * group first line                    group second line <br>
         * The group elements:[First, Second]
         * @return The determination, true or false.
         */
        public boolean isLineGroup() {
            return First.equals(this) || Second.equals(this);
        }

        public boolean inGroup(String groupName) {
            if ("lineGroup".equals(groupName)) { return isLineGroup(); }
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<LineSepCommentCls> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof LineSepCommentCls) { return OptionalThing.of((LineSepCommentCls)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<LineSepCommentCls> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static LineSepCommentCls codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof LineSepCommentCls) { return (LineSepCommentCls)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static LineSepCommentCls nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<LineSepCommentCls> listAll() {
            return new ArrayList<LineSepCommentCls>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<LineSepCommentCls> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            if ("lineGroup".equalsIgnoreCase(groupName)) { return listOfLineGroup(); }
            throw new ClassificationNotFoundException("Unknown classification group: LineSepCommentCls." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<LineSepCommentCls> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<LineSepCommentCls> clsList = new ArrayList<LineSepCommentCls>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * group first line                    group second line <br>
         * The group elements:[First, Second]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<LineSepCommentCls> listOfLineGroup() {
            return new ArrayList<LineSepCommentCls>(Arrays.asList(First, Second));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<LineSepCommentCls> groupOf(String groupName) {
            if ("lineGroup".equals(groupName)) { return listOfLineGroup(); }
            return new ArrayList<LineSepCommentCls>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * no camelizing classification
     */
    public enum NamingDefaultCamelizingType implements CDef {
        /** Bonvo */
        Bonvo("BONVO", "Bonvo", emptyStrings())
        ,
        /** dstore */
        Dstore("DSTORE", "dstore", emptyStrings())
        ,
        /** LAND陸oneman */
        LAND陸oneman("LAND", "LAND陸oneman", emptyStrings())
        ,
        /** PI AR-I */
        PiArI("PIARI", "PI AR-I", emptyStrings())
        ,
        /** SEA海MYSTIC */
        Sea海mystic("SEA", "SEA海MYSTIC", emptyStrings())
        ;
        private static final Map<String, NamingDefaultCamelizingType> _codeClsMap = new HashMap<String, NamingDefaultCamelizingType>();
        private static final Map<String, NamingDefaultCamelizingType> _nameClsMap = new HashMap<String, NamingDefaultCamelizingType>();
        static {
            for (NamingDefaultCamelizingType value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private NamingDefaultCamelizingType(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.NamingDefaultCamelizingType; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<NamingDefaultCamelizingType> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof NamingDefaultCamelizingType) { return OptionalThing.of((NamingDefaultCamelizingType)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<NamingDefaultCamelizingType> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static NamingDefaultCamelizingType codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof NamingDefaultCamelizingType) { return (NamingDefaultCamelizingType)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static NamingDefaultCamelizingType nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<NamingDefaultCamelizingType> listAll() {
            return new ArrayList<NamingDefaultCamelizingType>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<NamingDefaultCamelizingType> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: NamingDefaultCamelizingType." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<NamingDefaultCamelizingType> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<NamingDefaultCamelizingType> clsList = new ArrayList<NamingDefaultCamelizingType>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<NamingDefaultCamelizingType> groupOf(String groupName) {
            return new ArrayList<NamingDefaultCamelizingType>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * no camelizing classification
     */
    public enum NamingNoCamelizingType implements CDef {
        /** Bonvo */
        Bonvo("BONVO", "Bonvo", emptyStrings())
        ,
        /** dstore */
        dstore("DSTORE", "dstore", emptyStrings())
        ,
        /** LAND陸oneman */
        LAND陸oneman("LAND", "LAND陸oneman", emptyStrings())
        ,
        /** PI AR-I */
        PI_ARI("PIARI", "PI AR-I", emptyStrings())
        ,
        /** SEA海MYSTIC */
        SEA海MYSTIC("SEA", "SEA海MYSTIC", emptyStrings())
        ;
        private static final Map<String, NamingNoCamelizingType> _codeClsMap = new HashMap<String, NamingNoCamelizingType>();
        private static final Map<String, NamingNoCamelizingType> _nameClsMap = new HashMap<String, NamingNoCamelizingType>();
        static {
            for (NamingNoCamelizingType value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private NamingNoCamelizingType(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.NamingNoCamelizingType; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<NamingNoCamelizingType> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof NamingNoCamelizingType) { return OptionalThing.of((NamingNoCamelizingType)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<NamingNoCamelizingType> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static NamingNoCamelizingType codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof NamingNoCamelizingType) { return (NamingNoCamelizingType)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static NamingNoCamelizingType nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<NamingNoCamelizingType> listAll() {
            return new ArrayList<NamingNoCamelizingType>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<NamingNoCamelizingType> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: NamingNoCamelizingType." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<NamingNoCamelizingType> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<NamingNoCamelizingType> clsList = new ArrayList<NamingNoCamelizingType>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<NamingNoCamelizingType> groupOf(String groupName) {
            return new ArrayList<NamingNoCamelizingType>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * is deprecated classification
     */
    @Deprecated
    public enum DeprecatedTopBasicType implements CDef {
        /** FooName */
        FooName("FOO", "FooName", emptyStrings())
        ,
        /** BarName */
        BarName("BAR", "BarName", emptyStrings())
        ,
        /** QuxName */
        QuxName("QUX", "QuxName", emptyStrings())
        ;
        private static final Map<String, DeprecatedTopBasicType> _codeClsMap = new HashMap<String, DeprecatedTopBasicType>();
        private static final Map<String, DeprecatedTopBasicType> _nameClsMap = new HashMap<String, DeprecatedTopBasicType>();
        static {
            for (DeprecatedTopBasicType value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private DeprecatedTopBasicType(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.DeprecatedTopBasicType; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<DeprecatedTopBasicType> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof DeprecatedTopBasicType) { return OptionalThing.of((DeprecatedTopBasicType)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<DeprecatedTopBasicType> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static DeprecatedTopBasicType codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof DeprecatedTopBasicType) { return (DeprecatedTopBasicType)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static DeprecatedTopBasicType nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<DeprecatedTopBasicType> listAll() {
            return new ArrayList<DeprecatedTopBasicType>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<DeprecatedTopBasicType> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: DeprecatedTopBasicType." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<DeprecatedTopBasicType> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<DeprecatedTopBasicType> clsList = new ArrayList<DeprecatedTopBasicType>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<DeprecatedTopBasicType> groupOf(String groupName) {
            return new ArrayList<DeprecatedTopBasicType>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * has deprecated element
     */
    public enum DeprecatedMapBasicType implements CDef {
        /** FooName */
        FooName("FOO", "FooName", emptyStrings())
        ,
        /** BarName: (deprecated: test of deprecated) */
        @Deprecated
        BarName("BAR", "BarName", emptyStrings())
        ,
        /** QuxName */
        QuxName("QUX", "QuxName", emptyStrings())
        ;
        private static final Map<String, DeprecatedMapBasicType> _codeClsMap = new HashMap<String, DeprecatedMapBasicType>();
        private static final Map<String, DeprecatedMapBasicType> _nameClsMap = new HashMap<String, DeprecatedMapBasicType>();
        static {
            for (DeprecatedMapBasicType value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private DeprecatedMapBasicType(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.DeprecatedMapBasicType; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<DeprecatedMapBasicType> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof DeprecatedMapBasicType) { return OptionalThing.of((DeprecatedMapBasicType)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<DeprecatedMapBasicType> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static DeprecatedMapBasicType codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof DeprecatedMapBasicType) { return (DeprecatedMapBasicType)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static DeprecatedMapBasicType nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<DeprecatedMapBasicType> listAll() {
            return new ArrayList<DeprecatedMapBasicType>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<DeprecatedMapBasicType> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: DeprecatedMapBasicType." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<DeprecatedMapBasicType> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<DeprecatedMapBasicType> clsList = new ArrayList<DeprecatedMapBasicType>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<DeprecatedMapBasicType> groupOf(String groupName) {
            return new ArrayList<DeprecatedMapBasicType>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * has deprecated element
     */
    public enum DeprecatedMapCollaborationType implements CDef {
        /** FooName */
        FooName("FOO", "FooName", emptyStrings())
        ,
        /** BarBar: here (deprecated: test of deprecated) */
        @Deprecated
        BarName("BAR", "BarBar", emptyStrings())
        ,
        /** QuxQux: (deprecated: no original comment) */
        @Deprecated
        QuxName("QUX", "QuxQux", emptyStrings())
        ;
        private static final Map<String, DeprecatedMapCollaborationType> _codeClsMap = new HashMap<String, DeprecatedMapCollaborationType>();
        private static final Map<String, DeprecatedMapCollaborationType> _nameClsMap = new HashMap<String, DeprecatedMapCollaborationType>();
        static {
            for (DeprecatedMapCollaborationType value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private DeprecatedMapCollaborationType(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.DeprecatedMapCollaborationType; }

        /**
         * Is the classification in the group? <br>
         * contains deprecated element here <br>
         * The group elements:[FooName, BarName]
         * @return The determination, true or false.
         */
        public boolean isContainsDeprecated() {
            return FooName.equals(this) || BarName.equals(this);
        }

        public boolean inGroup(String groupName) {
            if ("containsDeprecated".equals(groupName)) { return isContainsDeprecated(); }
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<DeprecatedMapCollaborationType> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof DeprecatedMapCollaborationType) { return OptionalThing.of((DeprecatedMapCollaborationType)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<DeprecatedMapCollaborationType> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static DeprecatedMapCollaborationType codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof DeprecatedMapCollaborationType) { return (DeprecatedMapCollaborationType)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static DeprecatedMapCollaborationType nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<DeprecatedMapCollaborationType> listAll() {
            return new ArrayList<DeprecatedMapCollaborationType>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<DeprecatedMapCollaborationType> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            if ("containsDeprecated".equalsIgnoreCase(groupName)) { return listOfContainsDeprecated(); }
            throw new ClassificationNotFoundException("Unknown classification group: DeprecatedMapCollaborationType." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<DeprecatedMapCollaborationType> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<DeprecatedMapCollaborationType> clsList = new ArrayList<DeprecatedMapCollaborationType>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of group classification elements. (returns new copied list) <br>
         * contains deprecated element here <br>
         * The group elements:[FooName, BarName]
         * @return The snapshot list of classification elements in the group. (NotNull)
         */
        public static List<DeprecatedMapCollaborationType> listOfContainsDeprecated() {
            return new ArrayList<DeprecatedMapCollaborationType>(Arrays.asList(FooName, BarName));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<DeprecatedMapCollaborationType> groupOf(String groupName) {
            if ("containsDeprecated".equals(groupName)) { return listOfContainsDeprecated(); }
            return new ArrayList<DeprecatedMapCollaborationType>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * unique key as classification
     */
    public enum UQClassificationType implements CDef {
        ;
        private static final Map<String, UQClassificationType> _codeClsMap = new HashMap<String, UQClassificationType>();
        private static final Map<String, UQClassificationType> _nameClsMap = new HashMap<String, UQClassificationType>();
        static {
            for (UQClassificationType value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private UQClassificationType(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.UQClassificationType; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<UQClassificationType> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof UQClassificationType) { return OptionalThing.of((UQClassificationType)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<UQClassificationType> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static UQClassificationType codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof UQClassificationType) { return (UQClassificationType)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static UQClassificationType nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<UQClassificationType> listAll() {
            return new ArrayList<UQClassificationType>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<UQClassificationType> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: UQClassificationType." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<UQClassificationType> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<UQClassificationType> clsList = new ArrayList<UQClassificationType>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<UQClassificationType> groupOf(String groupName) {
            return new ArrayList<UQClassificationType>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * Classification of Bar
     */
    public enum BarCls implements CDef {
        /** BarOne: BarOne */
        BarOne("B01", "BarOne", emptyStrings())
        ,
        /** BarTwo: BarTwo */
        BarTwo("B02", "BarTwo", emptyStrings())
        ,
        /** BarThree: BarThree */
        BarThree("B03", "BarThree", emptyStrings())
        ,
        /** BarFour: BarFour */
        BarFour("B04", "BarFour", emptyStrings())
        ,
        /** BarFive: BarFive */
        BarFive("B05", "BarFive", emptyStrings())
        ;
        private static final Map<String, BarCls> _codeClsMap = new HashMap<String, BarCls>();
        private static final Map<String, BarCls> _nameClsMap = new HashMap<String, BarCls>();
        static {
            for (BarCls value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private BarCls(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.BarCls; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<BarCls> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof BarCls) { return OptionalThing.of((BarCls)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<BarCls> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static BarCls codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof BarCls) { return (BarCls)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static BarCls nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<BarCls> listAll() {
            return new ArrayList<BarCls>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<BarCls> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: BarCls." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<BarCls> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<BarCls> clsList = new ArrayList<BarCls>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<BarCls> groupOf(String groupName) {
            return new ArrayList<BarCls>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * Classification of Foo
     */
    public enum FooCls implements CDef {
        /** FooOne: FooOne */
        FooOne("F01", "FooOne", emptyStrings())
        ,
        /** FooTwo: FooTwo */
        FooTwo("F02", "FooTwo", emptyStrings())
        ,
        /** FooThree: FooThree */
        FooThree("F03", "FooThree", emptyStrings())
        ,
        /** FooFour: FooFour */
        FooFour("F04", "FooFour", emptyStrings())
        ;
        private static final Map<String, FooCls> _codeClsMap = new HashMap<String, FooCls>();
        private static final Map<String, FooCls> _nameClsMap = new HashMap<String, FooCls>();
        static {
            for (FooCls value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private FooCls(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.FooCls; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<FooCls> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof FooCls) { return OptionalThing.of((FooCls)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<FooCls> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static FooCls codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof FooCls) { return (FooCls)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static FooCls nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<FooCls> listAll() {
            return new ArrayList<FooCls>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<FooCls> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: FooCls." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<FooCls> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<FooCls> clsList = new ArrayList<FooCls>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<FooCls> groupOf(String groupName) {
            return new ArrayList<FooCls>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * フラグを示す
     */
    public enum Flg implements CDef {
        /** はい: 有効を示す */
        True("1", "はい", emptyStrings())
        ,
        /** いいえ: 無効を示す */
        False("0", "いいえ", emptyStrings())
        ;
        private static final Map<String, Flg> _codeClsMap = new HashMap<String, Flg>();
        private static final Map<String, Flg> _nameClsMap = new HashMap<String, Flg>();
        static {
            for (Flg value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private Flg(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.Flg; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<Flg> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof Flg) { return OptionalThing.of((Flg)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<Flg> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static Flg codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof Flg) { return (Flg)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static Flg nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<Flg> listAll() {
            return new ArrayList<Flg>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<Flg> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: Flg." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<Flg> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<Flg> clsList = new ArrayList<Flg>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<Flg> groupOf(String groupName) {
            return new ArrayList<Flg>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * 会員ステータス: 会員の状態を示す
     */
    public enum MemberStatus implements CDef {
        /** 正式会員: 正式な会員を示す */
        Formalized("FML", "正式会員", emptyStrings())
        ,
        /** 仮会員: 仮の会員を示す */
        Provisional("PRV", "仮会員", emptyStrings())
        ,
        /** 退会会員: 退会した会員を示す */
        Withdrawal("WDL", "退会会員", emptyStrings())
        ;
        private static final Map<String, MemberStatus> _codeClsMap = new HashMap<String, MemberStatus>();
        private static final Map<String, MemberStatus> _nameClsMap = new HashMap<String, MemberStatus>();
        static {
            for (MemberStatus value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private MemberStatus(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.MemberStatus; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<MemberStatus> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof MemberStatus) { return OptionalThing.of((MemberStatus)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<MemberStatus> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static MemberStatus codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof MemberStatus) { return (MemberStatus)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static MemberStatus nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<MemberStatus> listAll() {
            return new ArrayList<MemberStatus>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<MemberStatus> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: MemberStatus." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<MemberStatus> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<MemberStatus> clsList = new ArrayList<MemberStatus>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<MemberStatus> groupOf(String groupName) {
            return new ArrayList<MemberStatus>(4);
        }

        @Override public String toString() { return code(); }
    }

    /**
     * 商品ステータス: 商品の状態を示す
     */
    public enum ProductStatus implements CDef {
        /** 生産販売可能 */
        OnSale("ONS", "生産販売可能", emptyStrings())
        ,
        /** 生産中止 */
        ProductStop("PST", "生産中止", emptyStrings())
        ,
        /** 販売中止 */
        SaleStop("SST", "販売中止", emptyStrings())
        ;
        private static final Map<String, ProductStatus> _codeClsMap = new HashMap<String, ProductStatus>();
        private static final Map<String, ProductStatus> _nameClsMap = new HashMap<String, ProductStatus>();
        static {
            for (ProductStatus value : values()) {
                _codeClsMap.put(value.code().toLowerCase(), value);
                for (String sister : value.sisterSet()) { _codeClsMap.put(sister.toLowerCase(), value); }
                _nameClsMap.put(value.name().toLowerCase(), value);
            }
        }
        private String _code; private String _alias; private Set<String> _sisterSet;
        private ProductStatus(String code, String alias, String[] sisters)
        { _code = code; _alias = alias; _sisterSet = Collections.unmodifiableSet(new LinkedHashSet<String>(Arrays.asList(sisters))); }
        public String code() { return _code; } public String alias() { return _alias; }
        public Set<String> sisterSet() { return _sisterSet; }
        public Map<String, Object> subItemMap() { return Collections.emptyMap(); }
        public ClassificationMeta meta() { return CDef.DefMeta.ProductStatus; }

        public boolean inGroup(String groupName) {
            return false;
        }

        /**
         * Get the classification of the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns empty)
         * @return The optional classification corresponding to the code. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<ProductStatus> of(Object code) {
            if (code == null) { return OptionalThing.ofNullable(null, () -> { throw new ClassificationNotFoundException("null code specified"); }); }
            if (code instanceof ProductStatus) { return OptionalThing.of((ProductStatus)code); }
            if (code instanceof OptionalThing<?>) { return of(((OptionalThing<?>)code).orElse(null)); }
            return OptionalThing.ofNullable(_codeClsMap.get(code.toString().toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification code: " + code);
            });
        }

        /**
         * Find the classification by the name. (CaseInsensitive)
         * @param name The string of name, which is case-insensitive. (NotNull)
         * @return The optional classification corresponding to the name. (NotNull, EmptyAllowed: if not found, returns empty)
         */
        public static OptionalThing<ProductStatus> byName(String name) {
            if (name == null) { throw new IllegalArgumentException("The argument 'name' should not be null."); }
            return OptionalThing.ofNullable(_nameClsMap.get(name.toLowerCase()), () ->{
                throw new ClassificationNotFoundException("Unknown classification name: " + name);
            });
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use of(code).</span> <br>
         * Get the classification by the code. (CaseInsensitive)
         * @param code The value of code, which is case-insensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the code. (NullAllowed: if not found, returns null)
         */
        public static ProductStatus codeOf(Object code) {
            if (code == null) { return null; }
            if (code instanceof ProductStatus) { return (ProductStatus)code; }
            return _codeClsMap.get(code.toString().toLowerCase());
        }

        /**
         * <span style="color: #AD4747; font-size: 120%">Old style so use byName(name).</span> <br>
         * Get the classification by the name (also called 'value' in ENUM world).
         * @param name The string of name, which is case-sensitive. (NullAllowed: if null, returns null)
         * @return The instance of the corresponding classification to the name. (NullAllowed: if not found, returns null)
         */
        public static ProductStatus nameOf(String name) {
            if (name == null) { return null; }
            try { return valueOf(name); } catch (RuntimeException ignored) { return null; }
        }

        /**
         * Get the list of all classification elements. (returns new copied list)
         * @return The snapshot list of all classification elements. (NotNull)
         */
        public static List<ProductStatus> listAll() {
            return new ArrayList<ProductStatus>(Arrays.asList(values()));
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if not found, throws exception)
         */
        public static List<ProductStatus> listByGroup(String groupName) {
            if (groupName == null) { throw new IllegalArgumentException("The argument 'groupName' should not be null."); }
            throw new ClassificationNotFoundException("Unknown classification group: ProductStatus." + groupName);
        }

        /**
         * Get the list of classification elements corresponding to the specified codes. (returns new copied list) <br>
         * @param codeList The list of plain code, which is case-insensitive. (NotNull)
         * @return The snapshot list of classification elements in the code list. (NotNull, EmptyAllowed: when empty specified)
         */
        public static List<ProductStatus> listOf(Collection<String> codeList) {
            if (codeList == null) { throw new IllegalArgumentException("The argument 'codeList' should not be null."); }
            List<ProductStatus> clsList = new ArrayList<ProductStatus>(codeList.size());
            for (String code : codeList) { clsList.add(of(code).get()); }
            return clsList;
        }

        /**
         * Get the list of classification elements in the specified group. (returns new copied list) <br>
         * @param groupName The string of group name, which is case-sensitive. (NullAllowed: if null, returns empty list)
         * @return The snapshot list of classification elements in the group. (NotNull, EmptyAllowed: if the group is not found)
         */
        public static List<ProductStatus> groupOf(String groupName) {
            return new ArrayList<ProductStatus>(4);
        }

        @Override public String toString() { return code(); }
    }

    public enum DefMeta implements ClassificationMeta {
        /** 会員が受けられるサービスのランクを示す */
        ServiceRank
        ,
        /** mainly region of member address */
        Region
        ,
        /** reason for member withdrawal */
        WithdrawalReason
        ,
        /** 支払方法 */
        PaymentMethod
        ,
        /** the test of reference variable in grouping map */
        GroupingReference
        ,
        /** The test of relation reference */
        SelfReference
        ,
        /** The test of top only */
        TopCommentOnly
        ,
        /** the test of sub-item map for implicit classification */
        SubItemImplicit
        ,
        /** the test of sub-item map for table classification */
        SubItemTable
        ,
        /** boolean classification for boolean column */
        BooleanFlg
        ,
        /** master type of variant relation (biz-many-to-one) */
        VariantRelationMasterType
        ,
        /** qux type of variant relation (biz-many-to-one) */
        VariantRelationQuxType
        ,
        /** merged */
        QuxCls
        ,
        /** delimiter; &amp; endBrace} &amp; path\foo\bar */
        EscapedDfpropCls
        ,
        /** /*IF pmb.yourTop&#42;/&gt;&lt;&amp; */
        EscapedJavaDocCls
        ,
        /** 6 */
        EscapedNumberInitialCls
        ,
        /** top first line            top second line            top third line */
        LineSepCommentCls
        ,
        /** no camelizing classification */
        NamingDefaultCamelizingType
        ,
        /** no camelizing classification */
        NamingNoCamelizingType
        ,
        /** is deprecated classification */
        DeprecatedTopBasicType
        ,
        /** has deprecated element */
        DeprecatedMapBasicType
        ,
        /** has deprecated element */
        DeprecatedMapCollaborationType
        ,
        /** unique key as classification */
        UQClassificationType
        ,
        /** Classification of Bar */
        BarCls
        ,
        /** Classification of Foo */
        FooCls
        ,
        /** フラグを示す */
        Flg
        ,
        /** 会員ステータス: 会員の状態を示す */
        MemberStatus
        ,
        /** 商品ステータス: 商品の状態を示す */
        ProductStatus
        ;
        public String classificationName() {
            return name(); // same as definition name
        }

        public OptionalThing<? extends Classification> of(Object code) {
            if (ServiceRank.name().equals(name())) { return CDef.ServiceRank.of(code); }
            if (Region.name().equals(name())) { return CDef.Region.of(code); }
            if (WithdrawalReason.name().equals(name())) { return CDef.WithdrawalReason.of(code); }
            if (PaymentMethod.name().equals(name())) { return CDef.PaymentMethod.of(code); }
            if (GroupingReference.name().equals(name())) { return CDef.GroupingReference.of(code); }
            if (SelfReference.name().equals(name())) { return CDef.SelfReference.of(code); }
            if (TopCommentOnly.name().equals(name())) { return CDef.TopCommentOnly.of(code); }
            if (SubItemImplicit.name().equals(name())) { return CDef.SubItemImplicit.of(code); }
            if (SubItemTable.name().equals(name())) { return CDef.SubItemTable.of(code); }
            if (BooleanFlg.name().equals(name())) { return CDef.BooleanFlg.of(code); }
            if (VariantRelationMasterType.name().equals(name())) { return CDef.VariantRelationMasterType.of(code); }
            if (VariantRelationQuxType.name().equals(name())) { return CDef.VariantRelationQuxType.of(code); }
            if (QuxCls.name().equals(name())) { return CDef.QuxCls.of(code); }
            if (EscapedDfpropCls.name().equals(name())) { return CDef.EscapedDfpropCls.of(code); }
            if (EscapedJavaDocCls.name().equals(name())) { return CDef.EscapedJavaDocCls.of(code); }
            if (EscapedNumberInitialCls.name().equals(name())) { return CDef.EscapedNumberInitialCls.of(code); }
            if (LineSepCommentCls.name().equals(name())) { return CDef.LineSepCommentCls.of(code); }
            if (NamingDefaultCamelizingType.name().equals(name())) { return CDef.NamingDefaultCamelizingType.of(code); }
            if (NamingNoCamelizingType.name().equals(name())) { return CDef.NamingNoCamelizingType.of(code); }
            if (DeprecatedTopBasicType.name().equals(name())) { return CDef.DeprecatedTopBasicType.of(code); }
            if (DeprecatedMapBasicType.name().equals(name())) { return CDef.DeprecatedMapBasicType.of(code); }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return CDef.DeprecatedMapCollaborationType.of(code); }
            if (UQClassificationType.name().equals(name())) { return CDef.UQClassificationType.of(code); }
            if (BarCls.name().equals(name())) { return CDef.BarCls.of(code); }
            if (FooCls.name().equals(name())) { return CDef.FooCls.of(code); }
            if (Flg.name().equals(name())) { return CDef.Flg.of(code); }
            if (MemberStatus.name().equals(name())) { return CDef.MemberStatus.of(code); }
            if (ProductStatus.name().equals(name())) { return CDef.ProductStatus.of(code); }
            throw new IllegalStateException("Unknown definition: " + this); // basically unreachable
        }

        public OptionalThing<? extends Classification> byName(String name) {
            if (ServiceRank.name().equals(name())) { return CDef.ServiceRank.byName(name); }
            if (Region.name().equals(name())) { return CDef.Region.byName(name); }
            if (WithdrawalReason.name().equals(name())) { return CDef.WithdrawalReason.byName(name); }
            if (PaymentMethod.name().equals(name())) { return CDef.PaymentMethod.byName(name); }
            if (GroupingReference.name().equals(name())) { return CDef.GroupingReference.byName(name); }
            if (SelfReference.name().equals(name())) { return CDef.SelfReference.byName(name); }
            if (TopCommentOnly.name().equals(name())) { return CDef.TopCommentOnly.byName(name); }
            if (SubItemImplicit.name().equals(name())) { return CDef.SubItemImplicit.byName(name); }
            if (SubItemTable.name().equals(name())) { return CDef.SubItemTable.byName(name); }
            if (BooleanFlg.name().equals(name())) { return CDef.BooleanFlg.byName(name); }
            if (VariantRelationMasterType.name().equals(name())) { return CDef.VariantRelationMasterType.byName(name); }
            if (VariantRelationQuxType.name().equals(name())) { return CDef.VariantRelationQuxType.byName(name); }
            if (QuxCls.name().equals(name())) { return CDef.QuxCls.byName(name); }
            if (EscapedDfpropCls.name().equals(name())) { return CDef.EscapedDfpropCls.byName(name); }
            if (EscapedJavaDocCls.name().equals(name())) { return CDef.EscapedJavaDocCls.byName(name); }
            if (EscapedNumberInitialCls.name().equals(name())) { return CDef.EscapedNumberInitialCls.byName(name); }
            if (LineSepCommentCls.name().equals(name())) { return CDef.LineSepCommentCls.byName(name); }
            if (NamingDefaultCamelizingType.name().equals(name())) { return CDef.NamingDefaultCamelizingType.byName(name); }
            if (NamingNoCamelizingType.name().equals(name())) { return CDef.NamingNoCamelizingType.byName(name); }
            if (DeprecatedTopBasicType.name().equals(name())) { return CDef.DeprecatedTopBasicType.byName(name); }
            if (DeprecatedMapBasicType.name().equals(name())) { return CDef.DeprecatedMapBasicType.byName(name); }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return CDef.DeprecatedMapCollaborationType.byName(name); }
            if (UQClassificationType.name().equals(name())) { return CDef.UQClassificationType.byName(name); }
            if (BarCls.name().equals(name())) { return CDef.BarCls.byName(name); }
            if (FooCls.name().equals(name())) { return CDef.FooCls.byName(name); }
            if (Flg.name().equals(name())) { return CDef.Flg.byName(name); }
            if (MemberStatus.name().equals(name())) { return CDef.MemberStatus.byName(name); }
            if (ProductStatus.name().equals(name())) { return CDef.ProductStatus.byName(name); }
            throw new IllegalStateException("Unknown definition: " + this); // basically unreachable
        }

        public Classification codeOf(Object code) { // null if not found, old style so use of(code)
            if (ServiceRank.name().equals(name())) { return CDef.ServiceRank.codeOf(code); }
            if (Region.name().equals(name())) { return CDef.Region.codeOf(code); }
            if (WithdrawalReason.name().equals(name())) { return CDef.WithdrawalReason.codeOf(code); }
            if (PaymentMethod.name().equals(name())) { return CDef.PaymentMethod.codeOf(code); }
            if (GroupingReference.name().equals(name())) { return CDef.GroupingReference.codeOf(code); }
            if (SelfReference.name().equals(name())) { return CDef.SelfReference.codeOf(code); }
            if (TopCommentOnly.name().equals(name())) { return CDef.TopCommentOnly.codeOf(code); }
            if (SubItemImplicit.name().equals(name())) { return CDef.SubItemImplicit.codeOf(code); }
            if (SubItemTable.name().equals(name())) { return CDef.SubItemTable.codeOf(code); }
            if (BooleanFlg.name().equals(name())) { return CDef.BooleanFlg.codeOf(code); }
            if (VariantRelationMasterType.name().equals(name())) { return CDef.VariantRelationMasterType.codeOf(code); }
            if (VariantRelationQuxType.name().equals(name())) { return CDef.VariantRelationQuxType.codeOf(code); }
            if (QuxCls.name().equals(name())) { return CDef.QuxCls.codeOf(code); }
            if (EscapedDfpropCls.name().equals(name())) { return CDef.EscapedDfpropCls.codeOf(code); }
            if (EscapedJavaDocCls.name().equals(name())) { return CDef.EscapedJavaDocCls.codeOf(code); }
            if (EscapedNumberInitialCls.name().equals(name())) { return CDef.EscapedNumberInitialCls.codeOf(code); }
            if (LineSepCommentCls.name().equals(name())) { return CDef.LineSepCommentCls.codeOf(code); }
            if (NamingDefaultCamelizingType.name().equals(name())) { return CDef.NamingDefaultCamelizingType.codeOf(code); }
            if (NamingNoCamelizingType.name().equals(name())) { return CDef.NamingNoCamelizingType.codeOf(code); }
            if (DeprecatedTopBasicType.name().equals(name())) { return CDef.DeprecatedTopBasicType.codeOf(code); }
            if (DeprecatedMapBasicType.name().equals(name())) { return CDef.DeprecatedMapBasicType.codeOf(code); }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return CDef.DeprecatedMapCollaborationType.codeOf(code); }
            if (UQClassificationType.name().equals(name())) { return CDef.UQClassificationType.codeOf(code); }
            if (BarCls.name().equals(name())) { return CDef.BarCls.codeOf(code); }
            if (FooCls.name().equals(name())) { return CDef.FooCls.codeOf(code); }
            if (Flg.name().equals(name())) { return CDef.Flg.codeOf(code); }
            if (MemberStatus.name().equals(name())) { return CDef.MemberStatus.codeOf(code); }
            if (ProductStatus.name().equals(name())) { return CDef.ProductStatus.codeOf(code); }
            throw new IllegalStateException("Unknown definition: " + this); // basically unreachable
        }

        public Classification nameOf(String name) { // null if not found, old style so use byName(name)
            if (ServiceRank.name().equals(name())) { return CDef.ServiceRank.valueOf(name); }
            if (Region.name().equals(name())) { return CDef.Region.valueOf(name); }
            if (WithdrawalReason.name().equals(name())) { return CDef.WithdrawalReason.valueOf(name); }
            if (PaymentMethod.name().equals(name())) { return CDef.PaymentMethod.valueOf(name); }
            if (GroupingReference.name().equals(name())) { return CDef.GroupingReference.valueOf(name); }
            if (SelfReference.name().equals(name())) { return CDef.SelfReference.valueOf(name); }
            if (TopCommentOnly.name().equals(name())) { return CDef.TopCommentOnly.valueOf(name); }
            if (SubItemImplicit.name().equals(name())) { return CDef.SubItemImplicit.valueOf(name); }
            if (SubItemTable.name().equals(name())) { return CDef.SubItemTable.valueOf(name); }
            if (BooleanFlg.name().equals(name())) { return CDef.BooleanFlg.valueOf(name); }
            if (VariantRelationMasterType.name().equals(name())) { return CDef.VariantRelationMasterType.valueOf(name); }
            if (VariantRelationQuxType.name().equals(name())) { return CDef.VariantRelationQuxType.valueOf(name); }
            if (QuxCls.name().equals(name())) { return CDef.QuxCls.valueOf(name); }
            if (EscapedDfpropCls.name().equals(name())) { return CDef.EscapedDfpropCls.valueOf(name); }
            if (EscapedJavaDocCls.name().equals(name())) { return CDef.EscapedJavaDocCls.valueOf(name); }
            if (EscapedNumberInitialCls.name().equals(name())) { return CDef.EscapedNumberInitialCls.valueOf(name); }
            if (LineSepCommentCls.name().equals(name())) { return CDef.LineSepCommentCls.valueOf(name); }
            if (NamingDefaultCamelizingType.name().equals(name())) { return CDef.NamingDefaultCamelizingType.valueOf(name); }
            if (NamingNoCamelizingType.name().equals(name())) { return CDef.NamingNoCamelizingType.valueOf(name); }
            if (DeprecatedTopBasicType.name().equals(name())) { return CDef.DeprecatedTopBasicType.valueOf(name); }
            if (DeprecatedMapBasicType.name().equals(name())) { return CDef.DeprecatedMapBasicType.valueOf(name); }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return CDef.DeprecatedMapCollaborationType.valueOf(name); }
            if (UQClassificationType.name().equals(name())) { return CDef.UQClassificationType.valueOf(name); }
            if (BarCls.name().equals(name())) { return CDef.BarCls.valueOf(name); }
            if (FooCls.name().equals(name())) { return CDef.FooCls.valueOf(name); }
            if (Flg.name().equals(name())) { return CDef.Flg.valueOf(name); }
            if (MemberStatus.name().equals(name())) { return CDef.MemberStatus.valueOf(name); }
            if (ProductStatus.name().equals(name())) { return CDef.ProductStatus.valueOf(name); }
            throw new IllegalStateException("Unknown definition: " + this); // basically unreachable
        }

        public List<Classification> listAll() {
            if (ServiceRank.name().equals(name())) { return toClsList(CDef.ServiceRank.listAll()); }
            if (Region.name().equals(name())) { return toClsList(CDef.Region.listAll()); }
            if (WithdrawalReason.name().equals(name())) { return toClsList(CDef.WithdrawalReason.listAll()); }
            if (PaymentMethod.name().equals(name())) { return toClsList(CDef.PaymentMethod.listAll()); }
            if (GroupingReference.name().equals(name())) { return toClsList(CDef.GroupingReference.listAll()); }
            if (SelfReference.name().equals(name())) { return toClsList(CDef.SelfReference.listAll()); }
            if (TopCommentOnly.name().equals(name())) { return toClsList(CDef.TopCommentOnly.listAll()); }
            if (SubItemImplicit.name().equals(name())) { return toClsList(CDef.SubItemImplicit.listAll()); }
            if (SubItemTable.name().equals(name())) { return toClsList(CDef.SubItemTable.listAll()); }
            if (BooleanFlg.name().equals(name())) { return toClsList(CDef.BooleanFlg.listAll()); }
            if (VariantRelationMasterType.name().equals(name())) { return toClsList(CDef.VariantRelationMasterType.listAll()); }
            if (VariantRelationQuxType.name().equals(name())) { return toClsList(CDef.VariantRelationQuxType.listAll()); }
            if (QuxCls.name().equals(name())) { return toClsList(CDef.QuxCls.listAll()); }
            if (EscapedDfpropCls.name().equals(name())) { return toClsList(CDef.EscapedDfpropCls.listAll()); }
            if (EscapedJavaDocCls.name().equals(name())) { return toClsList(CDef.EscapedJavaDocCls.listAll()); }
            if (EscapedNumberInitialCls.name().equals(name())) { return toClsList(CDef.EscapedNumberInitialCls.listAll()); }
            if (LineSepCommentCls.name().equals(name())) { return toClsList(CDef.LineSepCommentCls.listAll()); }
            if (NamingDefaultCamelizingType.name().equals(name())) { return toClsList(CDef.NamingDefaultCamelizingType.listAll()); }
            if (NamingNoCamelizingType.name().equals(name())) { return toClsList(CDef.NamingNoCamelizingType.listAll()); }
            if (DeprecatedTopBasicType.name().equals(name())) { return toClsList(CDef.DeprecatedTopBasicType.listAll()); }
            if (DeprecatedMapBasicType.name().equals(name())) { return toClsList(CDef.DeprecatedMapBasicType.listAll()); }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return toClsList(CDef.DeprecatedMapCollaborationType.listAll()); }
            if (UQClassificationType.name().equals(name())) { return toClsList(CDef.UQClassificationType.listAll()); }
            if (BarCls.name().equals(name())) { return toClsList(CDef.BarCls.listAll()); }
            if (FooCls.name().equals(name())) { return toClsList(CDef.FooCls.listAll()); }
            if (Flg.name().equals(name())) { return toClsList(CDef.Flg.listAll()); }
            if (MemberStatus.name().equals(name())) { return toClsList(CDef.MemberStatus.listAll()); }
            if (ProductStatus.name().equals(name())) { return toClsList(CDef.ProductStatus.listAll()); }
            throw new IllegalStateException("Unknown definition: " + this); // basically unreachable
        }

        public List<Classification> listByGroup(String groupName) { // exception if not found
            if (ServiceRank.name().equals(name())) { return toClsList(CDef.ServiceRank.listByGroup(groupName)); }
            if (Region.name().equals(name())) { return toClsList(CDef.Region.listByGroup(groupName)); }
            if (WithdrawalReason.name().equals(name())) { return toClsList(CDef.WithdrawalReason.listByGroup(groupName)); }
            if (PaymentMethod.name().equals(name())) { return toClsList(CDef.PaymentMethod.listByGroup(groupName)); }
            if (GroupingReference.name().equals(name())) { return toClsList(CDef.GroupingReference.listByGroup(groupName)); }
            if (SelfReference.name().equals(name())) { return toClsList(CDef.SelfReference.listByGroup(groupName)); }
            if (TopCommentOnly.name().equals(name())) { return toClsList(CDef.TopCommentOnly.listByGroup(groupName)); }
            if (SubItemImplicit.name().equals(name())) { return toClsList(CDef.SubItemImplicit.listByGroup(groupName)); }
            if (SubItemTable.name().equals(name())) { return toClsList(CDef.SubItemTable.listByGroup(groupName)); }
            if (BooleanFlg.name().equals(name())) { return toClsList(CDef.BooleanFlg.listByGroup(groupName)); }
            if (VariantRelationMasterType.name().equals(name())) { return toClsList(CDef.VariantRelationMasterType.listByGroup(groupName)); }
            if (VariantRelationQuxType.name().equals(name())) { return toClsList(CDef.VariantRelationQuxType.listByGroup(groupName)); }
            if (QuxCls.name().equals(name())) { return toClsList(CDef.QuxCls.listByGroup(groupName)); }
            if (EscapedDfpropCls.name().equals(name())) { return toClsList(CDef.EscapedDfpropCls.listByGroup(groupName)); }
            if (EscapedJavaDocCls.name().equals(name())) { return toClsList(CDef.EscapedJavaDocCls.listByGroup(groupName)); }
            if (EscapedNumberInitialCls.name().equals(name())) { return toClsList(CDef.EscapedNumberInitialCls.listByGroup(groupName)); }
            if (LineSepCommentCls.name().equals(name())) { return toClsList(CDef.LineSepCommentCls.listByGroup(groupName)); }
            if (NamingDefaultCamelizingType.name().equals(name())) { return toClsList(CDef.NamingDefaultCamelizingType.listByGroup(groupName)); }
            if (NamingNoCamelizingType.name().equals(name())) { return toClsList(CDef.NamingNoCamelizingType.listByGroup(groupName)); }
            if (DeprecatedTopBasicType.name().equals(name())) { return toClsList(CDef.DeprecatedTopBasicType.listByGroup(groupName)); }
            if (DeprecatedMapBasicType.name().equals(name())) { return toClsList(CDef.DeprecatedMapBasicType.listByGroup(groupName)); }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return toClsList(CDef.DeprecatedMapCollaborationType.listByGroup(groupName)); }
            if (UQClassificationType.name().equals(name())) { return toClsList(CDef.UQClassificationType.listByGroup(groupName)); }
            if (BarCls.name().equals(name())) { return toClsList(CDef.BarCls.listByGroup(groupName)); }
            if (FooCls.name().equals(name())) { return toClsList(CDef.FooCls.listByGroup(groupName)); }
            if (Flg.name().equals(name())) { return toClsList(CDef.Flg.listByGroup(groupName)); }
            if (MemberStatus.name().equals(name())) { return toClsList(CDef.MemberStatus.listByGroup(groupName)); }
            if (ProductStatus.name().equals(name())) { return toClsList(CDef.ProductStatus.listByGroup(groupName)); }
            throw new IllegalStateException("Unknown definition: " + this); // basically unreachable
        }

        public List<Classification> listOf(Collection<String> codeList) {
            if (ServiceRank.name().equals(name())) { return toClsList(CDef.ServiceRank.listOf(codeList)); }
            if (Region.name().equals(name())) { return toClsList(CDef.Region.listOf(codeList)); }
            if (WithdrawalReason.name().equals(name())) { return toClsList(CDef.WithdrawalReason.listOf(codeList)); }
            if (PaymentMethod.name().equals(name())) { return toClsList(CDef.PaymentMethod.listOf(codeList)); }
            if (GroupingReference.name().equals(name())) { return toClsList(CDef.GroupingReference.listOf(codeList)); }
            if (SelfReference.name().equals(name())) { return toClsList(CDef.SelfReference.listOf(codeList)); }
            if (TopCommentOnly.name().equals(name())) { return toClsList(CDef.TopCommentOnly.listOf(codeList)); }
            if (SubItemImplicit.name().equals(name())) { return toClsList(CDef.SubItemImplicit.listOf(codeList)); }
            if (SubItemTable.name().equals(name())) { return toClsList(CDef.SubItemTable.listOf(codeList)); }
            if (BooleanFlg.name().equals(name())) { return toClsList(CDef.BooleanFlg.listOf(codeList)); }
            if (VariantRelationMasterType.name().equals(name())) { return toClsList(CDef.VariantRelationMasterType.listOf(codeList)); }
            if (VariantRelationQuxType.name().equals(name())) { return toClsList(CDef.VariantRelationQuxType.listOf(codeList)); }
            if (QuxCls.name().equals(name())) { return toClsList(CDef.QuxCls.listOf(codeList)); }
            if (EscapedDfpropCls.name().equals(name())) { return toClsList(CDef.EscapedDfpropCls.listOf(codeList)); }
            if (EscapedJavaDocCls.name().equals(name())) { return toClsList(CDef.EscapedJavaDocCls.listOf(codeList)); }
            if (EscapedNumberInitialCls.name().equals(name())) { return toClsList(CDef.EscapedNumberInitialCls.listOf(codeList)); }
            if (LineSepCommentCls.name().equals(name())) { return toClsList(CDef.LineSepCommentCls.listOf(codeList)); }
            if (NamingDefaultCamelizingType.name().equals(name())) { return toClsList(CDef.NamingDefaultCamelizingType.listOf(codeList)); }
            if (NamingNoCamelizingType.name().equals(name())) { return toClsList(CDef.NamingNoCamelizingType.listOf(codeList)); }
            if (DeprecatedTopBasicType.name().equals(name())) { return toClsList(CDef.DeprecatedTopBasicType.listOf(codeList)); }
            if (DeprecatedMapBasicType.name().equals(name())) { return toClsList(CDef.DeprecatedMapBasicType.listOf(codeList)); }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return toClsList(CDef.DeprecatedMapCollaborationType.listOf(codeList)); }
            if (UQClassificationType.name().equals(name())) { return toClsList(CDef.UQClassificationType.listOf(codeList)); }
            if (BarCls.name().equals(name())) { return toClsList(CDef.BarCls.listOf(codeList)); }
            if (FooCls.name().equals(name())) { return toClsList(CDef.FooCls.listOf(codeList)); }
            if (Flg.name().equals(name())) { return toClsList(CDef.Flg.listOf(codeList)); }
            if (MemberStatus.name().equals(name())) { return toClsList(CDef.MemberStatus.listOf(codeList)); }
            if (ProductStatus.name().equals(name())) { return toClsList(CDef.ProductStatus.listOf(codeList)); }
            throw new IllegalStateException("Unknown definition: " + this); // basically unreachable
        }

        public List<Classification> groupOf(String groupName) { // old style
            if (ServiceRank.name().equals(name())) { return toClsList(CDef.ServiceRank.groupOf(groupName)); }
            if (Region.name().equals(name())) { return toClsList(CDef.Region.groupOf(groupName)); }
            if (WithdrawalReason.name().equals(name())) { return toClsList(CDef.WithdrawalReason.groupOf(groupName)); }
            if (PaymentMethod.name().equals(name())) { return toClsList(CDef.PaymentMethod.groupOf(groupName)); }
            if (GroupingReference.name().equals(name())) { return toClsList(CDef.GroupingReference.groupOf(groupName)); }
            if (SelfReference.name().equals(name())) { return toClsList(CDef.SelfReference.groupOf(groupName)); }
            if (TopCommentOnly.name().equals(name())) { return toClsList(CDef.TopCommentOnly.groupOf(groupName)); }
            if (SubItemImplicit.name().equals(name())) { return toClsList(CDef.SubItemImplicit.groupOf(groupName)); }
            if (SubItemTable.name().equals(name())) { return toClsList(CDef.SubItemTable.groupOf(groupName)); }
            if (BooleanFlg.name().equals(name())) { return toClsList(CDef.BooleanFlg.groupOf(groupName)); }
            if (VariantRelationMasterType.name().equals(name())) { return toClsList(CDef.VariantRelationMasterType.groupOf(groupName)); }
            if (VariantRelationQuxType.name().equals(name())) { return toClsList(CDef.VariantRelationQuxType.groupOf(groupName)); }
            if (QuxCls.name().equals(name())) { return toClsList(CDef.QuxCls.groupOf(groupName)); }
            if (EscapedDfpropCls.name().equals(name())) { return toClsList(CDef.EscapedDfpropCls.groupOf(groupName)); }
            if (EscapedJavaDocCls.name().equals(name())) { return toClsList(CDef.EscapedJavaDocCls.groupOf(groupName)); }
            if (EscapedNumberInitialCls.name().equals(name())) { return toClsList(CDef.EscapedNumberInitialCls.groupOf(groupName)); }
            if (LineSepCommentCls.name().equals(name())) { return toClsList(CDef.LineSepCommentCls.groupOf(groupName)); }
            if (NamingDefaultCamelizingType.name().equals(name())) { return toClsList(CDef.NamingDefaultCamelizingType.groupOf(groupName)); }
            if (NamingNoCamelizingType.name().equals(name())) { return toClsList(CDef.NamingNoCamelizingType.groupOf(groupName)); }
            if (DeprecatedTopBasicType.name().equals(name())) { return toClsList(CDef.DeprecatedTopBasicType.groupOf(groupName)); }
            if (DeprecatedMapBasicType.name().equals(name())) { return toClsList(CDef.DeprecatedMapBasicType.groupOf(groupName)); }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return toClsList(CDef.DeprecatedMapCollaborationType.groupOf(groupName)); }
            if (UQClassificationType.name().equals(name())) { return toClsList(CDef.UQClassificationType.groupOf(groupName)); }
            if (BarCls.name().equals(name())) { return toClsList(CDef.BarCls.groupOf(groupName)); }
            if (FooCls.name().equals(name())) { return toClsList(CDef.FooCls.groupOf(groupName)); }
            if (Flg.name().equals(name())) { return toClsList(CDef.Flg.groupOf(groupName)); }
            if (MemberStatus.name().equals(name())) { return toClsList(CDef.MemberStatus.groupOf(groupName)); }
            if (ProductStatus.name().equals(name())) { return toClsList(CDef.ProductStatus.groupOf(groupName)); }
            throw new IllegalStateException("Unknown definition: " + this); // basically unreachable
        }

        @SuppressWarnings("unchecked")
        private List<Classification> toClsList(List<?> clsList) {
            return (List<Classification>)clsList;
        }

        public ClassificationCodeType codeType() {
            if (ServiceRank.name().equals(name())) { return ClassificationCodeType.String; }
            if (Region.name().equals(name())) { return ClassificationCodeType.Number; }
            if (WithdrawalReason.name().equals(name())) { return ClassificationCodeType.String; }
            if (PaymentMethod.name().equals(name())) { return ClassificationCodeType.String; }
            if (GroupingReference.name().equals(name())) { return ClassificationCodeType.String; }
            if (SelfReference.name().equals(name())) { return ClassificationCodeType.Number; }
            if (TopCommentOnly.name().equals(name())) { return ClassificationCodeType.String; }
            if (SubItemImplicit.name().equals(name())) { return ClassificationCodeType.Number; }
            if (SubItemTable.name().equals(name())) { return ClassificationCodeType.String; }
            if (BooleanFlg.name().equals(name())) { return ClassificationCodeType.Boolean; }
            if (VariantRelationMasterType.name().equals(name())) { return ClassificationCodeType.String; }
            if (VariantRelationQuxType.name().equals(name())) { return ClassificationCodeType.String; }
            if (QuxCls.name().equals(name())) { return ClassificationCodeType.String; }
            if (EscapedDfpropCls.name().equals(name())) { return ClassificationCodeType.String; }
            if (EscapedJavaDocCls.name().equals(name())) { return ClassificationCodeType.String; }
            if (EscapedNumberInitialCls.name().equals(name())) { return ClassificationCodeType.String; }
            if (LineSepCommentCls.name().equals(name())) { return ClassificationCodeType.String; }
            if (NamingDefaultCamelizingType.name().equals(name())) { return ClassificationCodeType.String; }
            if (NamingNoCamelizingType.name().equals(name())) { return ClassificationCodeType.String; }
            if (DeprecatedTopBasicType.name().equals(name())) { return ClassificationCodeType.String; }
            if (DeprecatedMapBasicType.name().equals(name())) { return ClassificationCodeType.String; }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return ClassificationCodeType.String; }
            if (UQClassificationType.name().equals(name())) { return ClassificationCodeType.String; }
            if (BarCls.name().equals(name())) { return ClassificationCodeType.String; }
            if (FooCls.name().equals(name())) { return ClassificationCodeType.String; }
            if (Flg.name().equals(name())) { return ClassificationCodeType.Number; }
            if (MemberStatus.name().equals(name())) { return ClassificationCodeType.String; }
            if (ProductStatus.name().equals(name())) { return ClassificationCodeType.String; }
            return ClassificationCodeType.String; // as default
        }

        public ClassificationUndefinedHandlingType undefinedHandlingType() {
            if (ServiceRank.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (Region.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (WithdrawalReason.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (PaymentMethod.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (GroupingReference.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (SelfReference.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (TopCommentOnly.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (SubItemImplicit.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (SubItemTable.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (BooleanFlg.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (VariantRelationMasterType.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (VariantRelationQuxType.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (QuxCls.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (EscapedDfpropCls.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (EscapedJavaDocCls.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (EscapedNumberInitialCls.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (LineSepCommentCls.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (NamingDefaultCamelizingType.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (NamingNoCamelizingType.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (DeprecatedTopBasicType.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (DeprecatedMapBasicType.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (DeprecatedMapCollaborationType.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (UQClassificationType.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (BarCls.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (FooCls.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (Flg.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (MemberStatus.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            if (ProductStatus.name().equals(name())) { return ClassificationUndefinedHandlingType.EXCEPTION; }
            return ClassificationUndefinedHandlingType.LOGGING; // as default
        }

        public static OptionalThing<CDef.DefMeta> find(String classificationName) { // instead of valueOf()
            if (classificationName == null) { throw new IllegalArgumentException("The argument 'classificationName' should not be null."); }
            if (ServiceRank.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.ServiceRank); }
            if (Region.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.Region); }
            if (WithdrawalReason.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.WithdrawalReason); }
            if (PaymentMethod.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.PaymentMethod); }
            if (GroupingReference.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.GroupingReference); }
            if (SelfReference.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.SelfReference); }
            if (TopCommentOnly.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.TopCommentOnly); }
            if (SubItemImplicit.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.SubItemImplicit); }
            if (SubItemTable.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.SubItemTable); }
            if (BooleanFlg.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.BooleanFlg); }
            if (VariantRelationMasterType.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.VariantRelationMasterType); }
            if (VariantRelationQuxType.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.VariantRelationQuxType); }
            if (QuxCls.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.QuxCls); }
            if (EscapedDfpropCls.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.EscapedDfpropCls); }
            if (EscapedJavaDocCls.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.EscapedJavaDocCls); }
            if (EscapedNumberInitialCls.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.EscapedNumberInitialCls); }
            if (LineSepCommentCls.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.LineSepCommentCls); }
            if (NamingDefaultCamelizingType.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.NamingDefaultCamelizingType); }
            if (NamingNoCamelizingType.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.NamingNoCamelizingType); }
            if (DeprecatedTopBasicType.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.DeprecatedTopBasicType); }
            if (DeprecatedMapBasicType.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.DeprecatedMapBasicType); }
            if (DeprecatedMapCollaborationType.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.DeprecatedMapCollaborationType); }
            if (UQClassificationType.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.UQClassificationType); }
            if (BarCls.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.BarCls); }
            if (FooCls.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.FooCls); }
            if (Flg.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.Flg); }
            if (MemberStatus.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.MemberStatus); }
            if (ProductStatus.name().equalsIgnoreCase(classificationName)) { return OptionalThing.of(CDef.DefMeta.ProductStatus); }
            return OptionalThing.ofNullable(null, () -> {
                throw new ClassificationNotFoundException("Unknown classification: " + classificationName);
            });
        }

        public static CDef.DefMeta meta(String classificationName) { // old style so use find(name)
            if (classificationName == null) { throw new IllegalArgumentException("The argument 'classificationName' should not be null."); }
            if (ServiceRank.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.ServiceRank; }
            if (Region.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.Region; }
            if (WithdrawalReason.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.WithdrawalReason; }
            if (PaymentMethod.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.PaymentMethod; }
            if (GroupingReference.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.GroupingReference; }
            if (SelfReference.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.SelfReference; }
            if (TopCommentOnly.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.TopCommentOnly; }
            if (SubItemImplicit.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.SubItemImplicit; }
            if (SubItemTable.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.SubItemTable; }
            if (BooleanFlg.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.BooleanFlg; }
            if (VariantRelationMasterType.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.VariantRelationMasterType; }
            if (VariantRelationQuxType.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.VariantRelationQuxType; }
            if (QuxCls.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.QuxCls; }
            if (EscapedDfpropCls.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.EscapedDfpropCls; }
            if (EscapedJavaDocCls.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.EscapedJavaDocCls; }
            if (EscapedNumberInitialCls.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.EscapedNumberInitialCls; }
            if (LineSepCommentCls.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.LineSepCommentCls; }
            if (NamingDefaultCamelizingType.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.NamingDefaultCamelizingType; }
            if (NamingNoCamelizingType.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.NamingNoCamelizingType; }
            if (DeprecatedTopBasicType.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.DeprecatedTopBasicType; }
            if (DeprecatedMapBasicType.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.DeprecatedMapBasicType; }
            if (DeprecatedMapCollaborationType.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.DeprecatedMapCollaborationType; }
            if (UQClassificationType.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.UQClassificationType; }
            if (BarCls.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.BarCls; }
            if (FooCls.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.FooCls; }
            if (Flg.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.Flg; }
            if (MemberStatus.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.MemberStatus; }
            if (ProductStatus.name().equalsIgnoreCase(classificationName)) { return CDef.DefMeta.ProductStatus; }
            throw new IllegalStateException("Unknown classification: " + classificationName);
        }

        @SuppressWarnings("unused")
        private String[] xinternalEmptyString() {
            return emptyStrings(); // to suppress 'unused' warning of import statement
        }
    }
}
