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
package org.docksidestage.mysql.dbflute.immuhama.bsbhv.loader;

import java.util.List;

import org.dbflute.bhv.*;
import org.docksidestage.mysql.dbflute.immuhama.exbhv.*;
import org.docksidestage.mysql.dbflute.immuhama.exentity.*;

/**
 * The referrer loader of (会員ログイン情報)MEMBER_LOGIN as TABLE. <br>
 * <pre>
 * [primary key]
 *     MEMBER_LOGIN_ID
 *
 * [column]
 *     MEMBER_LOGIN_ID, MEMBER_ID, LOGIN_DATETIME, MOBILE_LOGIN_FLG, LOGIN_MEMBER_STATUS_CODE
 *
 * [sequence]
 *     
 *
 * [identity]
 *     MEMBER_LOGIN_ID
 *
 * [version-no]
 *     
 *
 * [foreign table]
 *     MEMBER_STATUS, MEMBER
 *
 * [referrer table]
 *     
 *
 * [foreign property]
 *     memberStatus, member
 *
 * [referrer property]
 *     
 * </pre>
 * @author DBFlute(AutoGenerator)
 */
public class ImmuLoaderOfMemberLogin {

    // ===================================================================================
    //                                                                           Attribute
    //                                                                           =========
    protected List<ImmuMemberLogin> _selectedList;
    protected BehaviorSelector _selector;
    protected ImmuMemberLoginBhv _myBhv; // lazy-loaded

    // ===================================================================================
    //                                                                   Ready for Loading
    //                                                                   =================
    public ImmuLoaderOfMemberLogin ready(List<ImmuMemberLogin> selectedList, BehaviorSelector selector)
    { _selectedList = selectedList; _selector = selector; return this; }

    protected ImmuMemberLoginBhv myBhv()
    { if (_myBhv != null) { return _myBhv; } else { _myBhv = _selector.select(ImmuMemberLoginBhv.class); return _myBhv; } }

    // ===================================================================================
    //                                                                    Pull out Foreign
    //                                                                    ================
    protected ImmuLoaderOfMemberStatus _foreignMemberStatusLoader;
    public ImmuLoaderOfMemberStatus pulloutMemberStatus() {
        if (_foreignMemberStatusLoader == null)
        { _foreignMemberStatusLoader = new ImmuLoaderOfMemberStatus().ready(myBhv().pulloutMemberStatus(_selectedList), _selector); }
        return _foreignMemberStatusLoader;
    }

    protected ImmuLoaderOfMember _foreignMemberLoader;
    public ImmuLoaderOfMember pulloutMember() {
        if (_foreignMemberLoader == null)
        { _foreignMemberLoader = new ImmuLoaderOfMember().ready(myBhv().pulloutMember(_selectedList), _selector); }
        return _foreignMemberLoader;
    }

    // ===================================================================================
    //                                                                            Accessor
    //                                                                            ========
    public List<ImmuMemberLogin> getSelectedList() { return _selectedList; }
    public BehaviorSelector getSelector() { return _selector; }
}
