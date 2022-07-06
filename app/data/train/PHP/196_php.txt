<?php

namespace Augwa\QuickBooks\Model;

/**
 * Master Account is the list of accounts in the master list. The master
 * list is the complete list of accounts prescribed by the French
 * Government. These accounts can be created in the company on a need
 * basis. The account create API needs to be used to create an account.
 *
 * Class MasterAccountModel
 * @package Augwa\QuickBooks\Model
 */
class MasterAccountModel
    extends AccountModel
{

    /**
     * @var bool
     */
    private $AccountExistsInCompany;


    /**
     * Product: ALL
     * Specifies whether the account has been created in the company.
     *
     * @return bool
     */
    public function getAccountExistsInCompany()
    {
        return $this->AccountExistsInCompany;
    }

    /**
     * Product: ALL
     * Specifies whether the account has been created in the company.
     *
     * @param bool $AccountExistsInCompany
     *
     * @return MasterAccountModel
     */
    public function setAccountExistsInCompany(
        $AccountExistsInCompany
    )
    {
        $this->AccountExistsInCompany = $AccountExistsInCompany;
        return $this;
    }

}