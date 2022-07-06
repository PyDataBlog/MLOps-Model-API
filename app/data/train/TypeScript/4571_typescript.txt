
namespace GestionEquestre.Ge {
    export interface CorCorporateRow {
        Id?: number;
        IsActive?: boolean;
        NotArchive?: boolean;
        InsertDate?: string;
        InsertUserId?: number;
        UpdateDate?: string;
        UpdateUserId?: number;
        Name?: string;
        Phone?: string;
        Gsm?: string;
        OtherPhone1?: string;
        Email1?: string;
        Email2?: string;
        BankAccount?: number;
        IdAdress?: number;
        FrSiren?: string;
        Caption?: string;
        ArchiveDate?: string;
        BankAccountCountryCodeIban?: string;
        BankAccountCheckDigitsIban?: string;
        BankAccountBban?: string;
        BankAccountBic?: string;
        BankAccountAccountOnwer?: string;
        BankAccountBankAdress?: string;
        BankAccountCis?: string;
        BankAccountUrm?: string;
        BankAccountMandateDateSign?: string;
        BankAccountTypePayment?: number;
        BankAccountDateLastPrelevement?: string;
        BankAccountTypeOfLastPayment?: number;
        IdAdressIsActive?: boolean;
        IdAdressNotArchive?: boolean;
        IdAdressInsertDate?: string;
        IdAdressInsertUserId?: number;
        IdAdressUpdateDate?: string;
        IdAdressUpdateUserId?: number;
        IdAdressAdress1?: string;
        IdAdressAdress2?: string;
        IdAdressAdress3?: string;
        IdAdressCity?: number;
        IdAdressCountry?: number;
        IdAdressCedex?: string;
        IdAdressBuilding?: string;
        IdAdressCaption?: string;
        IdAdressArchiveDate?: string;
    }

    export namespace CorCorporateRow {
        export const idProperty = 'Id';
        export const nameProperty = 'Name';
        export const localTextPrefix = 'Ge.CorCorporate';

        export namespace Fields {
            export declare const Id;
            export declare const IsActive;
            export declare const NotArchive;
            export declare const InsertDate;
            export declare const InsertUserId;
            export declare const UpdateDate;
            export declare const UpdateUserId;
            export declare const Name;
            export declare const Phone;
            export declare const Gsm;
            export declare const OtherPhone1;
            export declare const Email1;
            export declare const Email2;
            export declare const BankAccount;
            export declare const IdAdress;
            export declare const FrSiren;
            export declare const Caption;
            export declare const ArchiveDate;
            export declare const BankAccountCountryCodeIban;
            export declare const BankAccountCheckDigitsIban;
            export declare const BankAccountBban;
            export declare const BankAccountBic;
            export declare const BankAccountAccountOnwer;
            export declare const BankAccountBankAdress;
            export declare const BankAccountCis;
            export declare const BankAccountUrm;
            export declare const BankAccountMandateDateSign;
            export declare const BankAccountTypePayment;
            export declare const BankAccountDateLastPrelevement;
            export declare const BankAccountTypeOfLastPayment;
            export declare const IdAdressIsActive;
            export declare const IdAdressNotArchive;
            export declare const IdAdressInsertDate;
            export declare const IdAdressInsertUserId;
            export declare const IdAdressUpdateDate;
            export declare const IdAdressUpdateUserId;
            export declare const IdAdressAdress1;
            export declare const IdAdressAdress2;
            export declare const IdAdressAdress3;
            export declare const IdAdressCity;
            export declare const IdAdressCountry;
            export declare const IdAdressCedex;
            export declare const IdAdressBuilding;
            export declare const IdAdressCaption;
            export declare const IdAdressArchiveDate;
        }

        [
            'Id',
            'IsActive',
            'NotArchive',
            'InsertDate',
            'InsertUserId',
            'UpdateDate',
            'UpdateUserId',
            'Name',
            'Phone',
            'Gsm',
            'OtherPhone1',
            'Email1',
            'Email2',
            'BankAccount',
            'IdAdress',
            'FrSiren',
            'Caption',
            'ArchiveDate',
            'BankAccountCountryCodeIban',
            'BankAccountCheckDigitsIban',
            'BankAccountBban',
            'BankAccountBic',
            'BankAccountAccountOnwer',
            'BankAccountBankAdress',
            'BankAccountCis',
            'BankAccountUrm',
            'BankAccountMandateDateSign',
            'BankAccountTypePayment',
            'BankAccountDateLastPrelevement',
            'BankAccountTypeOfLastPayment',
            'IdAdressIsActive',
            'IdAdressNotArchive',
            'IdAdressInsertDate',
            'IdAdressInsertUserId',
            'IdAdressUpdateDate',
            'IdAdressUpdateUserId',
            'IdAdressAdress1',
            'IdAdressAdress2',
            'IdAdressAdress3',
            'IdAdressCity',
            'IdAdressCountry',
            'IdAdressCedex',
            'IdAdressBuilding',
            'IdAdressCaption',
            'IdAdressArchiveDate'
        ].forEach(x => (<any>Fields)[x] = x);
    }
}