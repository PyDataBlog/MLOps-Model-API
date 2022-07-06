/*
 * Created by zhye at 2013-11-20 9:23:04; 
 *
 *
 *
 *
 *
 * Copyright:Strong
 */
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Dapper;
using Model;

namespace DAL
{
    public class dtEquipAcceptAdapter    
    {   
        /// <summary>
        /// 获取方法
        /// </summary>
        public list<Model.dtEquipAccept> GetList(string strFilter)
        {
            private _strSelectSql = @"select             
                ID,
                BillCode,
                BillDate,
                bsAccountID,
                trDeptID,
                dtProjectID,
                ProjectCode,
                ProjectName,
                dtPurchaseID,
                ContractCode,
                bsSupplierID,
                ReceiveGoodsMan,
                ReceiveGoodsDate,
                ManufactureDate,
                CheckAcceptDate,
                ReceiveSuggest,
                Remark,
                CreateMan,
                CreateDate,
                EditMan,
                EditDate,
                tpBillStateID
            from dtEquipAccept";
            using (SqlConnection conn = DapperFactory.CrateOpenConnection())
            {
                return conn.QueryMultiple(strSql.ToString(), MixCondition, commandType: CommandType.Text).Read<dtPurchaseMdl>().ToList();
            }    
        }
        /// <summary>
        /// 新增方法
        /// </summary>
        public bool Insert(Model.dtEquipAccept entity)
        {
            //StringBuilder strSql = new StringBuilder();
            string _strInsertSql = @"insert into dtEquipAccept(
                ID,
                BillCode,
                BillDate,
                bsAccountID,
                trDeptID,
                dtProjectID,
                ProjectCode,
                ProjectName,
                dtPurchaseID,
                ContractCode,
                bsSupplierID,
                ReceiveGoodsMan,
                ReceiveGoodsDate,
                ManufactureDate,
                CheckAcceptDate,
                ReceiveSuggest,
                Remark,
                CreateMan,
                CreateDate,
                EditMan,
                EditDate,
                tpBillStateID)
            values (
                @ID,
                @BillCode,
                @BillDate,
                @bsAccountID,
                @trDeptID,
                @dtProjectID,
                @ProjectCode,
                @ProjectName,
                @dtPurchaseID,
                @ContractCode,
                @bsSupplierID,
                @ReceiveGoodsMan,
                @ReceiveGoodsDate,
                @ManufactureDate,
                @CheckAcceptDate,
                @ReceiveSuggest,
                @Remark,
                @CreateMan,
                @CreateDate,
                @EditMan,
                @EditDate,
                @tpBillStateID)";
            using (SqlConnection conn = DapperFactory.CrateOpenConnection())
            {
                return conn.Execute(strSql.ToString(), model, commandType: CommandType.Text) > 0 ? true : false;
            }    
        }
        /// <summary>
        /// 修改方法
        /// </summary>
        public void Update(Model.dtEquipAccept entity)
        {
            string _strUpdateSql = @"Update  dtEquipAccept set
                ID = @ID, 
                BillCode = @BillCode, 
                BillDate = @BillDate, 
                bsAccountID = @bsAccountID, 
                trDeptID = @trDeptID, 
                dtProjectID = @dtProjectID, 
                ProjectCode = @ProjectCode, 
                ProjectName = @ProjectName, 
                dtPurchaseID = @dtPurchaseID, 
                ContractCode = @ContractCode, 
                bsSupplierID = @bsSupplierID, 
                ReceiveGoodsMan = @ReceiveGoodsMan, 
                ReceiveGoodsDate = @ReceiveGoodsDate, 
                ManufactureDate = @ManufactureDate, 
                CheckAcceptDate = @CheckAcceptDate, 
                ReceiveSuggest = @ReceiveSuggest, 
                Remark = @Remark, 
                CreateMan = @CreateMan, 
                CreateDate = @CreateDate, 
                EditMan = @EditMan, 
                EditDate = @EditDate, 
                tpBillStateID = @tpBillStateID 
            where Id = @Id";
            using (SqlConnection conn = DapperFactory.CrateOpenConnection())
            {
                return conn.Execute(strSql.ToString(), model,commandType:CommandType.Text) > 0 ? true : false; 
            }    
        }
        /// <summary>
        /// 删除方法
        /// </summary>
        public bool Delete(int Id)
        {
            string _strDeleteSql = @"delete dtEquipAccept where Id = @Id";
            var Condition =new { ID = Id };
            using (SqlConnection conn = DapperFactory.CrateOpenConnection())
            {
                return conn.Execute(strSql.ToString(), Condition, commandType: CommandType.Text) > 0 ? true : false;
            }    
        }
    }
}    