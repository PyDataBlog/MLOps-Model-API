package rds_region

import (
	. "aliyun-openapi-go-sdk/core"
	"strconv"
)

type CreateBackupRequest struct {
	RpcRequest
	OwnerId              int
	ResourceOwnerAccount string
	ResourceOwnerId      int
	DBInstanceId         string
	DBName               string
	BackupMethod         string
	BackupType           string
	OwnerAccount         string
}

func (r *CreateBackupRequest) SetOwnerId(value int) {
	r.OwnerId = value
	r.QueryParams.Set("OwnerId", strconv.Itoa(value))
}
func (r *CreateBackupRequest) GetOwnerId() int {
	return r.OwnerId
}
func (r *CreateBackupRequest) SetResourceOwnerAccount(value string) {
	r.ResourceOwnerAccount = value
	r.QueryParams.Set("ResourceOwnerAccount", value)
}
func (r *CreateBackupRequest) GetResourceOwnerAccount() string {
	return r.ResourceOwnerAccount
}
func (r *CreateBackupRequest) SetResourceOwnerId(value int) {
	r.ResourceOwnerId = value
	r.QueryParams.Set("ResourceOwnerId", strconv.Itoa(value))
}
func (r *CreateBackupRequest) GetResourceOwnerId() int {
	return r.ResourceOwnerId
}
func (r *CreateBackupRequest) SetDBInstanceId(value string) {
	r.DBInstanceId = value
	r.QueryParams.Set("DBInstanceId", value)
}
func (r *CreateBackupRequest) GetDBInstanceId() string {
	return r.DBInstanceId
}
func (r *CreateBackupRequest) SetDBName(value string) {
	r.DBName = value
	r.QueryParams.Set("DBName", value)
}
func (r *CreateBackupRequest) GetDBName() string {
	return r.DBName
}
func (r *CreateBackupRequest) SetBackupMethod(value string) {
	r.BackupMethod = value
	r.QueryParams.Set("BackupMethod", value)
}
func (r *CreateBackupRequest) GetBackupMethod() string {
	return r.BackupMethod
}
func (r *CreateBackupRequest) SetBackupType(value string) {
	r.BackupType = value
	r.QueryParams.Set("BackupType", value)
}
func (r *CreateBackupRequest) GetBackupType() string {
	return r.BackupType
}
func (r *CreateBackupRequest) SetOwnerAccount(value string) {
	r.OwnerAccount = value
	r.QueryParams.Set("OwnerAccount", value)
}
func (r *CreateBackupRequest) GetOwnerAccount() string {
	return r.OwnerAccount
}

func (r *CreateBackupRequest) Init() {
	r.RpcRequest.Init()
	r.SetVersion(Version)
	r.SetAction("CreateBackup")
	r.SetProduct(Product)
}

type CreateBackupResponse struct {
}

func CreateBackup(req *CreateBackupRequest, accessId, accessSecret string) (*CreateBackupResponse, error) {
	var pResponse CreateBackupResponse
	body, err := ApiHttpRequest(accessId, accessSecret, req)
	if err != nil {
		return nil, err
	}
	ApiUnmarshalResponse(req.GetFormat(), body, &pResponse)
	return &pResponse, err
}
