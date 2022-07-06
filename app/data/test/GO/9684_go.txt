package rds_region

import (
	. "aliyun-openapi-go-sdk/core"
	"strconv"
)

type UpgradeDBInstanceEngineVersionRequest struct {
	RpcRequest
	OwnerId              int
	ResourceOwnerAccount string
	ResourceOwnerId      int
	ClientToken          string
	DBInstanceId         string
	EngineVersion        string
	OwnerAccount         string
}

func (r *UpgradeDBInstanceEngineVersionRequest) SetOwnerId(value int) {
	r.OwnerId = value
	r.QueryParams.Set("OwnerId", strconv.Itoa(value))
}
func (r *UpgradeDBInstanceEngineVersionRequest) GetOwnerId() int {
	return r.OwnerId
}
func (r *UpgradeDBInstanceEngineVersionRequest) SetResourceOwnerAccount(value string) {
	r.ResourceOwnerAccount = value
	r.QueryParams.Set("ResourceOwnerAccount", value)
}
func (r *UpgradeDBInstanceEngineVersionRequest) GetResourceOwnerAccount() string {
	return r.ResourceOwnerAccount
}
func (r *UpgradeDBInstanceEngineVersionRequest) SetResourceOwnerId(value int) {
	r.ResourceOwnerId = value
	r.QueryParams.Set("ResourceOwnerId", strconv.Itoa(value))
}
func (r *UpgradeDBInstanceEngineVersionRequest) GetResourceOwnerId() int {
	return r.ResourceOwnerId
}
func (r *UpgradeDBInstanceEngineVersionRequest) SetClientToken(value string) {
	r.ClientToken = value
	r.QueryParams.Set("ClientToken", value)
}
func (r *UpgradeDBInstanceEngineVersionRequest) GetClientToken() string {
	return r.ClientToken
}
func (r *UpgradeDBInstanceEngineVersionRequest) SetDBInstanceId(value string) {
	r.DBInstanceId = value
	r.QueryParams.Set("DBInstanceId", value)
}
func (r *UpgradeDBInstanceEngineVersionRequest) GetDBInstanceId() string {
	return r.DBInstanceId
}
func (r *UpgradeDBInstanceEngineVersionRequest) SetEngineVersion(value string) {
	r.EngineVersion = value
	r.QueryParams.Set("EngineVersion", value)
}
func (r *UpgradeDBInstanceEngineVersionRequest) GetEngineVersion() string {
	return r.EngineVersion
}
func (r *UpgradeDBInstanceEngineVersionRequest) SetOwnerAccount(value string) {
	r.OwnerAccount = value
	r.QueryParams.Set("OwnerAccount", value)
}
func (r *UpgradeDBInstanceEngineVersionRequest) GetOwnerAccount() string {
	return r.OwnerAccount
}

func (r *UpgradeDBInstanceEngineVersionRequest) Init() {
	r.RpcRequest.Init()
	r.SetVersion(Version)
	r.SetAction("UpgradeDBInstanceEngineVersion")
	r.SetProduct(Product)
}

type UpgradeDBInstanceEngineVersionResponse struct {
	TaskId string `xml:"TaskId" json:"TaskId"`
}

func UpgradeDBInstanceEngineVersion(req *UpgradeDBInstanceEngineVersionRequest, accessId, accessSecret string) (*UpgradeDBInstanceEngineVersionResponse, error) {
	var pResponse UpgradeDBInstanceEngineVersionResponse
	body, err := ApiHttpRequest(accessId, accessSecret, req)
	if err != nil {
		return nil, err
	}
	ApiUnmarshalResponse(req.GetFormat(), body, &pResponse)
	return &pResponse, err
}
