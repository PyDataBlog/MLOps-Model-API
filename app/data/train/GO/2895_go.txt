package cdn

import (
	. "aliyun-openapi-go-sdk/core"
	"strconv"
)

type OpenCdnServiceRequest struct {
	RpcRequest
	OwnerId              int
	ResourceOwnerAccount string
	ResourceOwnerId      int
	InternetChargeType   string
}

func (r *OpenCdnServiceRequest) SetOwnerId(value int) {
	r.OwnerId = value
	r.QueryParams.Set("OwnerId", strconv.Itoa(value))
}
func (r *OpenCdnServiceRequest) GetOwnerId() int {
	return r.OwnerId
}
func (r *OpenCdnServiceRequest) SetResourceOwnerAccount(value string) {
	r.ResourceOwnerAccount = value
	r.QueryParams.Set("ResourceOwnerAccount", value)
}
func (r *OpenCdnServiceRequest) GetResourceOwnerAccount() string {
	return r.ResourceOwnerAccount
}
func (r *OpenCdnServiceRequest) SetResourceOwnerId(value int) {
	r.ResourceOwnerId = value
	r.QueryParams.Set("ResourceOwnerId", strconv.Itoa(value))
}
func (r *OpenCdnServiceRequest) GetResourceOwnerId() int {
	return r.ResourceOwnerId
}
func (r *OpenCdnServiceRequest) SetInternetChargeType(value string) {
	r.InternetChargeType = value
	r.QueryParams.Set("InternetChargeType", value)
}
func (r *OpenCdnServiceRequest) GetInternetChargeType() string {
	return r.InternetChargeType
}

func (r *OpenCdnServiceRequest) Init() {
	r.RpcRequest.Init()
	r.SetVersion(Version)
	r.SetAction("OpenCdnService")
	r.SetProduct(Product)
}

type OpenCdnServiceResponse struct {
}

func OpenCdnService(req *OpenCdnServiceRequest, accessId, accessSecret string) (*OpenCdnServiceResponse, error) {
	var pResponse OpenCdnServiceResponse
	body, err := ApiHttpRequest(accessId, accessSecret, req)
	if err != nil {
		return nil, err
	}
	ApiUnmarshalResponse(req.GetFormat(), body, &pResponse)
	return &pResponse, err
}
