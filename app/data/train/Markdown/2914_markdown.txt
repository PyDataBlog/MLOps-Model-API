# Cycle

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ref_id** | **string** | Unique GUID for this cycle | 
**school_info_ref_id** | **string** | The RefId of the SchoolInfo to which this Cycle belongs. This is optional to preserve backwards compatibility but it is very strongly advised that it should be correctly populated and considered mandatory in this version of the specification. | [optional] 
**cycle_name** | **string** |  | [optional] 
**cycle_days** | **int** | Number of days in cycle | 
**periods_per_day** | **int** | (Max) Number of periods per day (see Rectangular Grid Representation above) | 
**standard_period_list** | [**\DataExchange\SIFUK20\Models\Period[]**](Period.md) |  | 
**tt_day_list** | [**\DataExchange\SIFUK20\Models\TTDay[]**](TTDay.md) |  | 
**sif_extended_elements** | [**\DataExchange\SIFUK20\Models\SIFExtendedElement[]**](SIFExtendedElement.md) | Allows an agent to include data not yet defined within a SIF data object as name/value pairs. | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


