# TermInfo

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ref_id** | **string** | The ID (GUID) that uniquely identifies this TermInfo entity. | 
**school_info_ref_id** | **string** | The ID (GUID) that identifies the school where the term is used. | 
**school_year** | **string** |  | 
**start_date** | [**\DateTime**](Date.md) | Starting date of the term. | 
**end_date** | [**\DateTime**](Date.md) | Ending date of the term. | 
**description** | **string** | Text-based description of the term. | [optional] 
**term_code** | **string** | Locally-defined code. Use &amp;#039;AUT&amp;#039;, &amp;#039;SPR&amp;#039;, or &amp;#039;SUM&amp;#039; for a 3 term year. Otherwise specify the term number within the year. | [optional] 
**terms_per_year** | **int** |  | [optional] 
**marking_term** | **string** | Does this TermInfo represent a marking period? | [optional] 
**scheduling_term** | **string** | Does this TermInfo represent a scheduling term? | [optional] 
**attendance_term** | **string** | Does this TermInfo represent an attendance term? | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


