package io.swagger.client.api;

import com.sun.jersey.api.client.GenericType;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import io.swagger.client.model.CodeSnippet;
import io.swagger.client.model.CodeSnippetList;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-03-11T02:09:38.462Z")
public class DefaultApi {
  private ApiClient apiClient;

  public DefaultApi() {
    this(Configuration.getDefaultApiClient());
  }

  public DefaultApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  
  /**
   * Creates a code snippet.
   * Creates a code snippet in the specified language.
   * @param codeSnippetBody Code snippet object.
   * @throws ApiException if fails to make API call
   */
  public void snipPost(CodeSnippet codeSnippetBody) throws ApiException {
    Object localVarPostBody = codeSnippetBody;
    
    // create path and map variables
    String localVarPath = "/snip".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    

    

    

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    
    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
    
  }
  
  /**
   * Retrieves the specified code snippet.
   * Retrieves the specified code snippet.
   * @param codeSnippetUuid Code snippet unique identifier.
   * @return CodeSnippet
   * @throws ApiException if fails to make API call
   */
  public CodeSnippet snipCodeSnippetUuidGet(String codeSnippetUuid) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'codeSnippetUuid' is set
    if (codeSnippetUuid == null) {
      throw new ApiException(400, "Missing the required parameter 'codeSnippetUuid' when calling snipCodeSnippetUuidGet");
    }
    
    // create path and map variables
    String localVarPath = "/snip/{code_snippet_uuid}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "code_snippet_uuid" + "\\}", apiClient.escapeString(codeSnippetUuid.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    

    

    

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    
    GenericType<CodeSnippet> localVarReturnType = new GenericType<CodeSnippet>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    
  }
  
  /**
   * Updates a code snippet.
   * Updates a code snippet changes.
   * @param codeSnippetUuid Code snippet unique identifier.
   * @param codeSnippetBody Code snippet object.
   * @throws ApiException if fails to make API call
   */
  public void snipCodeSnippetUuidPut(String codeSnippetUuid, CodeSnippet codeSnippetBody) throws ApiException {
    Object localVarPostBody = codeSnippetBody;
    
    // verify the required parameter 'codeSnippetUuid' is set
    if (codeSnippetUuid == null) {
      throw new ApiException(400, "Missing the required parameter 'codeSnippetUuid' when calling snipCodeSnippetUuidPut");
    }
    
    // create path and map variables
    String localVarPath = "/snip/{code_snippet_uuid}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "code_snippet_uuid" + "\\}", apiClient.escapeString(codeSnippetUuid.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    

    

    

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    
    apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
    
  }
  
  /**
   * Deletes the specified code snippet.
   * Deletes the specified code snippet.
   * @param codeSnippetUuid Code snippet unique identifier.
   * @throws ApiException if fails to make API call
   */
  public void snipCodeSnippetUuidDelete(String codeSnippetUuid) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'codeSnippetUuid' is set
    if (codeSnippetUuid == null) {
      throw new ApiException(400, "Missing the required parameter 'codeSnippetUuid' when calling snipCodeSnippetUuidDelete");
    }
    
    // create path and map variables
    String localVarPath = "/snip/{code_snippet_uuid}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "code_snippet_uuid" + "\\}", apiClient.escapeString(codeSnippetUuid.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    

    

    

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    
    apiClient.invokeAPI(localVarPath, "DELETE", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
    
  }
  
  /**
   * Retrieves all code snippets.
   * 
   * @return CodeSnippetList
   * @throws ApiException if fails to make API call
   */
  public CodeSnippetList snipsGet() throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/snips".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    

    

    

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    
    GenericType<CodeSnippetList> localVarReturnType = new GenericType<CodeSnippetList>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    
  }
  
}
