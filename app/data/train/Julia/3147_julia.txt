module Octokit
	using HttpCommon
	using HTTPClient
	import JSON

	immutable Endpoint
		url::URI
	end

	const default_endpoint = Endpoint(URI("https://api.github.com"))

	function api_uri(endpoint,path,query,userinfo="") 
		uri = endpoint.url
		URI(uri.schema,uri.host,uri.port,joinpath(uri.path,path),query,"",userinfo,uri.specifies_authority)
	end

	immutable APIRequestError <: Exception
		message::String
	end

	immutable InvalidDataError <: Exception
		message::String
		errors::Array
	end

	function api_error(resp::Response)
		if resp.status == 400
			throw(APIRequestError(JSON.parse(resp.data)["message"]))
		elseif resp.status == 422
			p = JSON.parse(resp.data)
			throw(InvalidDataError(p["message"],p["errors"]))
		elseif resp.status == 404
			throw(APIRequestError("404: Not found"))
		end
		resp
	end

	# Authentication

	abstract Credentials

	immutable BasicAuth <: Credentials
		username::String
		password::String
	end

	immutable OAuthToken <: Credentials
		token::String
	end

	immutable OAuthSecret <: Credentials
		client_id::String
		client_secret::String
	end

	immutable Anonymous <: Credentials
	end

	do_query(path, parameters, auth::Credentials = Anonymous(), endpoint = default_endpoint) =
		api_error(_do_request(path,parameters,auth,endpoint))

	_do_query(path,parameters,auth::Anonymous,endpoint) = HTTPClient.get(api_uri(endpoint,path,""))
	_do_query(path,parameters,auth::BasicAuth,endpoint) = HTTPClient.get(api_uri(endpoint,path,"","$(auth.username):$(auth.password)"))
	_do_query(path,parameters,auth::OAuthToken,endpoint) = HTTPClient.get(api_uri(endpoint,path,""),headers={"Authorization"=>"token $(auth.token)"})
	function _do_query(path,parameters,auth::OAuthSecret,endpoint) 
		params = {"client_secret" => auth.client_secret, "client_id" => auth.client_id}
		merge!(params,parameters)
		_do_query(path,params,Anonymous(),endpoint)
	end

	do_action(path, parameters, data, auth::Credentials = Anonymous(), endpoint = default_endpoint) =
		api_error(_do_action(path,parameters,data,auth,endpoint))

	_do_action(path,parameters,data,auth::Anonymous,endpoint) = HTTPClient.post(api_uri(endpoint,path,""),data)
	_do_action(path,parameters,data,auth::BasicAuth,endpoint) = HTTPClient.post(api_uri(endpoint,path,"","$(auth.username):$(auth.password)"),data)
	_do_action(path,parameters,data,auth::OAuthToken,endpoint) = HTTPClient.post(api_uri(endpoint,path,""),data,headers={"Authorization"=>"token $(auth.token)"})
	function _do_action(path,parameters,auth::OAuthSecret,endpoint) 
		params = {"client_secret" => auth.client_secret, "client_id" => auth.client_id}
		merge!(params,parameters)
		_do_action(path,params,data,Anonymous(),endpoint)
	end

	function createToken(auth::BasicAuth,scopes=["public_repo"],note="Created using Octokit.jl";endpoint=default_endpoint) 
		r = do_action("/authorizations",{},JSON.to_json({"scopes"=>scopes,"note"=>note}),auth,endpoint)
		d = JSON.parse(r.data)
		(d["id"],OAuthToken(d["token"]))
	end
end