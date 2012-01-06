-module (c_authcategories).
-export([
	init/1,
	content_types_provided/2,
	to_json/2,
	allowed_methods/2,
	is_authorized/2,
	malformed_request/2
	]).

-include_lib("webmachine/include/webmachine.hrl").

-define(SEPARATOR_CHAR, $!).

init(_) ->
	{ok, undefined}.

allowed_methods(ReqData, Context) ->
	{['GET'], ReqData, Context}.

malformed_request(ReqData, _) ->
	{Invalid, PostData} = case wrq:method(ReqData) of
		'GET' ->
			{false, {}}
	end,
	{Invalid, ReqData, {PostData}}.

is_authorized(ReqData, {PostData}) ->
	{true, ReqData, {PostData}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, {PostData}) ->
        User = dict:fetch(user, wrq:path_info(ReqData)),
        Password = dict:fetch(password, wrq:path_info(ReqData)),
        AllowedCats = sa_auth:get_allowed_categories(User, Password),
        EscapedAuthCats = [ sa_webserver:escape_list_to_string(E) || E <- AllowedCats],
        JSONCats = [ "{" ++ sa_webserver:str_to_json("cat", E) ++ "}" || E <- EscapedAuthCats],
        Body = "{" ++ sa_webserver:list_to_json_array("allowedcats", JSONCats) ++ "}",
        Response =
            case wrq:get_qs_value("jsoncallback", ReqData) of
                undefined ->    Body;
                Callback  ->    Callback ++ "(" ++ Body ++ ")"
            end,
        {Response, ReqData, {PostData}}.

