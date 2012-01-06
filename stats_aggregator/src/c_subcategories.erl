-module (c_subcategories).
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
%        User = wrq:get_qs_value("user", ReqData),
	{Authorized, Category} =
		case wrq:method(ReqData) of
			% only owner can get
			'GET' -> 
                            Cat = sa_helper:remove_empty_items(re:split([?SEPARATOR_CHAR] ++ dict:fetch(category, wrq:path_info(ReqData)), [?SEPARATOR_CHAR], [{return, list}])),
                            User = dict:fetch(user, wrq:path_info(ReqData)),
                            Auth = sa_auth:is_authorized(User, dict:fetch(password, wrq:path_info(ReqData)), Cat),
                            stats_aggregator:add_event(["monitor", "statsqueries", User], 1),
                            {Auth, Cat}
		end,
	{Authorized, ReqData, {PostData, Category}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, {PostData, Category}) ->
        L = stats_aggregator:get_subcategories(Category),
        JSONItems = ["{" ++ sa_webserver:str_to_json("cat", sa_webserver:escape_list_to_string(CatList)) ++ ", " ++ sa_webserver:str_to_json("count", integer_to_list(Count)) ++ "}"
                     || {CatList, Count} <- L],
        Body = "{" ++ sa_webserver:list_to_json_array("items", JSONItems) ++ "}",
        Response =
            case wrq:get_qs_value("jsoncallback", ReqData) of
                undefined ->    Body;
                Callback  ->    Callback ++ "(" ++ Body ++ ")"
            end,
        {Response, ReqData, {PostData, Category}}.

