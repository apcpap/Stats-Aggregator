-module(sa_auth).

-export([
         get_allowed_categories/2,
         is_authorized/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

get_allowed_categories(User, Password) ->
    case {User, Password} of
        {"admin", "admin"}          ->  [[]];
        {"customer1", "customer1"}  ->  [["apps", "app1"]];
        {"customer2", "customer2"}  ->  [["apps", "app2"], ["apps", "app3"]];
        _   -> auth_error
    end.

is_authorized(User, Password, CatList) ->
    test_authorization(get_allowed_categories(User, Password), CatList).


%%%===================================================================
%%% Internal functions
%%%===================================================================

test_authorization(AuthCatLists, CatList) ->
    case AuthCatLists of
        []  -> false;
        [ACatList | R] ->
            case sa_helper:list_has_prefix(ACatList, CatList) of
                true    -> true;
                _       -> test_authorization(R, CatList)
            end
    end.
