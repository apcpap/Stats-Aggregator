-module(sa_helper).

-include_lib("eunit/include/eunit.hrl").

-export([
         stringlist_to_intlist/1,
         remove_empty_items/1,
         get_time_granularity/1,
         pad_int_list_r/3,
         fill_time_list/2,
         fill_time_list/1,
         unfill_time_list/1,
         time_to_list/1,
         list_to_time/1,
         is_intlist_inbetween/3,
         is_intlist_leq/2,
         is_intlist_geq/2,
         list_has_prefix/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

remove_empty_items(L) ->
    lists:foldr(fun(E,A) ->
                    case E of
                        []  -> A;
                        _   -> [E | A]
                    end
                end, [], L).

stringlist_to_intlist(L) ->
    lists:foldr(fun(E,A) ->
                    {I, _} = string:to_integer(E),
                    [I | A]
                end, [], L).

fill_time_list(A) ->
    fill_time_list(A, 1).

fill_time_list(A, E) ->
    case length(A) < 6 of
        true  -> fill_time_list([-1 | A], E);
        false -> A ++ [E]
    end.

pad_int_list_r(List, Length, PadElement) ->
    case length(List) < Length of
        true  -> pad_int_list_r(List ++ [PadElement], Length, PadElement);
        false -> List
    end.


unfill_time_list(A) ->
    case A of
        [E | B] ->
            case E of
                -1 -> unfill_time_list(B);
                _  -> lists:sublist(A, length(A) - 1)
            end
    end.

get_time_granularity(A) ->
    get_time_granularity(A, 6).

get_time_granularity(A, Cnt) ->
    case A of
        [] -> Cnt;
        [E | B] ->
            case E of
                -1 -> get_time_granularity(B, Cnt-1);
                _  -> Cnt
            end
    end.

time_to_list({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    [Year, Month, Day, Hour, Minute, Second].

list_to_time(L) ->
    case L of
        [Year, Month, Day, Hour, Minute, Second]    ->  {{Year, Month, Day}, {Hour, Minute, Second}};
        [Year, Month, Day, Hour, Minute]            ->  {{Year, Month, Day}, {Hour, Minute, 0}};
        [Year, Month, Day, Hour]                    ->  {{Year, Month, Day}, {Hour, 0, 0}};
        [Year, Month, Day]                          ->  {{Year, Month, Day}, {0, 0, 0}};
        [Year, Month]                               ->  {{Year, Month, 1}, {0, 0, 0}};
        [Year]                                      ->  {{Year, 1, 1}, {0, 0, 0}};
        []                                          ->  {{0, 1, 1}, {0, 0, 0}}
    end.

is_intlist_inbetween(TestTimeList, StartTimeList, EndTimeList) ->
    is_intlist_geq(TestTimeList, StartTimeList) andalso
    is_intlist_leq(TestTimeList, EndTimeList).

is_intlist_leq(L0, L1) ->
    is_intlist_geq(L1, L0).

is_intlist_geq(L0, L1) ->
    case {L0, L1} of
        {[], []}    ->  true;
        {_,  []}    ->  is_intlist_geq(L0, [0]);
        {[],  _}    ->  is_intlist_geq([0], L1);
        {[L0_E | L0_Rest], [L1_E | L1_Rest]} ->
            if
                L0_E > L1_E     -> true;
                L0_E =:= L1_E   -> is_intlist_geq(L0_Rest, L1_Rest);
                true            -> false
            end
    end.

list_has_prefix(Prefix, List2) ->
    case {Prefix, List2} of
        {[], _}                 -> true;
        {[A | L0], [A | L1]}    -> list_has_prefix(L0, L1);
        _                       -> false
    end.


intlist_compare_test() ->
    false   =   is_intlist_leq([1]  ,  [0]),
    true    =   is_intlist_leq([1]  ,  [1]),
    true    =   is_intlist_leq([1]  ,  [2]),
    false   =   is_intlist_leq([1,1]  ,  [1]),
    true    =   is_intlist_leq([0,1]  ,  [1]),
    true    =   is_intlist_leq([1,0]  ,  [1]),
    false   =   is_intlist_leq([1,1]  ,  [1]),
    true    =   is_intlist_leq([1]  ,  [1,0]),
    true    =   is_intlist_leq([1]  ,  [1,1]),

    true     =   is_intlist_geq([1]  ,  [1]),
    false    =   is_intlist_geq([1]  ,  [1,1]),
    false    =   is_intlist_geq([1]  ,  [2]),
    true     =   is_intlist_geq([1,1]  ,  [1]),
    false    =   is_intlist_geq([0,1]  ,  [1,2]),
    ok.