%%%-------------------------------------------------------------------
%%% @author hlst
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 4:16 PM
%%%-------------------------------------------------------------------
-module(sc_store).
-author("hlst").

%% API
-export([
  init/0,
  insert/2,
  delete/1,
  lookup/1
]).

-define(TABLE_ID, ?MODULE).
init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Key, Pid) ->
  ets:insert(?TABLE_ID, {Key, Pid}).
lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] -> {ok, Pid};
    []           -> {error, not_found}
  end.

delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).
