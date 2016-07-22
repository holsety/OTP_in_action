%%%-------------------------------------------------------------------
%%% @author hlst
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 4:15 PM
%%%-------------------------------------------------------------------
-module(simple_cache).
-author("hlst").

%% API
-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = sc_element:create(Value),
      sc_store:insert(Key, Pid)
  end.

lookup(Key) ->
  try
    {ok, Pid} = sc_store:lookup(Key),
    {ok, Value} = sc_element:fetch(Pid),
    {ok, Value}
  catch
    _Class:_Exception ->
      {error, not_found}
  end.

delete(Key) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.