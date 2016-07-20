%%%-------------------------------------------------------------------
%%% @author hlst
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2016 4:20 PM
%%%-------------------------------------------------------------------
{application, simple_cache, [
  {description, "A simple cache system"},
  {vsn, "0.1.0"},
  {registered, [sc_sup]},
  {modules, [
    sc_app,
    sc_sup
  ]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {sc_app, []}},
  {env, []}
]}.