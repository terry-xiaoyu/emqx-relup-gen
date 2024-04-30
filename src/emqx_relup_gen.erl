-module(emqx_relup_gen).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = emqx_relup_gen_prv:init(State),
    {ok, State1}.
