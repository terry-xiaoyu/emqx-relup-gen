-module(emqx_relup_gen_prv_tests).

-include_lib("eunit/include/eunit.hrl").

gen_compelte_relup0_test() ->
    Relups =
    [#{target_version => "5.6.1", from_version => "5.6.0",
       code_changes => [0], post_upgrade_callbacks => [0]}],
    CompleteRelup = emqx_relup_gen_prv:gen_compelte_relup(create_vsn_index(Relups),
                        "5.6.1", ["5.6.0"]),
    ?assertEqual([
        #{target_version => "5.6.1", from_version => "5.6.0",
          code_changes => [0], post_upgrade_callbacks => [0]}
        ], CompleteRelup).

gen_compelte_relup1_test() ->
    Relups =
    [#{target_version => "5.6.1", from_version => "5.6.0",
       code_changes => [0], post_upgrade_callbacks => [0]},
     #{target_version => "5.6.2", from_version => "5.6.1",
       code_changes => [1], post_upgrade_callbacks => [1]},
     #{target_version => "5.6.3", from_version => "5.6.2",
       code_changes => [2], post_upgrade_callbacks => [2]}],
    CompleteRelup = emqx_relup_gen_prv:gen_compelte_relup(create_vsn_index(Relups),
                        "5.6.3", ["5.6.2", "5.6.1", "5.6.0"]),
    ?assertEqual([
        #{target_version => "5.6.3", from_version => "5.6.2",
          code_changes => [2], post_upgrade_callbacks => [2]},
        #{target_version => "5.6.3", from_version => "5.6.1",
          code_changes => [1, 2], post_upgrade_callbacks => [1, 2]},
        #{target_version => "5.6.3", from_version => "5.6.0",
          code_changes => [0, 1, 2], post_upgrade_callbacks => [0, 1, 2]}
        ], CompleteRelup).

gen_compelte_relup2_test() ->
    Relups =
    [#{target_version => "5.6.1", from_version => "5.6.0",
       code_changes => [0], post_upgrade_callbacks => [0]},
     #{target_version => "5.6.2", from_version => "5.6.1",
       code_changes => [1], post_upgrade_callbacks => [1]},
     #{target_version => "5.6.3", from_version => "5.6.2",
       code_changes => [2], post_upgrade_callbacks => [2]},
     #{target_version => "5.6.3", from_version => "5.6.0",
       code_changes => [000], post_upgrade_callbacks => [000]}
    ],
    CompleteRelup = emqx_relup_gen_prv:gen_compelte_relup(create_vsn_index(Relups),
                        "5.6.3", ["5.6.2", "5.6.1", "5.6.0"]),
    ?assertEqual([
        #{target_version => "5.6.3", from_version => "5.6.2",
          code_changes => [2], post_upgrade_callbacks => [2]},
        #{target_version => "5.6.3", from_version => "5.6.1",
          code_changes => [1, 2], post_upgrade_callbacks => [1, 2]},
        #{target_version => "5.6.3", from_version => "5.6.0",
          code_changes => [000], post_upgrade_callbacks => [000]}
        ], CompleteRelup).

gen_compelte_relup_err0_test() ->
    Relups = [],
    ?assertThrow(
        {relup_not_found, #{from := "5.6.0", target := "5.6.1"}},
        emqx_relup_gen_prv:gen_compelte_relup(create_vsn_index(Relups), "5.6.1", ["5.6.0"]),
        "Should throw relup_not_found when no relup found"
    ).

gen_compelte_relup_err1_test() ->
    Relups = [
    #{target_version => "5.6.1", from_version => "5.6.0",
       code_changes => [0], post_upgrade_callbacks => [0]},
     #{target_version => "5.6.2", from_version => "5.6.1",
       code_changes => [1], post_upgrade_callbacks => [1]}],
    ?assertThrow(
        {relup_not_found, #{from := "5.6.2", target := "5.6.3"}},
        emqx_relup_gen_prv:gen_compelte_relup(create_vsn_index(Relups),
            "5.6.3", ["5.6.2", "5.6.1", "5.6.0"]),
        "Should throw relup_not_found when a upgrade relup missing"
    ).

create_vsn_index(Relups) ->
    lists:foldl(fun(Relup, Acc) ->
        maps:put({maps:get(target_version, Relup), maps:get(from_version, Relup)}, Relup, Acc)
    end, #{}, Relups).
