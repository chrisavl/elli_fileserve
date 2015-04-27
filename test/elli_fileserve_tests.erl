-module(elli_fileserve_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DIR, (filename:absname(<<".">>))).
-define(TEST_FILE, (filename:basename(list_to_binary(?FILE)))).
-define(TEST(FunName, Arg), {lists:flatten(io_lib:format("~s ~p", [FunName, Arg])), ?_test(FunName(Arg))}).

test_setup() ->
    Mods = [ [elli_fileserve, [passthrough]]
           , [elli_request]
           ],
    [ {hd(Args), apply(meck, new, Args)} || Args <- Mods ].

test_teardown(Mods) ->
    [ meck:unload(Mod) || {Mod, ok} <- Mods ].

handle_test_() ->
    [{setup, local,
      fun test_setup/0,
      fun test_teardown/1,
      [?TEST(test_for_prefix, <<"/prefix">>),
       ?TEST(test_for_prefix, <<"/prefix/">>),
       ?TEST(test_for_prefix, {regex, <<"^/p.+x/">>}),
       ?TEST(test_for_regex_prefix, <<".+/assets">>),
       ?TEST(test_for_regex_prefix, <<".+/assets/">>),
       ?TEST(test_for_regex_prefix, <<"/[^/]+/[^/]+/assets/">>),
       ?_test(test_default_charset()),
       ?_test(test_undefined_charset()),
       ?_test(test_charset_config())
      ]
     }].

test_for_prefix(Prefix) ->
    Config = [{path, ?TEST_DIR}, {prefix, Prefix}],

    meck:expect(elli_request, raw_path, 1, <<"/prefix/../file.ext">>),
    ?assertEqual({403, [], <<"Not Allowed">>}, elli_fileserve:handle(req, Config)),

    meck:expect(elli_request, raw_path, 1, <<"/prefix/non-existing-file.ext">>),
    ?assertEqual({404, [], <<"File Not Found">>}, elli_fileserve:handle(req, Config)),

    meck:expect(elli_request, raw_path, 1, <<"/prefix/", ?TEST_FILE/binary>>),
    ExpectedFile = filename:join([?TEST_DIR, ?TEST_FILE]),
    ?assertMatch({200, [{"Content-Length", _}], {file, ExpectedFile}},
                 elli_fileserve:handle(req, Config)),

    meck:expect(elli_request, raw_path, 1, <<"/other/prefix/file.ext">>),
    ?assertEqual(ignore, elli_fileserve:handle(req, Config)).


test_for_regex_prefix(Prefix) ->
    Config = [{path, ?TEST_DIR}, {prefix, {regex, Prefix}}],

    meck:expect(elli_request, raw_path, 1, <<"/deep/link/assets/", ?TEST_FILE/binary>>),
    ExpectedFile = filename:join([?TEST_DIR, ?TEST_FILE]),
    ?assertMatch({200, [{"Content-Length", _}], {file, ExpectedFile}},
                 elli_fileserve:handle(req, Config)).


test_default_charset() ->
    Config = [],
    meck:expect(elli_request, raw_path, 1, <<"/test.js">>),
    meck:expect(elli_fileserve, file_size, 1, {ok, 0}),
    ?assertMatch({200,
                  [{"Content-Length", 0},
                   {"Content-Type", "application/javascript"}],
                  {file, _}},
                 elli_fileserve:handle(req, Config)).

test_undefined_charset() ->
    Config = [{charset, undefined}],
    meck:expect(elli_request, raw_path, 1, <<"/test.js">>),
    meck:expect(elli_fileserve, file_size, 1, {ok, 0}),
    ?assertMatch({200,
                  [{"Content-Length", 0},
                   {"Content-Type", "application/javascript"}],
                  {file, _}},
                 elli_fileserve:handle(req, Config)).

test_charset_config() ->
    Config = [{charset, "mycharset"}],
    meck:expect(elli_request, raw_path, 1, <<"/test.js">>),
    meck:expect(elli_fileserve, file_size, 1, {ok, 0}),
    ?assertMatch({200,
                  [{"Content-Length", 0},
                   {"Content-Type", "application/javascript; charset=mycharset"}],
                  {file, _}},
                 elli_fileserve:handle(req, Config)).


mime_type_test() ->
    ?assertEqual("text/plain", elli_fileserve:mime_type(<<"/some/file.txt">>)),
    ?assertEqual(undefined, elli_fileserve:mime_type(<<"/no/file/extension">>)).


local_path_test_() ->
    [{"Should return file asked for",
      ?_assertEqual(<<"/test/css/main.css">>,
                    elli_fileserve:local_path([{path, <<"/test">>}],
                                              <<"css/main.css">>))},
     {"Should return default file on empty path",
      ?_assertEqual(<<"/test/index.xhtml">>,
                    elli_fileserve:local_path([{path, <<"/test">>},
                                               {default, <<"index.xhtml">>}],
                                              <<"">>))},
     {"Should return default file if asked for directory",
      ?_assertEqual(<<"/test/static/index.xhtml">>,
                    elli_fileserve:local_path([{path, <<"/test">>},
                                               {default, <<"index.xhtml">>}],
                                              <<"static/">>))},
     {"Should not allow 'parent directories' in path",
      ?_assertEqual(undefined,
                    elli_fileserve:local_path([{path, <<"/test/">>},
                                               {default, <<"index.xhtml">>}],
                                              <<"../">>))},
     {"Should serve absolute paths relative to 'path' directory",
      ?_assertEqual(<<"/test/etc/passwd">>,
                    elli_fileserve:local_path([{path, <<"/test">>}],
                                              <<"/etc/passwd">>))}].

unprefix_test_() ->
    [{"Empty prefix",
      ?_assertEqual(<<"/etc/passwd">>,
                    elli_fileserve:unprefix(<<"/etc/passwd">>, <<"">>))},
     {"Simple slash",
      ?_assertEqual(<<"etc/passwd">>,
                    elli_fileserve:unprefix(<<"/etc/passwd">>, <<"/">>))},
     {"Prefix",
      ?_assertEqual(<<"/etc/passwd">>,
                    elli_fileserve:unprefix(<<"/prefix/etc/passwd">>, <<"/prefix">>))},
     {"Prefix",
      ?_assertEqual(<<"etc/passwd">>,
                    elli_fileserve:unprefix(<<"/prefix/etc/passwd">>, <<"/prefix/">>))},
     {"Regex prefix",
      ?_assertEqual(<<"etc/passwd">>,
                    elli_fileserve:unprefix(<<"/prefix/etc/passwd">>, {regex, <<"^/p.+x/">>}))}].
