# Serve static files with [Elli](https://github.com/knutin/elli)

This middleware allows you to serve static files with [Elli](https://github.com/knutin/elli) by mapping a prefix to a local folder on your server. A prefix can be of arbritrary length, the following are all valid prefixes: "/", "/prefix", "/some/longer/prefix/" etc.

You can also drop in your own MIME types/Content types by editing priv/mime.types before compiling.

Example config for serving local files in "/tmp" under "/prefix", eg GET /prefix/some/file.txt will serve /tmp/some/file.txt.


```erlang
-module(my_elli_stuff).
-export([start_link/0]).

start_link() ->
    FileserveConfig = [{prefix, <<"/prefix">>},
                       {path, <<"/tmp">>}],

    Config = [{mods, [{elli_fileserve, FileserveConfig}]}],

    elli:start_link([{callback, elli_middleware},
                     {callback_args, Config}]).
```

## Dynamic prefixes using regex

If your prefix is dynamic, use regular expressions to match it. The following example matches all pathes that contain `/assets` somewhere.

```erlang
FileserveConfig = [{prefix, {regex, <<".+/assets">>}},
                   {path, <<"/www">>}],
```

Resolves to:

| Path | Result |
|------|--------|
| `/foo/assets/file.zip` | `/www/file.zip` |
| `/bar/assets/file.zip` | `/www/file.zip` |
| `/assets/file.zip` | `ignore` |

# TODO

 * Serve index.htm(l) files for /-requests if available

 * Support file listing
