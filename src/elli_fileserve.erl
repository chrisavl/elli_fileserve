%% @doc Elli fileserve overview
%%
%% This middleware serves static files given a URL prefix and a local path,
%% any request containing "/../" is ignored.

-module(elli_fileserve).
-behaviour(elli_handler).

-compile({parse_transform, ct_expand}).

-include_lib("kernel/include/file.hrl").

-export([handle/2, handle_event/3]).

%% exported for mockability in tests (called through ?MODULE)
-export([file_size/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.


handle(Req, Config) ->
    case unprefix(elli_request:raw_path(Req), prefix(Config)) of
        undefined ->
            ignore;
        FilePath ->
            Filename = local_path(Config, FilePath),
            case ?MODULE:file_size(Filename) of
                {error, illegal_path} ->
                    {403, [], <<"Not Allowed">>};
                {error, _Reason} ->
                    {404, [], <<"File Not Found">>};
                {ok, Size} ->
                    {200, headers(Filename, Size, charset(Config)), {file, Filename}}
            end
    end.

handle_event(_, _, _) ->
    ok.

%%
%% Config
%%

default(Config) ->
    proplists:get_value(default, Config, <<"index.html">>).

path(Config) ->
    proplists:get_value(path, Config, <<"/tmp">>).

prefix(Config) ->
    proplists:get_value(prefix, Config, <<>>).

charset(Config) ->
    proplists:get_value(charset, Config).

%%
%% Helpers
%%

unprefix(RawPath, {regex, Prefix}) ->
    case re:run(RawPath, Prefix, [{capture, all, binary}]) of
        nomatch ->
            undefined;
        _Result ->
            re:replace(RawPath, Prefix, "", [{return, binary}])
    end;

unprefix(RawPath, Prefix) ->
    PrefixSz = size(Prefix),
    case RawPath of
        <<Prefix:PrefixSz/binary, File/binary>> ->
            File;
        _ ->
            undefined
    end.

local_path(Config, <<"/", File/binary>>) ->
    local_path(Config, File);

local_path(Config, <<"">>) ->
    filename:join(filename:flatten([path(Config), default(Config)]));

local_path(Config, FilePath) ->
    MappedPath = path(Config),
    case binary:match(filename:dirname(FilePath), <<"..">>) of
        nomatch ->
            case binary:last(FilePath) of
                $/ ->
                    filename:join(filename:flatten([MappedPath, FilePath,
                                                    default(Config)]));
                _ ->
                    filename:join(filename:flatten([MappedPath, FilePath]))
            end;
        _       -> undefined
    end.

file_size(undefined) ->
    {error, illegal_path};

file_size(Filename) ->
    case file:read_file_info(Filename, [{time, posix}]) of
        {ok, #file_info{type = regular, access = Perm, size = Size}}
                when Perm =:= read orelse Perm =:= read_write ->
            {ok, Size};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, invalid_file}
    end.

headers(Filename, Size, Charset) ->
    case mime_type(Filename) of
        undefined ->
            [{"Content-Length", Size}];
        MimeType ->
            [{"Content-Length", Size}, {"Content-Type", content_type(MimeType, Charset)}]
    end.

content_type(MimeType, undefined) ->
    MimeType;
content_type(MimeType, Charset) ->
    MimeType ++ "; charset=" ++ Charset.

%%
%% Mime types
%%

mime_types() ->
    ct_expand:term(
        dict:from_list(element(2, httpd_conf:load_mime_types(
                    code:priv_dir(elli_fileserve) ++ "/mime.types")))).

mime_type(Filename) when is_binary(Filename) ->
    case filename:extension(Filename) of
        <<>> ->
            undefined;
        <<$., Ext/binary>> ->
            case dict:find(binary_to_list(Ext), mime_types()) of
                {ok, MimeType} -> MimeType;
                error          -> undefined
            end
    end.
