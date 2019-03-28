%%%-------------------------------------------------------------------
%%% File    : memory_server.erl
%%% Author  : Ryan Crum <ryan.j.crum@gmail.com>
%%% Description : A simplistic and useless implementation of a
%%%               bifrost server.
%%%-------------------------------------------------------------------

-module(ftp_memory_server_producer).
-include_lib("bifrost/include/bifrost.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_bifrost_server).

% Bifrost callbacks
-export([login/3,
         init/2,
         current_directory/1,
         make_directory/2,
         change_directory/2,
         list_files/2,
         remove_directory/2,
         remove_file/2,
         put_file/4,
         get_file/2,
         file_info/2,
         rename_file/3,
         site_command/3,
         site_help/1,
         disconnect/1]).

% The structure for the Server's state. This is where you'll keep state about the current
% user's session in your own module.
-record(msrv_state,
        {
          current_dir = [[]],
          fs = new_directory("/")
         }).

% Initialize the state
init(InitialState, _) ->
    InitialState.

% Authenticate the user. Return {false, State} to fail.
login(State, _Username, _Password) ->
    {true, initialize_state(State)}.

% Return the current directory.
current_directory(State) ->
    case current_directory_list(State) of
        [[]] ->
            "/";
        Path ->
            string:join(Path, "/")
    end.

% mkdir -- in this case, just alter the state since we're in-memory.
make_directory(State, Directory) ->
    Target = absolute_path(State, Directory),
    Fs = get_fs(get_module_state(State)),
    case fetch_path(Fs, Target) of
        not_found ->
            {ok, set_state_fs(State,
                              set_path(Fs,
                                       Target,
                                       new_directory(lists:last(Target))))};
        _ ->
            {error, State}
    end.

% Change the current directory.
change_directory(State, Directory) ->
    Target = absolute_path(State, Directory),
    Fs = get_fs(get_module_state(State)),
    case fetch_path(Fs, Target) of
        not_found ->
            {error, State};
        {file, _, _} ->
            {error, State};
        {dir, _, _} ->
            ModState = get_module_state(State),
            NewModState = ModState#msrv_state{current_dir=Target},
            {ok, set_module_state(State, NewModState)};
        _ ->
            {error, State}
    end.

disconnect(_) ->
    ok.

% Delete a file
remove_file(State, File) ->
    Target = absolute_path(State, File),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    case fetch_path(Fs, Target) of
        not_found ->
            {error, not_found};
        {dir, _, _} ->
            {error, not_file};
        {file, _, _} ->
            NewModState = ModState#msrv_state{fs=set_path(Fs,
                                                          Target,
                                                          remove)},
            {ok, set_module_state(State, NewModState)};
        _ ->
            {error, unknown}
    end.

% Renaming is not yet supported by this module. Return {ok, State} if you support it.
rename_file(_State, _FromPath, _ToPath) ->
    {error, not_supported}.

% Delete a directory.
remove_directory(State, Directory) ->
    Target = absolute_path(State, Directory),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    case fetch_path(Fs, Target) of
        not_found ->
            {error, not_found};
        {file, _, _} ->
            {error, not_directory};
        {dir, Dict, _} ->
            DictSize = dict:size(Dict),
            if DictSize > 0 ->
                    {error, not_empty};
               true ->
                    NewModState = ModState#msrv_state{fs=set_path(Fs,
                                                                  Target,
                                                                  remove)},
                    {ok, set_module_state(State, NewModState)}
            end;
        _ ->
            {error, unknown}
    end.

% List files in the current or specified directory.
list_files(State, "") ->
    list_files(State, current_directory(State));
list_files(State, Directory) ->
    Target = absolute_path(State, Directory),
    Fs = get_fs(get_module_state(State)),
    case fetch_path(Fs, Target) of
        not_found ->
            {error, State};
        {dir, Dict, DirInfo} ->
            {dir, _, ParentInfo} = fetch_parent(Fs, Target),
            lists:map(fun({_,{_, _, Info}}) -> Info end,
                      dict:to_list(Dict)) ++
                [DirInfo#file_info{name = "."},
                 ParentInfo#file_info{name = ".."}];
        {file, _, Info} ->
            [Info];
        _ ->
            {error, State}
    end.

% mode could be append or write, but we're only supporting
% write.
% FileRetrievalFun is fun() and returns {ok, Bytes, Count} or done
put_file(State, ProvidedFileName, _Mode, FileRetrievalFun) ->
    FileName = lists:last(string:tokens(ProvidedFileName, "/")),
    Target = absolute_path(State, FileName),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    {ok, FileBytes, FileSize} = read_from_fun(FileRetrievalFun),
    NewFs= set_path(Fs, Target, {file,
                                 FileBytes,
                                 new_file_info(FileName, file, FileSize)}),
    NewModState = ModState#msrv_state{fs=NewFs},
    file_producer:put_file(Fs, Target, FileBytes, FileSize),
    {ok, set_module_state(State, NewModState)}.

% Returns {ok, fun(ByteCount)}, which is a function that reads ByteCount byes
% and itself returns a continuation until {done, State} is returned.
get_file(State, Path) ->
    Target = absolute_path(State, Path),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    case fetch_path(Fs, Target) of
        {file, Contents, _} ->
            {ok, reading_fun(State, Contents)};
        _ ->
            error
    end.

% Returns a file_info struct about the given path
file_info(State, Path) ->
    Target = absolute_path(State, Path),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    case fetch_path(Fs, Target) of
        {file, _, Info} ->
            {ok, Info};
        _ ->
            {error, not_found}
    end.

% SITE command support
site_command(_, _, _) ->
    {error, not_found}.

site_help(_) ->
    {error, not_found}.

% Memory Server-specific Functions

read_from_fun(Fun) ->
    read_from_fun([], 0, Fun).
read_from_fun(Buffer, Count, Fun) ->
    case Fun() of
        {ok, Bytes, ReadCount} ->
            read_from_fun(Buffer ++ [Bytes], Count + ReadCount, Fun);
        done ->
            {ok, Buffer, Count}
    end.

reading_fun(State, Bytes) ->
    reading_fun(State, 0, Bytes).
reading_fun(State, _, []) ->
    fun(_) ->
            {done, State}
    end;
reading_fun(State, Pos, Bytes=[Head|Rest]) ->
    TotalSize = size(Head),
    RemainingBytes = TotalSize - Pos,
    if Pos >= TotalSize ->
            reading_fun(State, 0, Rest);
       true ->
            fun(ByteCount) ->
                    Window = binary:part(Head, Pos, min(RemainingBytes, ByteCount)),
                    ReadCount = size(Window),
                    {ok, Window, reading_fun(State, Pos + ReadCount, Bytes)}
            end
    end.

get_module_state(State) ->
    State#connection_state.module_state.

get_fs(ModState) ->
    ModState#msrv_state.fs.

split_directory(DirString) ->
    string:tokens(DirString, "/").

absolute_path(State, Directory=[FirstChar | _]) ->
    Path = case FirstChar of
               $/ ->
                   [[]] ++ split_directory(Directory);
               _ ->
                   current_directory_list(State) ++ split_directory(Directory)
           end,
    resolve_path(State, Path).

resolve_path(State, Path) ->
    resolve_path(State, Path, []).

resolve_path(_, [], []) ->
    [[]]; % back to the root
resolve_path(_, [], R) ->
    R;
resolve_path(State, [H|T], R) ->
    case H of
        "." ->
            resolve_path(State, T, R);
        ".." ->
            % drop the last element of R
            [_|Rem] = lists:reverse(R),
            resolve_path(State, T, lists:reverse(Rem));
        P ->
            resolve_path(State, T, R ++ [P])
    end.

set_module_state(State, ModState) ->
    State#connection_state{module_state=ModState}.

set_state_fs(State, Fs) ->
    ModState = get_module_state(State),
    NewModState = ModState#msrv_state{fs=Fs},
    set_module_state(State, NewModState).

current_directory_list(State) ->
    ModState = State#connection_state.module_state,
    ModState#msrv_state.current_dir.

initialize_state(State) ->
    State#connection_state{module_state=#msrv_state{current_dir=[[]]}}.

fetch_parent(Root, [[]]) ->
    Root;
fetch_parent(Root, Path) ->
    [_ | T] = lists:reverse(Path),
    fetch_path(Root, lists:reverse(T)).

fetch_path(F, []) ->
    F;
fetch_path(F, [[] | T]) ->
    fetch_path(F, T);
fetch_path({file, _, _}, [_ | _]) ->
    not_found;
fetch_path({_, Root, _}, [Current]) ->
    case dict:is_key(Current, Root) of
        true ->
            dict:fetch(Current, Root);
        _ ->
            not_found
    end;
fetch_path({dir, Root, _}, [Current | Rest]) ->
    case dict:is_key(Current, Root) of
        true ->
            fetch_path(dict:fetch(Current, Root), Rest);
        _ ->
            not_found
    end.

new_file_info(Name, Type, Size) ->
    #file_info{name=Name,
               mtime=erlang:localtime(),
               type=Type,
               mode=511, % 0777
               gid=0,
               uid=0,
               size=Size}.

new_directory(Name) ->
    {dir, dict:new(), new_file_info(Name, dir, 0)}.

set_path(F, [[]|T], V) ->
    set_path(F, T, V);
set_path({dir, Root, FileInfo}, [Current], Val) ->
    case Val of
        remove ->
            {dir, dict:erase(Current, Root), FileInfo};
        _ ->
            {dir, dict:store(Current, Val, Root), FileInfo}
    end;
set_path({dir, Root, FileInfo}, [Current | Rest], Val) ->
    case dict:is_key(Current, Root) of
        true ->
            {dir,
             dict:store(Current,
                        set_path(dict:fetch(Current, Root), Rest, Val),
                        Root),
            FileInfo};
        _ ->
            {dir,
             dict:store(Current,
                        set_path(new_directory(Current), Rest, Val),
                        Root),
            FileInfo}
    end.
