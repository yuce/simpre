% Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
% All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:

% * Redistributions of source code must retain the above copyright
%   notice, this list of conditions and the following disclaimer.

% * Redistributions in binary form must reproduce the above copyright
%   notice, this list of conditions and the following disclaimer in the
%   documentation and/or other materials provided with the distribution.

% * The names of its contributors may not be used to endorse or promote
%   products derived from this software without specific prior written
%   permission.

% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(simpre).
-behaviour(gen_server).

-export([start_link/1,
         update/2,
         remove/1,
         pid/1,
         stop/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% == API

start_link(RegName) when is_atom(RegName) ->
    gen_server:start_link({local, RegName}, ?MODULE, [], []);

start_link(RegName) ->
    gen_server:start_link(RegName, ?MODULE, [], []).

update({Name, Ref}, Pid) ->
    gen_server:cast(Name, {update, {Name, Ref}, Pid}).

remove({Name, Ref}) ->
    gen_server:cast(Name, {remove, {Name, Ref}}).

pid({Name, Ref}) ->
    gen_server:call(Name, {pid, {Name, Ref}}).

stop(Name) ->
    gen_server:stop(Name).

%% == Callbacks

init([]) ->
    Ets = ets:new(?MODULE, []),
    {ok, Ets}.

handle_call({pid, Key}, _From, Ets) ->
    Reply = case ets:lookup(Ets, Key) of
        [] -> not_found;
        [{Key, Pid}] -> {ok, Pid}
    end,
    {reply, Reply, Ets}.

handle_cast({update, Key, Pid}, Ets) ->
    true = ets:insert(Ets, {Key, Pid}),
    {noreply, Ets};

handle_cast({remove, Key}, Ets) ->
    true = ets:delete(Ets, Key),
    {noreply, Ets}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

proc() ->
    F = fun() ->
        receive
            after 1000 ->
                ok
        end
    end,
    spawn(F).

insert_test() ->
    _Simpre = start_link({local, simpre}),
    Key = {simpre, make_ref()},
    Pid = proc(),

    update(Key, Pid),
    {ok, R1} = pid(Key),
    ?assertEqual(Pid, R1),

    remove(Key),
    R2 = pid(Key),
    ?assertEqual(not_found, R2).

-endif.