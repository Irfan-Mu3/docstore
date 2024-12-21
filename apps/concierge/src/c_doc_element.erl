-module(c_doc_element).
-behavior(gen_server).

-export([start_link/1, create/1, fetch/1, replace/2, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {doc}).

%API
create(Document) ->
  c_doc_element_sup:start_child(Document).

fetch(Pid) ->
  gen_server:call(Pid, fetch).

replace(Pid, Document) ->
  gen_server:cast(Pid, {replace, Document}).

delete(Pid) ->
  gen_server:cast(Pid, delete).

%Callbacks
start_link(Document) ->
  gen_server:start_link(?MODULE, [Document], []).

init([Document]) ->
  {ok, #state{doc = Document}}.

handle_call(fetch, _From, State) ->
  #state{doc = Document} = State,
  {reply, {ok, Document}, State}.

handle_cast({replace, Document}, State) ->
  {noreply, State#state{doc = Document}};
handle_cast(delete, State) ->
  {stop, normal, State}. % calls terminate below

handle_info(timeout, State) ->
  {stop, normal, State}. % calls terminate below

terminate(_Reason, _State) ->
  c_store:delete(self()),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



