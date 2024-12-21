-module(l_token).

-behavior(gen_server).

-export([start_link/0, create/0, update/2, fetch/1, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {doc_catalogue}).
-record(document, {count, locations}).

%%% Catalogue => DocumentCatalogues/Tokens => {
%%%   Documents/DocID => {Count, DocID, Title, Indices}
%%% }

create() ->
  % Create Document Catalogue process
  l_token_sup:start_child().

update(Pid, {Title, LineNum, Idx}) ->
  % Update Document Catalogue with new entries (Books/Documents/Articles)
  gen_server:cast(Pid, {update, {Title, LineNum, Idx}}).

fetch(Pid) ->
  gen_server:call(Pid, fetch).

delete(Pid) ->
  gen_server:cast(Pid, delete).

%Callbacks
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, #state{doc_catalogue = maps:new()}}.

handle_call(fetch, _From, State) ->
  #state{doc_catalogue = DocCatalogue} = State,
  {reply, {ok, DocCatalogue}, State}.

handle_cast({update, {Title, LineNum, Idx}}, #state{doc_catalogue = DocCatalogue} = State) ->
  UpdateDocFunc = 
    fun(#document{locations = Locations, count = Count}) ->
      OldCount = length(maps:get(LineNum, Locations, [])),
       NewLocations = maps:update_with(
        LineNum, 
        fun(Indices) -> [Idx | lists:delete(Idx, Indices)] end, 
        [Idx],
        Locations
      ),
       NewCount = length(maps:get(LineNum, NewLocations)) - OldCount,
       #document{count = Count + NewCount, locations = NewLocations}
    end, 
 DefaultCatalog = #document{count = 1, locations = #{LineNum => [Idx]}},
 NewDocCatalogue = maps:update_with(Title,
                     UpdateDocFunc,
                     DefaultCatalog,
                     DocCatalogue),
  {noreply, State#state{doc_catalogue = NewDocCatalogue}};
handle_cast(delete, State) ->
  {stop, normal, State}. % calls terminate below

handle_info(timeout, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  l_store:delete(self()),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
