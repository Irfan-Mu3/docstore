-module(l_token).

-behavior(gen_server).

-export([start_link/0, create/0, update/2, fetch/1, delete/1, remove_doc/2]).
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

remove_doc(Pid, Title) ->
  gen_server:cast(Pid, {remove, Title}).

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
    UpdatedCatalogue = update_document_catalogue(Title, LineNum, Idx, DocCatalogue),
    {noreply, State#state{doc_catalogue = UpdatedCatalogue}};

handle_cast({remove, Title}, #state{doc_catalogue = DocCatalogue} = State) ->
    {noreply, State#state{doc_catalogue = maps:remove(Title, DocCatalogue)}};

handle_cast(delete, State) ->
    {stop, normal, State}. %calls terminate below

update_document_catalogue(Title, LineNum, Idx, DocCatalogue) ->
    maps:update_with(Title,
        fun(Doc) -> update_document(Doc, LineNum, Idx) end,
        #document{count = 1, locations = #{LineNum => [Idx]}},
        DocCatalogue).

update_document(#document{locations = Locations, count = Count} = Doc, LineNum, Idx) ->
    ExistingIndices = maps:get(LineNum, Locations, []),
    NewIndices = [Idx | lists:delete(Idx, ExistingIndices)],
    NewLocations = maps:put(LineNum, NewIndices, Locations),
    NewCount = Count + (length(NewIndices) - length(ExistingIndices)),
    Doc#document{locations = NewLocations, count = NewCount}.

handle_info(timeout, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  l_store:delete(token, self()),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
