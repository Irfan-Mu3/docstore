-module(librarian).

-export([insert_token/4, lookup/1, delete/1, get_frequency_score/1]).
-export([get_search_results/1]).
-export([remove_doc/1]).
-export([list_tokens/0]).
-export([list_docs/0]).
-export([fetch_tokens/0]).
-export([cleanup/0]).
-export([fetch_documents/0]).

-record(document, {count, locations}).

insert_token(Token, Title, LineNum, Idx) ->
  case l_store:lookup(token, Token) of
    {ok, Pid} ->
      l_token:update(Pid, {Title, LineNum, Idx});
    {error, not_found} ->
      {ok, Pid} = l_token:create(),
      l_store:insert(token, Token, Pid),
      l_token:update(Pid, {Title, LineNum, Idx})
  end,
  l_store:insert(document, Title, Pid).

lookup(Token) ->
  try
    {ok, Pid} = l_store:lookup(token, Token),
    {ok, _Document} = l_token:fetch(Pid)
  catch
    _Class:_Exception ->
      {error, not_found}
  end.

fetch_tokens() ->
  TokensTups = list_tokens(),
  lists:map(fun({Token, Pid}) ->
               {ok, DocCatalog} = l_token:fetch(Pid),
               {Token, DocCatalog}
            end,
            TokensTups).

fetch_documents() ->
  Docs = list_docs(),
  lists:map(fun({Title, TokenPids}) ->
               Tokens = lists:map(fun(Pid) ->
                            {ok, DocCatalog} = l_token:fetch(Pid),
                            {ok, Token} = l_store:lookup_reverse(token, Pid),
                            {Token, DocCatalog}
                         end,
                         TokenPids),
                {Title, Tokens}
            end,
            Docs).

list_tokens() ->
  l_store:list_table(token).

list_docs() ->
  l_store:list_table(document).

delete(Token) ->
  case l_store:lookup(token, Token) of
    {ok, Pid} ->
      l_token:delete(Pid);
    {error, _Reason} ->
      ok
  end.

remove_doc(Title) ->
  case l_store:lookup(document, Title) of
    {ok, TokenPids} ->
      lists:foreach(fun(Pid) -> l_token:remove_doc(Pid, Title) end, TokenPids);
    {error, _Reason} ->
      ok
  end,
  l_store:delete(document, Title),
  ok.

update_fscore_helper(Title, Val, Acc) ->
  maps:update_with(Val#document.count,
                   fun(ExistingDocNames) -> [{Title, Val#document.locations} | ExistingDocNames]
                   end,
                   [{Title, Val#document.locations}],
                   Acc).

cleanup() ->
  TokensTups = list_tokens(),
  lists:foreach(fun({_Token, Pid}) ->
                   {ok, DocCatalog} = l_token:fetch(Pid),
                   case map_size(DocCatalog) of 0 -> l_token:delete(Pid) end
                end,
                TokensTups).

% Create a new map Count -> List of Books.
get_frequency_score(DocMap) ->
  FScoreMap = maps:fold(fun update_fscore_helper/3, #{}, DocMap),
  FScores =
    lists:reverse(
      lists:sort(
        maps:keys(FScoreMap))),
  {FScoreMap, FScores}.

% TODO: Get rid of duplicate tokens, or stop words, somewhere
get_search_results(Tokens) ->
  GetDocCatalogsFun =
    fun(Token, Acc) ->
       case librarian:lookup(Token) of
         {ok, DocCatalog} -> [DocCatalog | Acc];
         _ -> Acc
       end
    end,
  DocCatalogs = lists:foldl(GetDocCatalogsFun, [], Tokens),
  get_sorted_results(DocCatalogs).

get_sorted_results(DocCatalogs) ->
  DocMap = lists:foldl(fun merge_doc_maps/2, #{}, DocCatalogs),
  erlang:display(DocMap),
  get_frequency_score(DocMap).

merge_doc_maps(DocumentSet, Acc) ->
  % For each pair of DocCatalogs, if they keys are identical merge them
  MergeMapFunc =
    fun(_, DocA, DocB) ->
       #document{count = DocA#document.count + DocB#document.count,
                 locations =
                   maps:merge_with(fun(_, LocA, LocB) -> lists:append(LocA, LocB) end,
                                   DocA#document.locations,
                                   DocB#document.locations)}
    end,

  maps:merge_with(MergeMapFunc, DocumentSet, Acc).
