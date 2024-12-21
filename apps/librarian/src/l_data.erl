-module(l_data).

%Callbacks
-export([test_token/0, test_document/0]).

test_document() ->
  {"Hello Document",
   "Hello World!\n    All the oceans are blue, and so is the sky!\n "
   "   The sun is shining and so do your eyes!\n    The end."}.

test_token() ->
    {"Hello Document", "oceans", 2, 3}.