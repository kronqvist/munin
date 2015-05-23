-module(munin).

-include("munin.hrl").
-export([attributes/1, binaries/1, filter/2, leafs/1, links/1
       , map/3, name/1, paths/2, prune/2, select/2, tree/1]).

-type attribute() :: [ {binary(), binary()} ].
-type tree()      :: {binary(), [ attribute() ], [ tree() ]}
                   | {comment, binary()}
                   | binary().
-type failure()   :: search_failed.

%%==============================================================================
%% API
%%==============================================================================

%% -----------------------------------------------------------------------------
%% attributes: Return the attributes of a tree.
%%
%% example: attributes(tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec attributes(Tree :: tree()) -> [ attribute() ].

attributes({_, Attributes, _}) ->
  Attributes.


%% -----------------------------------------------------------------------------
%% binaries: Return all binaries in a tree.
%%
%% example: binaries(tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec binaries(Tree :: tree()) -> [ binary() ].

binaries([ ]) ->
  [ ];

binaries([ B | Leafs ]) when is_binary(B) ->
  [ B | binaries(Leafs) ];

binaries([ {comment, _} | Leafs ]) ->
  binaries(Leafs);

binaries([ Tree | Leafs ]) ->
  lists:flatmap( fun(T) -> binaries(T) end, leafs(Tree)) ++ binaries(Leafs);

binaries({comment, _}) ->
  [];

binaries(Bin) when is_binary(Bin) ->
  [Bin];

binaries(Tree) when is_tuple(Tree) ->
  lists:flatmap( fun(T) -> binaries(T) end, leafs(Tree)).


%% -----------------------------------------------------------------------------
%% filter:  Filter a tree by following a path. A path is a space-separated list
%%          of "tokens" where each token is an HTML tag name (e.g. 'div') with
%%          an optional class or id. A class is denoted with div.my_class and
%%          an id is denoted with div#my_id.
%%
%%          Note that the path is not necessarily unique. This function will
%%          follow the first matching path and return the leafs of the last
%%          match.
%%
%% example: filter("html body div.Content span", tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec filter(Path :: string(), Tree :: tree() | [ tree() ]) -> [ tree() ] | failure().

filter(Path, Tree) when is_tuple(Tree) orelse is_list(Tree) ->
  Tree1 = select(Path, Tree),

  case Tree1 == fail() of
    true -> fail();
    false -> leafs(Tree1)
  end.


%% -----------------------------------------------------------------------------
%% name: Return the leafs of a tree.
%%
%% example: name(tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec leafs(Tree :: tree()) -> [ tree() ].

leafs({_, _, Leafs}) ->
  Leafs.


%% -----------------------------------------------------------------------------
%% links: Return all links in a tree.
%%
%% example: name(tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec links(Tree :: tree()) -> [ #link{} ].

links(Tree) ->
  links(Tree, [ ]).

links(_Leaf, _) when is_binary(_Leaf) ->
  [ ];

links({comment, _}, _) ->
  [ ];

links(Tree, Path) ->
  Path1 = [ name(Tree) | Path ],
  Link =
    [ begin
       #link{
        path  = lists:reverse(Path),
        href  = proplists:get_value(<<"href">>, attributes(Tree), <<"">>),
        value = leafs(Tree) }
      end
      || name(Tree) == <<"a">> ],

  Link ++
    lists:flatmap( fun(Tree1) -> links(Tree1, Path1) end, leafs(Tree)).


%% -----------------------------------------------------------------------------
%% map: Maps over all leafs that matches Path.
%%
%% example: map(fun(Tree) -> name(Tree) end, "html body table tr",
%%              tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec map(Fun :: fun( (T :: tree()) -> term() ), Path :: string(), Tree :: tree() | [ tree() ]) -> [ any() ].

map(Fun, Path, Tree) when is_tuple(Tree) ->
  map1(Fun, Path, Tree);

map(Fun, Path, Leafs) when is_list(Leafs) ->
  map1(Fun, Path, Leafs).

map1(Fun, Path, Tree) ->
  [ Token | RTokens ] = lists:reverse(string:tokens(Path, " ")),
  Leafs = filter(string:join(lists:reverse(RTokens), " "), Tree),

  case Leafs == fail() of
    true -> fail();
    false ->
      [ Fun(Tree0) || Tree0 <- Leafs, match(list_to_binary(Token), Tree0) ]
  end.


%% -----------------------------------------------------------------------------
%% name: Return the name of a tree.
%%
%% example: name(tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec name(Tree :: tree()) -> binary().

name({Name, _, _}) ->
  Name.


%% -----------------------------------------------------------------------------
%% paths: Return all paths to where Regexp matches.
%%
%% example: paths("[Aa]udi", tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec paths(Regexp :: string(), Tree :: tree() | [ tree() ]) -> [ matching_result() ].
-type matching_result() :: {Match :: binary(), Path :: [ binary() ]}.

paths(Regexp, Tree) ->
  paths1(Regexp, Tree, [ ]).

paths1(_Regexp, [ ], _Path) ->
  [ ];

paths1(Regexp, [ B | Leafs ], Path) when is_binary(B) ->
  [ {B, lists:reverse(Path)} || re:run(B, Regexp, [{capture, none}]) == match ]
    ++ paths1(Regexp, Leafs, Path);

paths1(Regexp, [ {comment, _} | Leafs ], Path) ->
  paths1(Regexp, Leafs, Path);

paths1(Regexp, [ Tree | Leafs ], Path) ->
  paths1(Regexp, leafs(Tree), [ path_serial(Tree) | Path ]) ++ paths1(Regexp, Leafs, Path);

paths1(Regexp, Tree, Path) when is_tuple(Tree) ->
  paths1(Regexp, leafs(Tree), [ path_serial(Tree) | Path ]).

path_serial(Tree) ->
  Serial = proplists:get_value(<<"serial">>, attributes(Tree)),
  list_to_binary(
    binary_to_list(name(Tree)) ++ ":" ++ binary_to_list(Serial)).


%% -----------------------------------------------------------------------------
%% prune: Remove nodes in the input list.
%%
%% example: prune("div.not_interesting span", tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec prune(RemoveList :: string(), Tree :: tree()) -> tree().

prune(String, Tree) when is_tuple(Tree) ->
  prune1(tokenize(String), Tree).

prune1(_Tokens, [ ]) ->
  [ ];

prune1(Tokens, [ B | Leafs ]) when is_binary(B) ->
  [ B | prune1(Tokens, Leafs) ];

prune1(Tokens, [ C = {comment, _} | Leafs ]) ->
  [ C | prune1(Tokens, Leafs) ];

prune1(Tokens, [ Tree | Leafs ]) ->
  case lists:any(fun(T) -> match(T, Tree) end, Tokens) of
    true ->
      prune1(Tokens, Leafs);
    false ->
      [ {name(Tree), attributes(Tree), prune1(Tokens, leafs(Tree))}
      | prune1(Tokens, Leafs) ]
  end;

prune1(Tokens, Tree) when is_tuple(Tree) ->
  {name(Tree), attributes(Tree), prune1(Tokens, leafs(Tree))}.


%% -----------------------------------------------------------------------------
%% select:  Select a tree by following a path. A path is a space-separated list
%%          of "tokens" where each token is an HTML tag name (e.g. 'div') with
%%          an optional class or id. A class is denoted with div.my_class and
%%          an id is denoted with div#my_id.
%%
%%          Note that the path is not necessarily unique. This function will
%%          follow the first matching path and return the first matching tree.
%%
%% example: select("html body div.Content span", tree("myfile.html"))
%% -----------------------------------------------------------------------------
-spec select(Path :: string(), Tree :: tree() | [ tree() ]) -> tree() | failure().

select(Path, Tree) when is_tuple(Tree) ->
  Tokens = tokenize(Path),

  case match(hd(Tokens), Tree) of
    true -> select1(tl(Tokens), leafs(Tree));
    false -> fail()
  end;

select(Path, Leafs) when is_list(Leafs) ->
  select1(tokenize(Path), Leafs).

select1([ ], Tree) ->
  Tree;

select1(_, [ ]) ->
  fail();

select1([ Token ], Leafs) ->
  case match(Token, hd(Leafs)) of
    true -> hd(Leafs);
    false -> select1([ Token ], tl(Leafs))
  end;

select1(Tokens, Leafs) ->
  case match(hd(Tokens), hd(Leafs)) of
    true -> select1(tl(Tokens), leafs(hd(Leafs)));
    false -> select1(Tokens, tl(Leafs))
  end.


%% -----------------------------------------------------------------------------
%% tree: Return an HTML tree from a file name.
%%
%% example: tree("myfile.html")
%% -----------------------------------------------------------------------------
-spec tree(Filename :: string()) -> tree().


tree(Filename) when is_list(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  tree(Bin);

tree(Bin) when is_binary(Bin) ->
  tree1(mochiweb_html:parse(Bin), 0).

tree1([ ], _I) ->
  [ ];

tree1([ B | Leafs ], I) when is_binary(B) ->
  [ B | tree1(Leafs, I) ];

tree1([ C = {comment, _} | Leafs ], I) ->
  [ C | tree1(Leafs, I) ];


tree1([ Tree | Leafs ], I) ->
  [ {name(Tree), [ tree_serial(I) | attributes(Tree) ], tree1(leafs(Tree), 0)}
  | tree1(Leafs, I + 1) ];

tree1(Tree, I) when is_tuple(Tree) ->
  {name(Tree), [ tree_serial(I) | attributes(Tree) ], tree1(leafs(Tree), 0)}.

tree_serial(I) ->
  {<<"serial">>, list_to_binary(integer_to_list(I))}.


%%==============================================================================
%% Internal Functions
%%==============================================================================

tokenize(String) ->
  [ tokenize1(Str) || Str <- string:tokens(String, " ") ].

tokenize1(PreToken) ->
  Contains    = fun(C) -> lists:any(fun(C1) -> C1 == C end, PreToken) end,
  Partition   =
    fun(C) ->
      {A, B} = lists:splitwith(
                 fun(C1) -> C1 /= C end, PreToken),
      {A, tl(B)}
    end,

  case {Contains($#), Contains($.), Contains($:)} of
    {true, false, false} ->
      {A, B} = Partition($#),
      {list_to_binary(A), {list_to_binary("id"), list_to_binary(B)}};
    {false, true, false} ->
      {A, B} = Partition($.),
      {list_to_binary(A), {list_to_binary("class"), list_to_binary(B)}};
    {false, false, true} ->
      {A, B} = Partition($:),
      {list_to_binary(A), {list_to_binary("serial"), list_to_binary(B)}};
    {false, false, false} ->
      list_to_binary(PreToken)
  end.

fail() ->
  search_failed.

match(Token, Tree) ->
  case catch match1(Token, Tree) of
    {'EXIT', _} -> false;
    Result -> Result
  end.

match1({Name, Filter}, Tree) ->
  Name == name(Tree) andalso
    (lists:member(Filter, attributes(Tree)) orelse
      begin
        {K1, V1} = Filter,
        lists:member(
          binary_to_list(V1),
          string:tokens(
            binary_to_list(
              proplists:get_value(K1, attributes(Tree), <<"">>)), " "))
      end);

match1(Name, Tree) ->
  Name == name(Tree).
