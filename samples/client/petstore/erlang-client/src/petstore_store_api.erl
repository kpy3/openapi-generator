-module(petstore_store_api).

-export([delete_order/1, delete_order/2,
         get_inventory/0, get_inventory/1,
         get_order_by_id/1, get_order_by_id/2,
         place_order/1, place_order/2]).

-define(BASE_URL, <<"/v2">>).

%% @doc Delete purchase order by ID
%% For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
-spec delete_order(binary()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_order(OrderId) ->
    delete_order(OrderId, #{}).

-spec delete_order(binary(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_order(OrderId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = delete,
    Path = [?BASE_URL, "/store/order/", OrderId, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Returns pet inventories by status
%% Returns a map of status codes to quantities
-spec get_inventory() -> {ok, maps:map(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_inventory() ->
    get_inventory(#{}).

-spec get_inventory(maps:map()) -> {ok, maps:map(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_inventory(Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = get,
    Path = [?BASE_URL, "/store/inventory"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Find purchase order by ID
%% For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
-spec get_order_by_id(integer()) -> {ok, petstore_order:petstore_order(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_order_by_id(OrderId) ->
    get_order_by_id(OrderId, #{}).

-spec get_order_by_id(integer(), maps:map()) -> {ok, petstore_order:petstore_order(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_order_by_id(OrderId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = get,
    Path = [?BASE_URL, "/store/order/", OrderId, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Place an order for a pet
-spec place_order(petstore_order:petstore_order()) -> {ok, petstore_order:petstore_order(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
place_order(PetstoreOrder) ->
    place_order(PetstoreOrder, #{}).

-spec place_order(petstore_order:petstore_order(), maps:map()) -> {ok, petstore_order:petstore_order(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
place_order(PetstoreOrder, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = post,
    Path = [?BASE_URL, "/store/order"],
    QS = [],
    Headers = [],
    Body1 = PetstoreOrder,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


