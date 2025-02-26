-module(petstore_user_api).

-export([create_user/1, create_user/2,
         create_users_with_array_input/1, create_users_with_array_input/2,
         create_users_with_list_input/1, create_users_with_list_input/2,
         delete_user/1, delete_user/2,
         get_user_by_name/1, get_user_by_name/2,
         login_user/2, login_user/3,
         logout_user/0, logout_user/1,
         update_user/2, update_user/3]).

-define(BASE_URL, <<"/v2">>).

%% @doc Create user
%% This can only be done by the logged in user.
-spec create_user(petstore_user:petstore_user()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
create_user(PetstoreUser) ->
    create_user(PetstoreUser, #{}).

-spec create_user(petstore_user:petstore_user(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
create_user(PetstoreUser, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = post,
    Path = [?BASE_URL, "/user"],
    QS = [],
    Headers = [],
    Body1 = PetstoreUser,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Creates list of users with given input array
-spec create_users_with_array_input(list()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
create_users_with_array_input(PetstoreUserArray) ->
    create_users_with_array_input(PetstoreUserArray, #{}).

-spec create_users_with_array_input(list(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
create_users_with_array_input(PetstoreUserArray, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = post,
    Path = [?BASE_URL, "/user/createWithArray"],
    QS = [],
    Headers = [],
    Body1 = PetstoreUserArray,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Creates list of users with given input array
-spec create_users_with_list_input(list()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
create_users_with_list_input(PetstoreUserArray) ->
    create_users_with_list_input(PetstoreUserArray, #{}).

-spec create_users_with_list_input(list(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
create_users_with_list_input(PetstoreUserArray, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = post,
    Path = [?BASE_URL, "/user/createWithList"],
    QS = [],
    Headers = [],
    Body1 = PetstoreUserArray,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Delete user
%% This can only be done by the logged in user.
-spec delete_user(binary()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_user(Username) ->
    delete_user(Username, #{}).

-spec delete_user(binary(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_user(Username, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = delete,
    Path = [?BASE_URL, "/user/", Username, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Get user by user name
-spec get_user_by_name(binary()) -> {ok, petstore_user:petstore_user(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_user_by_name(Username) ->
    get_user_by_name(Username, #{}).

-spec get_user_by_name(binary(), maps:map()) -> {ok, petstore_user:petstore_user(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_user_by_name(Username, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = get,
    Path = [?BASE_URL, "/user/", Username, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Logs user into the system
-spec login_user(binary()binary()) -> {ok, binary(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
login_user(UsernamePassword) ->
    login_user(UsernamePassword, #{}).

-spec login_user(binary()binary(), maps:map()) -> {ok, binary(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
login_user(UsernamePassword, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = get,
    Path = [?BASE_URL, "/user/login"],
    QS = lists:flatten([{<<"username">>, Username}, {<<"password">>, Password}])++petstore_utils:optional_params([], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Logs out current logged in user session
-spec logout_user() -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
logout_user() ->
    logout_user(, #{}).

-spec logout_user(, maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
logout_user(, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = get,
    Path = [?BASE_URL, "/user/logout"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Updated user
%% This can only be done by the logged in user.
-spec update_user(binary()petstore_user:petstore_user()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_user(UsernamePetstoreUser) ->
    update_user(UsernamePetstoreUser, #{}).

-spec update_user(binary()petstore_user:petstore_user(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_user(UsernamePetstoreUser, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = put,
    Path = [?BASE_URL, "/user/", Username, ""],
    QS = [],
    Headers = [],
    Body1 = PetstoreUser,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


