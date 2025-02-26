-module(petstore_pet_api).

-export([add_pet/1, add_pet/2,
         delete_pet/1, delete_pet/2,
         find_pets_by_status/1, find_pets_by_status/2,
         find_pets_by_tags/1, find_pets_by_tags/2,
         get_pet_by_id/1, get_pet_by_id/2,
         update_pet/1, update_pet/2,
         update_pet_with_form/1, update_pet_with_form/2,
         upload_file/1, upload_file/2]).

-define(BASE_URL, <<"/v2">>).

%% @doc Add a new pet to the store
-spec add_pet(petstore_pet:petstore_pet()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
add_pet(PetstorePet) ->
    add_pet(PetstorePet, #{}).

-spec add_pet(petstore_pet:petstore_pet(), maps:map()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
add_pet(PetstorePet, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = post,
    Path = [?BASE_URL, "/pet"],
    QS = [],
    Headers = [],
    Body1 = PetstorePet,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>, <<"application/xml">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Deletes a pet
-spec delete_pet(integer()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_pet(PetId) ->
    delete_pet(PetId, #{}).

-spec delete_pet(integer(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
delete_pet(PetId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = delete,
    Path = [?BASE_URL, "/pet/", PetId, ""],
    QS = [],
    Headers = []++petstore_utils:optional_params(['api_key'], _OptionalParams),
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Finds Pets by status
%% Multiple status values can be provided with comma separated strings
-spec find_pets_by_status(list()) -> {ok, [petstore_pet:petstore_pet()], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
find_pets_by_status(Status) ->
    find_pets_by_status(Status, #{}).

-spec find_pets_by_status(list(), maps:map()) -> {ok, [petstore_pet:petstore_pet()], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
find_pets_by_status(Status, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = get,
    Path = [?BASE_URL, "/pet/findByStatus"],
    QS = lists:flatten([[{<<"status">>, X} || X <- Status]])++petstore_utils:optional_params([], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Finds Pets by tags
%% Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-spec find_pets_by_tags(list()) -> {ok, [petstore_pet:petstore_pet()], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
find_pets_by_tags(Tags) ->
    find_pets_by_tags(Tags, #{}).

-spec find_pets_by_tags(list(), maps:map()) -> {ok, [petstore_pet:petstore_pet()], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
find_pets_by_tags(Tags, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = get,
    Path = [?BASE_URL, "/pet/findByTags"],
    QS = lists:flatten([[{<<"tags">>, X} || X <- Tags]])++petstore_utils:optional_params([], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Find pet by ID
%% Returns a single pet
-spec get_pet_by_id(integer()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_pet_by_id(PetId) ->
    get_pet_by_id(PetId, #{}).

-spec get_pet_by_id(integer(), maps:map()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
get_pet_by_id(PetId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = get,
    Path = [?BASE_URL, "/pet/", PetId, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = petstore_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Update an existing pet
-spec update_pet(petstore_pet:petstore_pet()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_pet(PetstorePet) ->
    update_pet(PetstorePet, #{}).

-spec update_pet(petstore_pet:petstore_pet(), maps:map()) -> {ok, petstore_pet:petstore_pet(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_pet(PetstorePet, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = put,
    Path = [?BASE_URL, "/pet"],
    QS = [],
    Headers = [],
    Body1 = PetstorePet,
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/json">>, <<"application/xml">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Updates a pet in the store with form data
-spec update_pet_with_form(integer()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_pet_with_form(PetId) ->
    update_pet_with_form(PetId, #{}).

-spec update_pet_with_form(integer(), maps:map()) -> {ok, [], petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
update_pet_with_form(PetId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = post,
    Path = [?BASE_URL, "/pet/", PetId, ""],
    QS = [],
    Headers = [],
    Body1 = {form, []++petstore_utils:optional_params(['name', 'status'], _OptionalParams)},
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"application/x-www-form-urlencoded">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc uploads an image
-spec upload_file(integer()) -> {ok, petstore_api_response:petstore_api_response(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
upload_file(PetId) ->
    upload_file(PetId, #{}).

-spec upload_file(integer(), maps:map()) -> {ok, petstore_api_response:petstore_api_response(), petstore_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), petstore_utils:response_info()}.
upload_file(PetId, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(petstore_api, config, #{})),

    Method = post,
    Path = [?BASE_URL, "/pet/", PetId, "/uploadImage"],
    QS = [],
    Headers = [],
    Body1 = {form, []++petstore_utils:optional_params(['additionalMetadata', 'file'], _OptionalParams)},
    ContentTypeHeader = petstore_utils:select_header_content_type([<<"multipart/form-data">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    petstore_utils:request(Method, Path, QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


