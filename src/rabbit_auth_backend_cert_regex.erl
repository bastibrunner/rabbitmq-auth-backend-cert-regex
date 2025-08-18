-module(rabbit_auth_backend_cert_regex).
-behaviour(rabbit_authz_backend).

-include_lib("rabbit_common/include/rabbit.hrl").

-export([description/0,
         user_login_authorization/1,
         user_login_authorization/2,
         check_vhost_access/3,
         check_resource_access/4,
         check_topic_access/4,
         state_can_expire/0]).

%%--------------------------------------------------------------------
%% Description
description() ->
    [{name, <<"CertRegex">>},
     {description, <<"Authorization via regex matching of client certificate DN">>}].

%%--------------------------------------------------------------------
%% Login - skip internal DB
user_login_authorization(_) -> {ok, none}.
user_login_authorization(_, _) -> {ok, none}.

%%--------------------------------------------------------------------
%% VHost Access Check
check_vhost_access(_User, VHost, AuthzData) ->
    DN = extract_username(AuthzData),
    Rules = get_rules(),
    case match_rules(DN, Rules, vhost, VHost) of
        true -> true;
        false ->
            rabbit_log:warning("VHost access denied for ~s to ~s~n", [DN, VHost]),
            false
    end.

%%--------------------------------------------------------------------
%% Resource Access Check
check_resource_access(_User, #resource{virtual_host = _VHost, name = Name, kind = Kind}, Permission, AuthzData) ->
    DN = extract_username(AuthzData),
    Rules = get_rules(),
    case match_rules(DN, Rules, Permission, Name) of
        true -> true;
        false ->
            rabbit_log:warning("Resource access denied for ~s: ~s ~s ~s~n", [DN, Kind, Permission, Name]),
            false
    end.

%%--------------------------------------------------------------------
%% Topic Access Check
check_topic_access(_User, #resource{virtual_host = _VHost, name = Name}, Permission, AuthzData) ->
    DN = extract_username(AuthzData),
    Rules = get_rules(),
    case match_rules(DN, Rules, Permission, Name) of
        true -> true;
        false ->
            rabbit_log:warning("Topic access denied for ~s: ~s ~s~n", [DN, Permission, Name]),
            false
    end.

%%--------------------------------------------------------------------
%% State expiration (none)
state_can_expire() -> false.

%%--------------------------------------------------------------------
%% Helpers

get_rules() ->
    {ok, Rules} = application:get_env(rabbitmq_auth_backend_cert_regex, rules),
    Rules.

extract_username(undefined) -> <<"">>;
extract_username(unknown) -> <<"">>;
extract_username(#{ssl_cert := Cert}) ->
    %% Extract DN from the client cert info
    %% RabbitMQ puts this into peer_cert, not peername
    case lists:keyfind(subject, 1, Cert) of
        {subject, SubjectList} ->
            format_dn(SubjectList);
        _ ->
            <<"">>
    end;
extract_username(_) -> <<"">>.

format_dn(DNList) ->
    %% Convert subject tuple list to flat string, e.g., [{CN,"someuser"}] => "CN=someuser"
    String = string:join([io_lib:format("~s=~s", [atom_to_list(K), V]) || {K, V} <- DNList], ","),
    list_to_binary(lists:flatten(String)).

match_rules(_, [], _, _) -> false;
match_rules(DN, [{RegexStr, Perms} | Rest], What, Target) ->
    case re:run(binary_to_list(DN), RegexStr, [{capture, none}]) of
        match ->
            case proplists:get_value(What, Perms) of
                undefined -> match_rules(DN, Rest, What, Target);
                Pattern ->
                    case re:run(Target, Pattern, [{capture, none}]) of
                        match -> true;
                        nomatch -> match_rules(DN, Rest, What, Target)
                    end
            end;
        nomatch ->
            match_rules(DN, Rest, What, Target)
    end.
