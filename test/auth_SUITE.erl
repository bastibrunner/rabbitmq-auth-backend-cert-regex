%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(auth_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

all() -> [
          cert_dn_denied,
          cert_dn_allowed,
          cert_dn_vhost_access,
          cert_dn_resource_access,
          cert_dn_topic_access
         ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(Config, [{rmq_nodename_suffix, ?MODULE}]),
    AuthConf =
        [{rabbit, [
             {auth_backends, [
                 {rabbit_auth_backend_internal, [rabbit_auth_backend_internal,
                                                 rabbit_auth_backend_cert_regex]}
             ]}
        ]},
        {rabbitmq_auth_backend_cert_regex, [
            {rules, [
                {".*CN=admin.*", [
                    {vhost, ".*"},
                    {configure, ".*"},
                    {write, ".*"},
                    {read, ".*"}
                ]},
                {".*CN=user1.*", [
                    {vhost, "test_vhost"},
                    {configure, "test_queue"},
                    {write, "test_queue"},
                    {read, "test_queue"}
                ]},
                {".*CN=user2.*", [
                    {vhost, "restricted_vhost"},
                    {configure, "restricted_queue"},
                    {write, "restricted_queue"},
                    {read, "restricted_queue"}
                ]}
            ]}
        ]}
    ],
    Config2 = rabbit_ct_helpers:merge_app_env(Config1, AuthConf),
    Steps = rabbit_ct_broker_helpers:setup_steps() ++
            rabbit_ct_client_helpers:setup_steps(),
    Config3 = rabbit_ct_helpers:run_setup_steps(Config2, Steps),
    setup_test_environment(Config3),
    Config3.

setup_test_environment(Config) ->
    %% Create test vhosts
    ok = rabbit_ct_broker_helpers:add_vhost(Config, 0, <<"test_vhost">>),
    ok = rabbit_ct_broker_helpers:add_vhost(Config, 0, <<"restricted_vhost">>),
    
    %% Create test queues
    ok = rabbit_ct_broker_helpers:declare_queue(Config, 0, <<"test_queue">>, <<"test_vhost">>),
    ok = rabbit_ct_broker_helpers:declare_queue(Config, 0, <<"restricted_queue">>, <<"restricted_vhost">>).

end_per_suite(Config) ->
    Steps = rabbit_ct_client_helpers:teardown_steps() ++
            rabbit_ct_broker_helpers:teardown_steps(),
    rabbit_ct_helpers:run_teardown_steps(Config, Steps).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% Test cases for cert_regex plugin

cert_dn_denied(Config) ->
    %% Test with a DN that doesn't match any rules
    AuthzData = #{ssl_cert => [{subject, [{cn, "unknown_user"}]}]},
    User = <<"guest">>,
    VHost = <<"/">>,
    
    %% Should be denied access
    false = rabbit_auth_backend_cert_regex:check_vhost_access(User, VHost, AuthzData).

cert_dn_allowed(Config) ->
    %% Test with a DN that matches admin rule
    AuthzData = #{ssl_cert => [{subject, [{cn, "admin"}]}]},
    User = <<"guest">>,
    VHost = <<"/">>,
    
    %% Should be allowed access
    true = rabbit_auth_backend_cert_regex:check_vhost_access(User, VHost, AuthzData).

cert_dn_vhost_access(Config) ->
    %% Test vhost access with specific DN
    AuthzData = #{ssl_cert => [{subject, [{cn, "user1"}]}]},
    User = <<"guest">>,
    
    %% Should have access to test_vhost
    true = rabbit_auth_backend_cert_regex:check_vhost_access(User, <<"test_vhost">>, AuthzData),
    
    %% Should not have access to restricted_vhost
    false = rabbit_auth_backend_cert_regex:check_vhost_access(User, <<"restricted_vhost">>, AuthzData).

cert_dn_resource_access(Config) ->
    %% Test resource access with specific DN
    AuthzData = #{ssl_cert => [{subject, [{cn, "user1"}]}]},
    User = <<"guest">>,
    Resource = #resource{virtual_host = <<"test_vhost">>, name = <<"test_queue">>, kind = queue},
    
    %% Should have configure, write, read access to test_queue
    true = rabbit_auth_backend_cert_regex:check_resource_access(User, Resource, configure, AuthzData),
    true = rabbit_auth_backend_cert_regex:check_resource_access(User, Resource, write, AuthzData),
    true = rabbit_auth_backend_cert_regex:check_resource_access(User, Resource, read, AuthzData),
    
    %% Should not have access to restricted_queue
    Resource2 = #resource{virtual_host = <<"restricted_vhost">>, name = <<"restricted_queue">>, kind = queue},
    false = rabbit_auth_backend_cert_regex:check_resource_access(User, Resource2, configure, AuthzData).

cert_dn_topic_access(Config) ->
    %% Test topic access with specific DN
    AuthzData = #{ssl_cert => [{subject, [{cn, "user2"}]}]},
    User = <<"guest">>,
    Resource = #resource{virtual_host = <<"restricted_vhost">>, name = <<"restricted_queue">>},
    
    %% Should have access to restricted_queue topic
    true = rabbit_auth_backend_cert_regex:check_topic_access(User, Resource, write, AuthzData),
    true = rabbit_auth_backend_cert_regex:check_topic_access(User, Resource, read, AuthzData),
    
    %% Should not have access to test_queue topic
    Resource2 = #resource{virtual_host = <<"test_vhost">>, name = <<"test_queue">>},
    false = rabbit_auth_backend_cert_regex:check_topic_access(User, Resource2, write, AuthzData).
