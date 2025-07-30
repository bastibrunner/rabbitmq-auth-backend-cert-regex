-module(test_simple).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Define the resource record for testing
-record(resource, {virtual_host, name, kind}).

%% Simple test to verify the cert_regex plugin functionality
test_cert_regex() ->
    %% Mock the application environment
    application:set_env(rabbitmq_auth_backend_cert_regex, rules, [
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
    ]),
    
    %% Test DN extraction
    AuthzData1 = #{ssl_cert => [{subject, [{cn, "admin"}]}]},
    AuthzData2 = #{ssl_cert => [{subject, [{cn, "user1"}]}]},
    AuthzData3 = #{ssl_cert => [{subject, [{cn, "unknown"}]}]},
    
    %% Test DN formatting
    DN1 = rabbit_auth_backend_cert_regex:extract_username(AuthzData1),
    DN2 = rabbit_auth_backend_cert_regex:extract_username(AuthzData2),
    DN3 = rabbit_auth_backend_cert_regex:extract_username(AuthzData3),
    
    io:format("DN1: ~p~n", [DN1]),
    io:format("DN2: ~p~n", [DN2]),
    io:format("DN3: ~p~n", [DN3]),
    
    %% Test vhost access
    User = <<"guest">>,
    
    %% Admin should have access to any vhost
    true = rabbit_auth_backend_cert_regex:check_vhost_access(User, <<"/">>, AuthzData1),
    true = rabbit_auth_backend_cert_regex:check_vhost_access(User, <<"test_vhost">>, AuthzData1),
    true = rabbit_auth_backend_cert_regex:check_vhost_access(User, <<"restricted_vhost">>, AuthzData1),
    
    %% User1 should have access to test_vhost only
    true = rabbit_auth_backend_cert_regex:check_vhost_access(User, <<"test_vhost">>, AuthzData2),
    false = rabbit_auth_backend_cert_regex:check_vhost_access(User, <<"restricted_vhost">>, AuthzData2),
    
    %% Unknown user should be denied
    false = rabbit_auth_backend_cert_regex:check_vhost_access(User, <<"/">>, AuthzData3),
    
    %% Test resource access
    Resource1 = #resource{virtual_host = <<"test_vhost">>, name = <<"test_queue">>, kind = queue},
    Resource2 = #resource{virtual_host = <<"restricted_vhost">>, name = <<"restricted_queue">>, kind = queue},
    
    %% User1 should have access to test_queue
    true = rabbit_auth_backend_cert_regex:check_resource_access(User, Resource1, configure, AuthzData2),
    true = rabbit_auth_backend_cert_regex:check_resource_access(User, Resource1, write, AuthzData2),
    true = rabbit_auth_backend_cert_regex:check_resource_access(User, Resource1, read, AuthzData2),
    
    %% User1 should not have access to restricted_queue
    false = rabbit_auth_backend_cert_regex:check_resource_access(User, Resource2, configure, AuthzData2),
    
    %% Test topic access
    true = rabbit_auth_backend_cert_regex:check_topic_access(User, Resource1, write, AuthzData2),
    true = rabbit_auth_backend_cert_regex:check_topic_access(User, Resource1, read, AuthzData2),
    false = rabbit_auth_backend_cert_regex:check_topic_access(User, Resource2, write, AuthzData2),
    
    io:format("All tests passed!~n"),
    ok.

run() ->
    test_cert_regex(). 