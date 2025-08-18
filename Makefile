PROJECT = rabbitmq_auth_backend_cert_regex
PROJECT_DESCRIPTION = RabbitMQ Certificate DN Regex Authentication Backend
# PROJECT_MOD = rabbit_auth_backend_cert_regex_app
RABBITMQ_VERSION ?= v3.13.x

current_rmq_ref = $(RABBITMQ_VERSION)
base_rmq_ref = master

define PROJECT_ENV
[
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
		]}
	]}
]
endef

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, ["3.13.0"]}
endef

DEPS = rabbit_common rabbit
TEST_DEPS = ct_helper rabbitmq_ct_helpers rabbitmq_ct_client_helpers amqp_client
dep_ct_helper = git https://github.com/extend/ct_helper.git master

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

dep_amqp_client                = git_rmq-subfolder rabbitmq-erlang-client $(RABBITMQ_VERSION)
dep_rabbit_common              = git_rmq-subfolder rabbitmq-common $(RABBITMQ_VERSION)
dep_rabbit                     = git_rmq-subfolder rabbitmq-server $(RABBITMQ_VERSION)
dep_rabbitmq_ct_client_helpers = git_rmq-subfolder rabbitmq-ct-client-helpers $(RABBITMQ_VERSION)
dep_rabbitmq_ct_helpers        = git_rmq-subfolder rabbitmq-ct-helpers $(RABBITMQ_VERSION)

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk