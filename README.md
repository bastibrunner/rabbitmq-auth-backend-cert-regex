# RabbitMQ plug-in for client authorization based on SSL certificate DN regex matching

[![Build Status](https://github.com/bastibrunn/rabbitmq-auth-backend-cert-regex/actions/workflows/main.yaml/badge.svg?branch=master)](https://github.com/bastibrunn/rabbitmq-auth-backend-cert-regex/actions)

## Supported RabbitMQ and Erlang Versions

This plugin currently targets RabbitMQ 3.9.x and Erlang 21.3+. Please see the [Releases](https://github.com/bastibrunn/rabbitmq-auth-backend-cert-regex/releases) page for binary downloads.

## Binary Builds

Binary builds are published as [GitHub releases](https://github.com/bastibrunn/rabbitmq-auth-backend-cert-regex/releases).

## Overview

This plugin provides authorization for RabbitMQ clients based on the Distinguished Name (DN) extracted from SSL certificates. It uses regex patterns to match certificate DNs and grants permissions based on configured rules.

## Configuration

This plugin uses both [RabbitMQ configuration files](http://www.rabbitmq.com/configure.html#configuration-file),
`rabbitmq.conf` and `advanced.config`.

### Basic Configuration

An example configuration file follows:

``` ini
auth_backends.1.authn = internal
auth_backends.1.authz = rabbit_auth_backend_cert_regex
```

```erlang
[
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
            ]}
        ]}
    ]}
].
```

### Using Certificate DN Regex as an AuthZ Backend

The `rabbit_auth_backend_cert_regex` should be used for authorization only. It may
be used with the `rabbit_auth_backend_internal`, `rabbit_auth_backend_ldap`, or other authentication backends.

For example:

``` ini
auth_backends.1.authn = internal
auth_backends.1.authz = rabbit_auth_backend_cert_regex

auth_backends.2.authz = internal
```

This will use the `internal` backend for authentication. `rabbit_auth_backend_cert_regex` will be tried
for authorization first, with a fallback to the standard `internal` database backend.

### Configuration Parameters

The plugin supports the following parameters configurable via `advanced.config`:

#### `rules`

A list of tuples `{RegexPattern, Permissions}` where:

- **RegexPattern**: A regex pattern to match against the certificate DN
- **Permissions**: A list of permission tuples defining access rights

#### Permission Types

The following permission types are supported:

- **vhost**: Virtual host access pattern
- **configure**: Resource configuration permission pattern
- **write**: Resource write permission pattern  
- **read**: Resource read permission pattern

#### Example Configuration

```erlang
{rabbitmq_auth_backend_cert_regex, [
    {rules, [
        %% Admin users with full access
        {".*CN=admin.*", [
            {vhost, ".*"},
            {configure, ".*"},
            {write, ".*"},
            {read, ".*"}
        ]},
        
        %% User1 with limited access to specific vhost and queue
        {".*CN=user1.*", [
            {vhost, "test_vhost"},
            {configure, "test_queue"},
            {write, "test_queue"},
            {read, "test_queue"}
        ]},
        
        %% User2 with access to restricted resources
        {".*CN=user2.*", [
            {vhost, "restricted_vhost"},
            {configure, "restricted_queue"},
            {write, "restricted_queue"},
            {read, "restricted_queue"}
        ]},
        
        %% Service accounts with specific naming pattern
        {".*CN=service-.*", [
            {vhost, "service_vhost"},
            {configure, "service_.*"},
            {write, "service_.*"},
            {read, "service_.*"}
        ]}
    ]}
]}
```

### Certificate DN Format

The plugin extracts the Distinguished Name from SSL certificates and formats it as:
`CN=commonname,OU=organizationalunit,O=organization,C=country`

Example certificate DNs that would match the patterns above:
- `CN=admin,OU=IT,O=Company,C=US`
- `CN=user1,OU=Engineering,O=Company,C=US`
- `CN=user2,OU=Operations,O=Company,C=US`
- `CN=service-api,OU=Services,O=Company,C=US`

### How It Works

1. **Certificate Extraction**: When a client connects with SSL, the plugin extracts the certificate DN
2. **DN Formatting**: The DN is formatted as a string (e.g., "CN=admin,OU=IT,O=Company,C=US")
3. **Regex Matching**: The formatted DN is matched against configured regex patterns
4. **Permission Checking**: If a pattern matches, the corresponding permissions are checked for the requested resource

### Access Control Flow

1. Client connects with SSL certificate
2. Plugin extracts DN from certificate
3. DN is matched against regex patterns in order
4. If pattern matches, permissions are checked:
   - VHost access is checked first
   - Resource permissions (configure, write, read) are checked as needed
   - Topic permissions are checked for topic operations
5. Access is granted or denied based on pattern matching and permission rules

## Build Instructions

This plug-in requires a [supported RabbitMQ release series](https://www.rabbitmq.com/versions.html). Build the plug-in following the
standard [Plugin Development Guide](https://www.rabbitmq.com/plugin-development.html).

## Testing

The plugin includes comprehensive tests that verify:

- Certificate DN extraction and formatting
- VHost access control based on certificate DN
- Resource-level permissions (configure, write, read)
- Topic-level permissions
- Access denial for non-matching certificates
- Proper regex pattern matching

Run the tests with:
```bash
make ct
```


