# Use official Erlang image
FROM erlang:26-alpine

# Install build deps and compatibility layer for DNS
RUN apk add --no-cache git make gcc libc-dev openssl-dev libc6-compat

WORKDIR /app

# Copy rebar3 config and lock files first for caching
COPY rebar.config rebar.lock ./

# Copy all source
COPY . .

# Build release
RUN rebar3 as prod release

# Expose port
EXPOSE 8080

# Start the app
CMD ["./_build/prod/rel/secure_chat/bin/secure_chat", "foreground"]
