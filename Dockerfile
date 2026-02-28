# Use official Erlang image (Debian-based to avoid Alpine DNS bugs)
FROM erlang:26

# Install build deps
RUN apt-get update && apt-get install -y git make gcc libc-dev libssl-dev

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
