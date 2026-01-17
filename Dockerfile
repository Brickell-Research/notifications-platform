FROM ghcr.io/gleam-lang/gleam:v1.14.0-erlang-alpine

WORKDIR /app

# Create non-root user
RUN addgroup -S app && adduser -S app -G app

# Copy dependency files first for caching
COPY gleam.toml manifest.toml ./
RUN gleam deps download

# Copy source and build
COPY src ./src
COPY priv ./priv/
RUN gleam build

# Change ownership of app directory to non-root user
RUN chown -R app:app /app

# Switch to non-root user
USER app

EXPOSE 8088

HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:8088/health || exit 1

CMD ["gleam", "run"]
