FROM ruby:3.3-slim

# Install native dependencies needed by charlock_holmes, rugged, etc.
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    pkg-config \
    libicu-dev \
    libssl-dev \
    zlib1g-dev \
    libcurl4-openssl-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /linguist

# Copy everything (use .dockerignore to skip vendor/bundle if needed)
COPY . .

RUN bundle install --jobs 4

# Build linguist's native C extension and generate samples.json
RUN bundle exec rake compile
RUN bundle exec rake samples
RUN bundle exec github-linguist --version

CMD ["bash"]
