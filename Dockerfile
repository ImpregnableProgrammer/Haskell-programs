# Use the slim docker image
# Dockerfile reference: https://docs.docker.com/reference/dockerfile/
# Dockerfile best practices: https://docs.docker.com/develop/develop-images/dockerfile_best-practices/
FROM debian:stable-slim
# Docker volumes reference: https://docs.docker.com/storage/volumes/
# Create a mount point for external volumes to persist files on the host machine
# Unneeded for VSCode devcontainers since VSCode takes care of creating the mount point
# for the current directory on the host machine
# WORKDIR /app
# VOLUME /app
# Copy programs into the CWD "/app" in the container
# COPY ./programs .
# Start installing GHCUp and necessary dependencies
# We'll use `cabal` instead of `stack` for the Haskell package management
# Along with installing `ghc` for compiling Haskell files
# GHCUp installation guide: 
# NOTE: Each `RUN` instruction creates a new (cached) image layer after launching temporary containers
## to run the commands given, and then committing the result of each `RUN` command to a new image
## layer! Anything written in the running container is discarded when the container is stopped and removed.
# Docker image layers: https://docs.docker.com/build/guide/layers/
ENV DEBIAN_FRONTEND=noninteractive \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1
RUN apt-get update && apt-get install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 ca-certificates git && apt-get clean
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN . /root/.ghcup/env && \
    ghcup install ghc && \
    ghcup install cabal && \
    ghcup install hls && \
    ghcup set ghc && \
    ghcup set cabal && \
    ghcup set hls && \
    cabal update
# cabal-install reference: https://cabal.readthedocs.io/en/3.4/cabal-commands.html
# haskell language server (hls) reference: https://haskell-language-server.readthedocs.io/en/stable/what-is-hls.html
# Install hls plugin packages using cabal-install!!
# Install Haskell package `hlint` for use with the `haskell-linter` vscode extension
RUN . /root/.ghcup/env && cabal install hlint
# Add the haskell executable directories to the PATH
RUN echo "export PATH=/root/.local/bin:/root/.cabal/bin:/root/.ghcup/bin:${PATH}" >> ~/.bashrc

## ADD ADDITIONAL CABAL-INSTALL COMMANDS (line 37) BELOW THIS COMMENT TO INSTALL MORE PACKAGES!! ##
