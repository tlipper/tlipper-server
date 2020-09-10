FROM haskell:8.6.5

WORKDIR /opt/tlipper-server

RUN stack update

# Add just the .cabal file to capture dependencies
COPY ./tlipper-server.cabal /opt/tlipper-server/tlipper-server.cabal
COPY ./stack.yaml /opt/tlipper-server/stack.yaml
COPY ./stack.yaml.lock /opt/tlipper-server/stack.yaml.lock
COPY ./*.md /opt/tlipper-server/

# Install the postgres client
RUN apt-get update
RUN apt-get install -y libpq-dev

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack install --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/tlipper-server
RUN stack install

EXPOSE 8080 8081 3000
CMD ["tlipper-server-exe"]
