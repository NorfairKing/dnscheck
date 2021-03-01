set -ex

stack install \
  --file-watch --watch-all \
  --pedantic --fast \
  --exec='dnscheck ./examples/cs-syd.yaml'
