# Install and Build

stack install mighttpd2

stack clean
stack build

# Generate docs

./sig-gen -d './public_html' -s rest-gen/files/Docs/

# Generate client codes

## Generate haskell code

./sig-gen -h

## generate javascript code

./sig-gen -j -t sig.js
mv sig.js public_html/js

# Run

./sig

sudo .stack-work/install/x86_64-linux/lts-3.6/7.10.2/bin/mighty .mighty/default.conf .mighty/default.route

access to http://localhost/~cutsea110/ on browser

