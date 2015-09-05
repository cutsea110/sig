cabal clean
cabal configure
cabal build

./sig

sudo .cabal-sandbox/bin/mighty .mighty/default.conf .mighty/default.route

access to http://localhost/~cutsea110/ on browser
