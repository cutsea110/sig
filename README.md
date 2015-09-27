stack install mighttpd2

stack clean
stack build

./sig

sudo .stack-work/install/x86_64-linux/lts-3.6/7.10.2/bin/mighty .mighty/default.conf .mighty/default.route

access to http://localhost/~cutsea110/ on browser
