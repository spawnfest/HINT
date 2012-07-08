erl -pa ebin ./apps/*/ebin ./deps/*/ebin ./site/ebin -name hint@127.0.0.1 -boot start_sasl -run make all load -s saloon_app
