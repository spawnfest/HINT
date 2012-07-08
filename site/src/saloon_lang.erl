-module(saloon_lang).

-export([t/1]).

-define(SRCPATH, "site/src/").
-define(BINPATH, "site/ebin/").

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%%spawnpoint

t([{text,<<"HINT overview">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"HINT overview">>;
		ru -> <<"HINT overview">>;
		lv -> <<"HINT overview">>;
		es -> <<"HINT overview">>;
		_ -> <<"*HINT overview*">>
	end;

t([{title,<<"HINT overview">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"HINT overview">>;
		ru -> <<"HINT overview">>;
		lv -> <<"HINT overview">>;
		es -> <<"HINT overview">>;
		_ -> <<"*HINT overview*">>
	end;

t([{text,<<"HINT search engine">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"HINT search engine">>;
		ru -> <<"HINT search engine">>;
		lv -> <<"HINT search engine">>;
		es -> <<"HINT search engine">>;
		_ -> <<"*HINT search engine*">>
	end;

t([{text,<<"With blackjack and hookers">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"With blackjack and hookers">>;
		ru -> <<"With blackjack and hookers">>;
		lv -> <<"With blackjack and hookers">>;
		es -> <<"With blackjack and hookers">>;
		_ -> <<"*With blackjack and hookers*">>
	end;

t([{text,<<"HINT search engine.">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"HINT search engine.">>;
		ru -> <<"HINT search engine.">>;
		lv -> <<"HINT search engine.">>;
		es -> <<"HINT search engine.">>;
		_ -> <<"*HINT search engine.*">>
	end;

t([{caption,<<"English">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"English">>;
		ru -> <<"English">>;
		lv -> <<"English">>;
		es -> <<"English">>;
		_ -> <<"*English*">>
	end;

t([{caption,<<"Russian">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Russian">>;
		ru -> <<"Russian">>;
		lv -> <<"Russian">>;
		es -> <<"Russian">>;
		_ -> <<"*Russian*">>
	end;

t([{caption,<<"Latvian">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Latvian">>;
		ru -> <<"Latvian">>;
		lv -> <<"Latvian">>;
		es -> <<"Latvian">>;
		_ -> <<"*Latvian*">>
	end;

t([{text,<<"Saloon overview">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Saloon overview">>;
		ru -> <<"Saloon overview">>;
		lv -> <<"Saloon overview">>;
		es -> <<"Saloon overview">>;
		_ -> <<"*Saloon overview*">>
	end;

t([{title,<<"Saloon overview">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Saloon overview">>;
		ru -> <<"Saloon overview">>;
		lv -> <<"Saloon overview">>;
		es -> <<"Saloon overview">>;
		_ -> <<"*Saloon overview*">>
	end;

t([{text,<<"To tweak the css, please download twitter-bootstrap and compile less in static/css.">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"To tweak the css, please download twitter-bootstrap and compile less in static/css.">>;
		ru -> <<"To tweak the css, please download twitter-bootstrap and compile less in static/css.">>;
		lv -> <<"To tweak the css, please download twitter-bootstrap and compile less in static/css.">>;
		es -> <<"To tweak the css, please download twitter-bootstrap and compile less in static/css.">>;
		_ -> <<"*To tweak the css, please download twitter-bootstrap and compile less in static/css.*">>
	end;

t([{text,<<"This is an example of bootstrap-powered langing page.">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"This is an example of bootstrap-powered langing page.">>;
		ru -> <<"This is an example of bootstrap-powered langing page.">>;
		lv -> <<"This is an example of bootstrap-powered langing page.">>;
		es -> <<"This is an example of bootstrap-powered langing page.">>;
		_ -> <<"*This is an example of bootstrap-powered langing page.*">>
	end;

t([{text,<<"Framework that offers, not forces.">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Framework that offers, not forces.">>;
		ru -> <<"Framework that offers, not forces.">>;
		lv -> <<"Framework that offers, not forces.">>;
		es -> <<"Framework that offers, not forces.">>;
		_ -> <<"*Framework that offers, not forces.*">>
	end;

t([{caption,<<"Get modern Internet Explorer">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Get modern Internet Explorer">>;
		ru -> <<"Get modern Internet Explorer">>;
		lv -> <<"Get modern Internet Explorer">>;
		es -> <<"Get modern Internet Explorer">>;
		_ -> <<"*Get modern Internet Explorer*">>
	end;

t([{link,<<"Get modern Internet Explorer">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Get modern Internet Explorer">>;
		ru -> <<"Get modern Internet Explorer">>;
		lv -> <<"Get modern Internet Explorer">>;
		es -> <<"Get modern Internet Explorer">>;
		_ -> <<"*Get modern Internet Explorer*">>
	end;

t([{error,<<"Old web-browser">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Old web-browser">>;
		ru -> <<"Old web-browser">>;
		lv -> <<"Old web-browser">>;
		es -> <<"Old web-browser">>;
		_ -> <<"*Old web-browser*">>
	end;

t([{error,<<"The site requires javascript.">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"The site requires javascript.">>;
		ru -> <<"The site requires javascript.">>;
		lv -> <<"The site requires javascript.">>;
		es -> <<"The site requires javascript.">>;
		_ -> <<"*The site requires javascript.*">>
	end;

t([{meta,<<"Copyright notice">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"Copyright notice">>;
		ru -> <<"Copyright notice">>;
		lv -> <<"Copyright notice">>;
		es -> <<"Copyright notice">>;
		_ -> <<"*Copyright notice*">>
	end;

t([{text,<<"MIT License, HINT Ignoble Narcissist Team. For Spawnfest2012">>}]) -> 
	case saloon_ctx:language() of 
		en -> <<"MIT License, HINT Ignoble Narcissist Team. For Spawnfest2012">>;
		ru -> <<"MIT License, HINT Ignoble Narcissist Team. For Spawnfest2012">>;
		lv -> <<"MIT License, HINT Ignoble Narcissist Team. For Spawnfest2012">>;
		es -> <<"MIT License, HINT Ignoble Narcissist Team. For Spawnfest2012">>;
		_ -> <<"*MIT License, HINT Ignoble Narcissist Team. For Spawnfest2012*">>
	end;

t([{Tag,X}]) -> 
	Fname = ?SRCPATH ++ saloon_util:to_list(?MODULE) ++ ".erl",
	{_, Myself} = file:read_file(Fname),
	NewFunText = io_lib:format("%%spawnpoint~n~nt([{~w,<<\"~ts\">>}]) -> ~n	case saloon_ctx:language() of ~n", [Tag,X]),
	NewFunText2= lists:foldl(fun (L, Acc) -> Acc ++ io_lib:format("		~ts -> <<\"~ts\">>;~n", [L, X]) end, "", saloon_conf:languages()),
	NewFunText3= io_lib:format("		_ -> <<\"*~ts*\">>~n	end;", [X]),
	NewerSelf = re:replace(
		Myself, 
		<<"%%spawnpoint">>, 
		unicode:characters_to_binary(NewFunText ++ NewFunText2 ++ NewFunText3)
	),
	file:write_file("site/src/saloon_lang.erl", NewerSelf),
	compile:file(Fname, [verbose,report_errors,report_warnings,{outdir,?BINPATH}]),
	%os:cmd("erlc " ++ Fname ++ " -o " ++ ?BINPATH), %%TODO: change to compile:file!
	code:purge(saloon_lang),
	code:load_file(saloon_lang),
	[<<"¡">>, X, <<"!">>].
