-module(fp_lab2_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> fp_lab2_sup:start_link().

stop(_State) -> ok.
