-module(ft).
-export([help/0, s/1, s/2, f/1, e/0]).

help() ->
io:format(
"===============~n"
"ft is a simple utility over the erlang dbg trace facility.~n"
"functions:~n~n"
"s(Target)~n"
"Initates a function call trace in the shell that includes the ~n"
"return values as well. It stops the trace after 50 hits.~n"
"Target can be a module name then it traces all function calls~n"
"or {M, F} to trace 1 particular function or {M,F,Arity}~n~n"
"s(Target, Max)~n"
"Same as s(Target) but the maximum hits is defined by Max.~n~n"
"f(Target)~nSame as s(Target) but traces in files. The files ~n"
"can be processed using dbg:trace_port/2.~n~n"
"e() to end all the traces.~n"
"===============~n"
, []).

s(M) -> s(M, 50).
s(M, Max) -> trace(M, Max, shell).

f(M) ->
    trace(M, nomax, file).

e() -> dbg:stop_clear().

trace({M, F, A}, Max, Type) ->
    if
        Type == shell ->
            dbg:tracer(process,{
                fun
                    (_, N) when N > Max ->
                        dbg:stop_clear();
                    (Msg, N) ->
                        io:format("~p~n", [Msg]),
                        N+1
                end,
                0
            });
        true ->
            dbg:tracer(port,
                dbg:trace_port(file,{
                    "trace_",
                    wrap,
                    "_" ++ atom_to_list(node()) ++ timestamp(),
                    50000000,12
                })
            )
    end,
    dbg:tpl(M, F, A, [{'_', [], [{return_trace}]}]),
    dbg:p(all, [call, timestamp]);

trace({M, F}, Max, Type) ->
    trace({M, F, '_'}, Max, Type);

trace(M, Max, Type) ->
    trace({M, '_', '_'}, Max, Type).

timestamp() ->
     {{Ye, Mo, Da}, {Ho, Mi, Se}} = calendar:local_time(),
    io_lib:format("~4.10B~2.10.0B~2.10B_~2.10B~2.10B~2.10B", [Ye, Mo, Da, Ho, Mi, Se]).
