-module(amoc_compile).

-export([install_module/2]).

-ifdef(TEST).
-export([remove_module/1, module_src/1, module_beam/1]).
-endif.

-define(PRIV_DIR, code:priv_dir(amoc_arsenal)).

-type sourcecode() :: binary().

-spec install_module(module(), sourcecode()) ->
    ok | {error, [Errors :: string()], [Warnings :: string()]}.
install_module(Module, ModuleSource) ->
    case erlang:module_loaded(Module) of
        true -> {error, ["module with such name already exists"], []};
        false ->
            ModulePath = module_path_name(Module),
            write_module_to_file(ModuleSource, ModulePath),
            case compile_and_load_module(ModulePath) of
                {ok, Module} ->
                    amoc_code_server:add_module(Module),
                    ok;
                Error -> Error
            end
    end.

-ifdef(TEST).

remove_module(Module) ->
    true = code:delete(Module),
    code:purge(Module),
    ModulePath = module_path_name(Module),
    ok = file:delete(ModulePath ++ ".beam"),
    ok = file:delete(ModulePath ++ ".erl").

module_src(Module) ->
    module_path_name(Module) ++ ".erl".

module_beam(Module) ->
    module_path_name(Module) ++ ".beam".

-endif.


-spec module_path_name(module()) -> file:filename().
module_path_name(Module) -> %% w/o ".erl" extension
    filename:join([?PRIV_DIR, atom_to_list(Module)]).

-spec write_module_to_file(sourcecode(), file:filename()) -> ok.
write_module_to_file(ModuleSource, ModulePath) ->
    ok = file:write_file(ModulePath ++ ".erl", ModuleSource, [write]).

-spec compile_and_load_module(string()) -> {ok, module()} | {error, [string()], [string()]}.
compile_and_load_module(ModulePath) ->
    CompilationFlags = [{outdir, ?PRIV_DIR}, return_errors, report_errors, verbose],
    case compile:file(ModulePath, CompilationFlags) of
        {ok, Module} ->
            {module, Module} = code:load_abs(ModulePath),
            {ok, Module};
        {error, Errors, Warnings} ->
            file:delete(ModulePath ++ ".erl"),
            {error, Errors, Warnings}
    end.
