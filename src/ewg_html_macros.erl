-module(ewg_html_macros).
-export([r/1]).
-import(ewg_lib, [plget/2, plget/3, plset/3, tost/1]).
-import(ewg_access, [get_form_param/1, get_validation_result/1]).

r({delete_postback, Id}) -> delete_postback(Id);
r({delete_postback, Id, ConfirmText}) -> delete_postback(Id, ConfirmText);

r({edit_postback, Id}) -> edit_postback(Id);

r({action_postback, Action, Content}) -> action_postback("no_id", Action, Content);
r({action_postback, Id, Action, Content}) -> action_postback(Id, Action, Content);

r({action_postback_confirm, Action, Content, ConfirmText}) ->
    action_postback("no_id", Action, Content, ConfirmText);
r({action_postback_confirm, Id, Action, Content, ConfirmText}) ->
    action_postback(Id, Action, Content, ConfirmText);

r({eform, Content, RestContent}) -> eform([{method, "POST"}], Content, RestContent);
r({eform, Attrs, Content, RestContent}) -> eform(Attrs, Content, RestContent);
r({eform, Name, Attrs, Content, RestContent}) -> eform(Name, Attrs, Content, RestContent);

r({formtable, Content}) -> formtable(Content);

r({datatable, Header, Content}) -> datatable(Header, Content);
r({datatable, Header, Content, Options}) -> datatable(Header, Content, Options);

r({input_datepicker, Name}) -> input_datepicker(Name);
r({input_datepicker, Name, Value}) -> input_datepicker(Name, Value);

r({input_text, Name}) -> input_text(Name);
r({input_text, Name, Value}) -> input_text(Name, Value);

r({input_hidden, Name}) -> input_hidden(Name);
r({input_hidden, Name, Value}) -> input_hidden(Name, Value);

r({input_password, Name}) -> input_password(Name);
r({input_password, Name, Value}) -> input_password(Name, Value);

r({input_textarea, Name}) -> input_textarea(Name);
r({input_textarea, Name, Value}) -> input_textarea(Name, Value);

r({input_checkbox, Name}) -> input_checkbox(Name);
r({input_checkbox, Name, State}) -> input_checkbox(Name, State);

r({input_radio, Name, Options}) -> input_radio(Name, Options);
r({input_radio, Name, Options, Selected}) -> input_radio(Name, Options, Selected);

r({input_select, Name, Options}) -> input_select(Name, Options);
r({input_select, Name, Options, Selected}) -> input_select(Name, Options, Selected);

r({error, Text}) -> page_error(Text);

r({page_help, Content}) -> page_help(Content).

%% These can be used in a table data. In combination with the ewebgui.js
%% included in the page they submit the form enclosing this element. The input
%% names to check for in the received post are: "ewg_hidden_item_id" and "ewg_hidden_action".
%% these are generated by the ewebgui.js function based on the data- tags set.
%%
%% example: generating a table row: [{td, [], [Val]} || Val <- RowValues ++ [delete_postback(hd(RowValues))]]
delete_postback(Id) ->
    action_postback(Id, "delete", [{img, [{src, "/images/delete.gif"}]}]).

delete_postback(Id, ConfirmText) ->
    action_postback(Id, "delete", [{img, [{src, "/images/delete.gif"}]}], ConfirmText).

edit_postback(Id) ->
    action_postback(Id, "edit", [{img, [{src, "/images/edit.gif"}]}]).

%% generic action postback
action_postback(Id, Action, Content) ->
    action_postback(Id, Action, Content, undefined).
action_postback(Id, Action, Content, ConfirmText) ->
    {span,
        [
            {class, "ewg_event_item"},
            {'data-action', "'" ++ Action ++ "'"},
            {'data-id', "'" ++ Id ++ "'"}
        ] ++
        if ConfirmText =/= undefined -> [{'data-confirm', ConfirmText}]; true -> [] end,
        Content
    }.

submit_with_data(Data, Content) ->
    submit_with_data(Data, Content, []).
submit_with_data(Data, Content, Options) ->
    ConfirmText = plget(confirm, Options),
    Element = plget(element, Options, span),
    Class = plget(attr, Options, []),
    {Element, append_class([
        {class, "ewg_submit_item"},
        {'data-submit', pl_to_json(Data)}
    ], Class) ++
    if ConfirmText =/= undefined -> [{'data-confirm', ConfirmText}]; true -> [] end,
    Content
}.

pl_to_json(PL) -> ["{ " , pl_to_json(PL, []) , " }"].
pl_to_json([{K, V}], Acc) -> [Acc , "\'", tost(K) , "\' : \'" , tost(V), "\'"];
pl_to_json([{K, V}|Rest], Acc) ->
    pl_to_json(Rest, [Acc , "\'", tost(K) , "\' : \'" , tost(V) , "\' , "]).

%%
%% Form input and table rendering helpers
%%

formtable(Content) ->
    %% this will make a form or headerless display table without header row with
    %% simple alternating rows
    {_, TrList} = lists:foldl(
        fun(Row, {Odd, Rows}) -> {not Odd, Rows ++ [formtable_row(Odd, Row)]} end,
        {true, []},
        Content
    ),
    {table, [{class, "ewg_formtable"}], TrList}.

formtable_row(Odd, {tr, Attrs, Html}) ->
    OddClass = if Odd -> "odd"; true -> "even" end,
    {NewAttrs, AppendClass} = case lists:member(advanced, Attrs) of
        true ->
            {lists:delete(advanced, Attrs), OddClass ++ " ewg_advanced_option"};
        false ->
            {Attrs, OddClass}
    end,
    {tr, append_class(NewAttrs, AppendClass) , Html};

formtable_row(Odd, {Name, Value}) ->
    formtable_row(Odd, {Name, Value, []});
formtable_row(Odd, {Name, Value, Options}) ->
    OddClass = if Odd -> "odd"; true -> "even" end,
    Class = case lists:member(advanced, Options) of
        true -> OddClass ++ " ewg_advanced_option";
        _ -> OddClass
    end,
    {
        tr,
        [{class, Class}] ++
        case plget(hint, Options) of undefined -> []; Hint -> [{title, Hint}] end,
        [{th, [], Name}, {td, [], [Value]}]
    }.

append_class(Attr, Class) ->
    plset(class,
        string:join(
            lists:usort(
                string:tokens(plget(class, Attr, "") ++ " " ++ Class, " ")
            )
        , " ")
    , Attr).

datatable(Header, Content) ->
    datatable(Header, Content, []).

datatable(Header, Content, Options) ->
    %% this will make a data table
    {_, TrList} = lists:foldl(
        fun(Row, {Odd, Rows}) ->
            {not Odd, Rows ++ [
                {
                    tr,
                    [{class,
                        if Odd -> "odd"; true -> "even" end ++
                        case plget(rowclassfun, Options) of
                            RowClassFun when is_function(RowClassFun) -> " " ++ RowClassFun(Row);
                            _ -> ""
                        end
                    }],
                    [{td, [], [Value]} || Value <- Row, case Value of {hidden, _} -> false; _-> true end]
                }
            ]}
        end,
        {false, []},
        Content
    ),
    {
        table,
        [{class, "ewgtable fullwidth"}],
        [{tr, [], lists:map(
            fun
                ({sortable, Tag, Item}) ->
                    {ImgAsc, ImgDesc} = case plget(sorted, Options) of
                        {Tag, desc} ->
                            {"/images/sort_g_asc.png", "/images/sort_r_desc.png"};
                        {Tag, asc} ->
                            {"/images/sort_r_asc.png", "/images/sort_g_desc.png"};
                        _ ->
                            {"/images/sort_g_asc.png", "/images/sort_g_desc.png"}
                    end,
                    {th, [], [
                        Item, " ",
                        submit_with_data([{sort_tag, Tag}, {sort_dir, asc}], {img, [{src, ImgAsc}]}),
                        submit_with_data([{sort_tag, Tag}, {sort_dir, desc}], {img, [{src, ImgDesc}]})
                    ]};
                (Item) ->
                    {th, [], Item}
            end,
            Header
        )}, TrList]
    }.


input_text(Name) ->
    {input,
        [{type, "text"}, {name, Name}] ++
        case get_validation_result(Name) of
            [] -> [];
            Errors -> [{class, "validation_error"}, {title, " - " ++ string:join(Errors, "\n -")}]
        end ++
        case get_form_param(Name) of
            undefined -> [];
            Value -> [{value, Value}]
        end
    }.
input_text(Name, Value) ->
    {input, [{type, "text"}, {name, Name}, {value, Value}]}.

input_password(Name) ->
    {input,
        [{type, "password"}, {name, Name}, {autocomplete, "off"}] ++
        case get_validation_result(Name) of
            [] -> [];
            Errors -> [{class, "validation_error"}, {title, " - " ++ string:join(Errors, "\n - ")}]
        end ++
        case get_form_param(Name) of
            undefined -> [];
            Value -> [{value, Value}]
        end
    }.
input_password(Name, Value) ->
    {input, [{type, "password"}, {name, Name}, {value, Value}]}.


input_radio(Name, Options) ->
    input_radio(Name, Options, get_form_param(Name)).
input_radio(Name, Options, Selected) -> [
    [
        {
            input,
            [{type, "radio"}, {name, Name}, {value, OptVal}]
            ++ if Selected == OptVal -> [checked]; true -> [] end
        },
        OptLabel
    ] || {OptLabel, OptVal} <- Options
].

input_checkbox(Name) ->
    input_checkbox(Name, get_form_param(Name)).
input_checkbox(Name, State) ->
    {input, [{type, "checkbox"}, {name, Name}] ++ if State == "on" -> [checked]; true -> [] end}.

input_select(Name, Options) ->
    input_select(Name, Options, get_form_param(Name)).
input_select(Name, Options, Value) ->
    {select, [{name, Name}], [
        case Option of
            {OptLabel, OptVal} ->
                {option,
                    [{value, OptVal}] ++
                    if
                        Value == OptVal -> [selected]; true -> []
                    end,
                    OptLabel
                };
            OptVal ->
                {option,
                    [{value, OptVal}] ++
                    if
                        Value == OptVal -> [selected]; true -> []
                    end,
                    OptVal
                }
        end ||
        Option <- Options
    ]}.

input_textarea(Name) ->
    {textarea,
        [{name, Name}, {cols, "30"}, {rows, "5"}] ++
        case get_validation_result(Name) of
            [] -> [];
            Errors -> [{class, "validation_error"}, {title, " - " ++ string:join(Errors, "\n - ")}]
        end,
        case get_form_param(Name) of
            undefined -> [];
            Value -> [Value]
        end
    }.
input_textarea(Name, Value) ->
    {textarea, [{name, Name}, {cols, "30"}, {rows, "5"}], Value}.

input_datepicker(Name) ->
    {input, [{type, "text"}, {class, "datetimepicker"}, {name, Name}] ++
        case get_form_param(Name) of
            undefined -> [];
            Value -> [{value, Value}]
        end ++
        case get_validation_result(Name) of
            [] -> [];
            Errors ->  [{class, "validation_error"},{title, " - " ++ string:join(Errors, "\n - ")}]
        end
    }.

input_datepicker(Name, Value) ->
    {input, [{type, "text"}, {class, "datetimepicker"}, {name, Name}, {value, Value}]}.

input_hidden(Name) ->
    {input, [{type, "hidden"}, {name, Name}] ++
        case get_form_param(Name) of
            undefined -> [];
            Value -> [{value, Value}]
        end
    }.
input_hidden(Name, Value) ->
    {input, [{type, "hidden"}, {name, Name}, {value, Value}]}.

page_error(Text) ->
    {'div', [{class, "big_error"}], Text}.

page_help(Content) ->
    [{p, [{class, "ewg_page_help_toggle"}], "page help"}, {'div', [], Content}].

eform(Attr, Content, RestContent) ->
    eform("no_name", Attr, Content, RestContent).

eform(Name, IAttr, TableContent, RestContent) ->
    Attr = case plget(method, IAttr) of
        undefined -> plset(method, "POST", IAttr);
        _ -> IAttr
    end,
    {Nattr, AdvControl} = case lists:member(advanced, Attr) of
        true ->
            Status = case get_form_param("ewg_advanced_status") of
                "show" -> "show";
                _ -> "hide"
            end,
            {
                lists:delete(advanced, Attr),
                [
                    {p, [{class, "ewg_advanced_options_toggle"}]},
                    input_hidden("ewg_advanced_status", Status)
                ]
            };
        _ ->
            {Attr, []}
    end,
    {form, Nattr, [
        {input, [{type, "hidden"},{name, "ewg_form_name"}, {value, Name}]},
        AdvControl,
        formtable(TableContent),
        RestContent
    ]}.