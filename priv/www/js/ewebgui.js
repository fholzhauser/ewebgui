function ewg_event_handler(item) {
    var action = item.attr("data-action");
    var id = item.attr("data-id");
    var confirm_text = item.attr("data-confirm");
    var form = item.closest("form");
    var action_input = form.find("input[name=ewg_hidden_action]")[0];
    var id_input = form.find("input[name=ewg_hidden_item_id]")[0];

    if (confirm_text) {
        if (confirm(confirm_text)) {
            if (id_input) {
                id_input.value(id);
            } else {
                form.append("<input type=hidden name=ewg_hidden_item_id value=" + id + ">");
            };
            if (action_input) {
                action_input.value(id);
            } else {
                form.append("<input type=hidden name=ewg_hidden_action value=" + action + ">");
            };
            form.submit();
        }
    } else {
        if (id_input) {
            id_input.value(id);
        } else {
            form.append("<input type=hidden name=ewg_hidden_item_id value=" + id + ">");
        };
        if (action_input) {
            action_input.value(id);
        } else {
            form.append("<input type=hidden name=ewg_hidden_action value=" + action + ">");
        };
        form.submit();
    }
};

function initialize_form_view_status() {
    var items = $(".ewg_advanced_options_toggle");
    $.each(items, function(i, item) {
        var form = $(item).closest("form");
        var input_field = form.find("input[name=ewg_advanced_status]")[0];
        if ($(input_field).val() == "hide") {
            $(item).html("show advanced");
            $(".ewg_advanced_option").hide();
        } else {
            $(item).html("hide advanced");
            $(".ewg_advanced_option").show();
        }
    })
};


function ewg_toggle_advanced_options(item) {
    var form = $(item).closest("form");
    var input_field = form.find("input[name=ewg_advanced_status]")[0];
    if ($(input_field).val() == "show") {
        $(".ewg_advanced_option").hide();
        $(input_field).val("hide");
        $(item).html("show advanced");
    } else {
        $(".ewg_advanced_option").show();
        $(input_field).val("show");
        $(item).html("hide advanced");
    }
};

function ewg_init_sortable_picks() {
    var items = $(".ewg_sortable_pick_list");
    var sortfun = function(a,b) {
        return $(a).text().toLowerCase() > $(b).text().toLowerCase() ? 1 : -1;
    };
    var getvaluefun = function(list) {
        var value = $.map($(list).find("li"), function(resultval, resultitem) {
            return $(resultval).text();
        });
        return value.join("|");
    };
    $.each(items, function(i, item) {
        var id = $(item).attr("id");
        var input = $(item).find("input[type=hidden]");
        var itemlist = "#" + id + " .itemlist";
        var optionlist = "#" + id + " .optionlist";
        $(item).find(".itemlist").sortable({
            connectWith : optionlist,
            create : function(event, ui) {
                $(input).val(getvaluefun($(this)));
            },
            update : function(event, ui) {
                $(input).val(getvaluefun($(this)));
            },
            scroll: true
        });
        $(item).find(".optionlist").sortable({
            connectWith : itemlist,
            update : function(event, ui) {
                var optionitems = $(this).find("li");
                var sortedvals = $(optionitems).sort(sortfun);
                $(this).html(sortedvals);
            },
            scroll: true
        })
    })
};

$(function() {
    ewg_init_sortable_picks();
    $(".ewg_event_item").on("click", function() {ewg_event_handler($(this))});
    $(".ewg_submit_item").on("click", function() {ewg_submit_item_handler($(this))});

    $(".ewg_page_help_toggle").on("click", function() {$(".ewg_page_help_toggle + div").slideToggle()});
    $(".ewg_advanced_options_toggle").on("click", function() {ewg_toggle_advanced_options($(this))});
    $(".datetimepicker").datetimepicker({dateFormat: "dd/mm/yy", firstDay: 1, numberOfMonths: 2});
    initialize_form_view_status();
});
