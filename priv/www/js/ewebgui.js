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


$(function() {
    $(".ewg_event_item").on("click", function() {ewg_event_handler($(this))});
    $(".ewg_submit_item").on("click", function() {ewg_submit_item_handler($(this))});

    $(".ewg_page_help_toggle").on("click", function() {$(".ewg_page_help_toggle + div").slideToggle()});
    $(".ewg_advanced_options_toggle").on("click", function() {ewg_toggle_advanced_options($(this))});
    $(".datetimepicker").datetimepicker({dateFormat: "dd/mm/yy", firstDay: 1, numberOfMonths: 2});
    initialize_form_view_status();
});
