%% This is to include some common metadata in events
-define(EVENT(EventId, EventData), ewg_event:event(EventId, [
    {ewg_event_id, EventId},
    {ewg_event_env, [
        {module, ?MODULE},
        {line, ?LINE},
        {pid, self()}
    ]} | EventData
])).

%% This we'll use for marking translatable texts (that part is to be worked out)
-define(_(Text), Text).
