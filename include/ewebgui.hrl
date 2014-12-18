-define(EVENT(EventId, EventData), ewg_event:event([
    {ewg_event, [
        {module, ?MODULE},
        {line, ?LINE},
        {pid, self()},
        {id, EventId}
    ]} | EventData
])).

-define(_(Text), Text).
