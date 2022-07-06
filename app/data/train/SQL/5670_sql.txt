--运行下面的查询可确定阻塞的会话。

select blocking_session_id, wait_duration_ms, session_id from 
sys.dm_os_waiting_tasks
where blocking_session_id is not null
--使用此调用可找出 blocking_session_id 所返回的 SQL。例如，如果 blocking_session_id 是 87，则运行此查询可获得相应的 SQL。

dbcc INPUTBUFFER(87)