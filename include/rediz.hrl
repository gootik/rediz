%% Redis Protocol
-define(RESP_TYPE_STRING, $+).
-define(RESP_TYPE_ERROR, $-).
-define(RESP_TYPE_INTEGER, $:).
-define(RESP_TYPE_BULK_STRING, $$).
-define(RESP_TYPE_ARRAY, $*).

-define(RESP_DELIM, "\r\n").
-define(RESP_LF_INT, $\n).

-define(RESP_NULL, "-1").

-define(DEFAULT_POOL_SIZE, 16).

%% External Types
-type rediz_reply() :: {ok, reply()} | {error, binary()}.
-type reply() :: reply_types() | [reply_types()].
-type reply_types() :: integer() | binary() | undefined.

-type rediz_command() :: tuple().

-export_type([
    rediz_reply/0,
    rediz_command/0
]).