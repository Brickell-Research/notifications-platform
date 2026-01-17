-module(smtp_ffi).
-export([send_email/7]).

send_email(Host, Port, Username, Password, From, To, Message) ->
    HostStr = binary_to_list(Host),

    %% SSL/TLS options - no verification for maximum compatibility
    SslOpts = [
        {verify, verify_none},
        {versions, ['tlsv1.2', 'tlsv1.3']}
    ],

    BaseOptions = [
        {relay, HostStr},
        {port, Port},
        {username, binary_to_list(Username)},
        {password, binary_to_list(Password)},
        {auth, always},
        {no_mx_lookups, true}
    ],

    Options = case Port of
        465 ->
            %% Direct SSL connection on port 465
            BaseOptions ++ [{ssl, true}, {sockopts, SslOpts}];
        _ ->
            %% STARTTLS on port 587
            BaseOptions ++ [{tls, if_available}, {tls_options, SslOpts}]
    end,

    case gen_smtp_client:send_blocking(
        {binary_to_list(From), [binary_to_list(To)], binary_to_list(Message)},
        Options
    ) of
        {ok, _Receipt} ->
            {ok, nil};
        Receipt when is_binary(Receipt) ->
            %% Some servers return just the receipt as binary
            {ok, nil};
        Receipt when is_list(Receipt) ->
            %% Some servers (like Zoho) return just the receipt as string
            {ok, nil};
        {error, Type, Reason} ->
            ErrorMsg = io_lib:format("~p: ~p", [Type, Reason]),
            {error, {send_error, list_to_binary(lists:flatten(ErrorMsg))}};
        {error, Reason} ->
            ErrorMsg = io_lib:format("~p", [Reason]),
            {error, {send_error, list_to_binary(lists:flatten(ErrorMsg))}}
    end.
