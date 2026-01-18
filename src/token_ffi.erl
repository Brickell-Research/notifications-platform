-module(token_ffi).
-export([hmac_sha256/2, hex_encode/1, base64_encode/1, base64_decode/1]).

hmac_sha256(Secret, Data) ->
    crypto:mac(hmac, sha256, Secret, Data).

hex_encode(Data) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [Byte]) || <<Byte>> <= Data])).

%% URL-safe base64 encoding (RFC 4648)
%% Replaces + with -, / with _, and removes = padding
base64_encode(Data) ->
    Standard = base64:encode(Data),
    UrlSafe = binary:replace(
        binary:replace(Standard, <<"+">>, <<"-">>, [global]),
        <<"/">>, <<"_">>, [global]
    ),
    %% Remove padding
    binary:replace(UrlSafe, <<"=">>, <<>>, [global]).

%% URL-safe base64 decoding
%% Replaces - with +, _ with /, and adds padding back
base64_decode(Data) ->
    try
        %% Restore standard base64 characters
        Standard = binary:replace(
            binary:replace(Data, <<"-">>, <<"+">>, [global]),
            <<"_">>, <<"/">>, [global]
        ),
        %% Add padding if needed
        Len = byte_size(Standard),
        Padded = case Len rem 4 of
            0 -> Standard;
            2 -> <<Standard/binary, "==">>;
            3 -> <<Standard/binary, "=">>;
            _ -> Standard
        end,
        {ok, base64:decode(Padded)}
    catch
        _:_ -> {error, nil}
    end.
