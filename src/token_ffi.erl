-module(token_ffi).
-export([hmac_sha256/2, hex_encode/1, base64_encode/1, base64_decode/1]).

hmac_sha256(Secret, Data) ->
    crypto:mac(hmac, sha256, Secret, Data).

hex_encode(Data) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [Byte]) || <<Byte>> <= Data])).

base64_encode(Data) ->
    base64:encode(Data).

base64_decode(Data) ->
    try
        {ok, base64:decode(Data)}
    catch
        _:_ -> {error, nil}
    end.
