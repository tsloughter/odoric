%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 19 April 2012 by Tristan Sloughter <tristan@mashape.com>
%%%-------------------------------------------------------------------
-module(odoric_uploader).

%% API
-export([upload/2,
         upload/3]).

upload(s3, Artifacts) -> 
    upload(s3, "erlang_target_systems", Artifacts).

upload(s3, Bucket, Artifacts) ->
    AwsCreds = get_aws_creds(),
    lists:foreach(fun(Artifact) ->
                          io:format("Uplading ~s to s3 bucket ~s...~n", [Artifact, Bucket]),
                          {ok, Binary} = file:read_file(Artifact),
                          Key = filename:basename(Artifact),
                          erlcloud_s3:put_object(Bucket, Key, Binary, AwsCreds)
                  end, Artifacts).

get_aws_creds() ->
    CredFilePath = os:getenv("AWS_CREDENTIAL_FILE"),
    {ok, CredFile} = file:read_file(CredFilePath), 
    AwsCreds =[list_to_tuple(string:tokens(Line, "=")) || Line <- string:tokens(binary_to_list(CredFile), "\n")],

    AccessKeyId = aws_access_key_id(AwsCreds),
    SecretKey = aws_secret_key(AwsCreds),

    erlcloud_s3:new(AccessKeyId, SecretKey).
    
aws_access_key_id(AwsCreds) ->
    {"AWSAccessKeyId", AccessKeyId} = lists:keyfind("AWSAccessKeyId", 1, AwsCreds),    
    AccessKeyId.

aws_secret_key(AwsCreds) ->
    {"AWSSecretKey", SecretKey} = lists:keyfind("AWSSecretKey", 1, AwsCreds),    
    SecretKey.










