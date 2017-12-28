%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(query_util).
-export([decode_input_file/2]).

decode_input_file(File, Fun)->
	case file:read_file(File) of
		{ok, Bin} -> 
			decode_detail(Bin, <<>>, Fun);
		{error, Reason}->
			lager:info("read file error:~p~n",[Reason])
	end.

decode_detail(<<>>, _, _)->
	ok;
decode_detail(<<"(",B1,B2,B3,B4,B5,B6,")",Binary/binary>>, LastName, Fun) ->
	Code = <<B1,B2,B3,B4,B5,B6>>,
	Fun({Code, LastName}),
	decode_detail(Binary,<<>>, Fun);
decode_detail(<<"\n",Binary/binary>>,LastName, Fun) ->
	decode_detail(Binary,LastName, Fun);
decode_detail(<<C,Binary/binary>>, LastName, Fun) ->
	decode_detail(Binary, <<LastName/binary, C>>, Fun).