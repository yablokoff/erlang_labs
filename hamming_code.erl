-module(hamming_code).
-export([get_bit/2, get_check_sum/2, get_check_bit/2, get_check_arrays/0, get_extended_from_num/1,
		 cond_map/4, set_check_bit/3, encode_num/1, decode_num/1]).

get_bit(Num, Pos) ->
	% Get bit from binary at certain position
	StartPos = Pos-1,
	<<_:StartPos, Sec:1, _/bitstring>> = Num,
	Sec.

get_check_sum(ListToCheck, Num) ->
	% Calc sum of all controlled bits
	% ListToCheck - the list of that bits
	get_check_sum_acc(0, ListToCheck, Num).

get_check_sum_acc(Acc, [], _) ->
	Acc;
get_check_sum_acc(Acc, [H|ListToCheck], Num) ->
	Bit = get_bit(Num, H),
	get_check_sum_acc(Acc+Bit, ListToCheck, Num).

get_check_bit(ListToCheck, Num) ->
	% Calc check_bit as it should be (for encoding).
	CheckSum = get_check_sum(ListToCheck, Num),
	if (CheckSum rem 2) =:= 0 ->
		1;
		true -> 0
	end.

get_check_arrays() ->
	% Return dict with
	% what each bit controls.
	D0 = dict:new(),
	D1 = dict:store(1,[3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37], D0),
	D2 = dict:store(2,[3,6,7,10,11,14,15,18,19,22,23,26,27,30,31,34,35,38], D1),
	D3 = dict:store(4,[5,6,7,12,13,14,15,20,21,22,23,28,29,30,31,36,37,38], D2),
	D4 = dict:store(8,[9,10,11,12,13,14,15,24,25,26,27,28,29,30,31], D3),
	D5 = dict:store(16,[17,18,19,20,21,22,23,24,25,26,27,28,29,30,31], D4),
	CheckArray = dict:store(32,[33,34,35,36,37,38], D5),
	CheckArray.

get_extended_from_num(Num) ->
	% Build extended (38-bits) binary from number (32-bits)
	<<A1:1,A2:3,A3:7,A4:15,A5:6>> = <<Num:32>>,
	ExtendedNum = <<0:2,A1:1,0:1,A2:3,0:1,A3:7,0:1,A4:15,0:1,A5:6>>,
	ExtendedNum.

get_num_from_extended(ExtendedNum) ->
	% Build normal number (32-bits) from extended (38-bits) binary
	<<_:2,A1:1,_:1,A2:3,_:1,A3:7,_:1,A4:15,_:1,A5:6>> = ExtendedNum,
	Num = <<A1:1,A2:3,A3:7,A4:15,A5:6>>,
	<<N:32/integer>> = Num,
	N.

cond_map(F,X,List,CondPos) ->
	% Conditional map.
	% Execute function F with additional argument <X> on <CondPos>-th element of <List>
	cond_map_ar(F,X,List,0,CondPos).

cond_map_ar(_,_,[],_,_) -> 
	[];
cond_map_ar(F,X,[H|Tail],CurPos,CondPos) ->
	NewList = cond_map_ar(F,X,Tail,CurPos+1,CondPos),
	case(CurPos == CondPos) of
		true -> 
			[F(H,X)|NewList];
		false ->
			[H|NewList]
	end.

set_bit(Elem, BitShift) ->
	Elem bor (1 bsl BitShift).

change_bit(Elem, BitShift) ->
	Elem bxor (1 bsl BitShift).

set_check_bit(Pos, CheckArray, ExtendedNum) ->
	% Set appropriate check bit at position <Pos>
	Bit = get_check_bit(dict:fetch(Pos,CheckArray), ExtendedNum),
	if Bit =:= 1 ->
		{1, operate_with_bit(fun set_bit/2, Pos, ExtendedNum)};
	true -> {0, ExtendedNum}
	end.

set_check_bit_ar(0,_,ExtendedNum) ->
	% Build new extended num with Hamming code,
	% return tuple with list of control bits and NewExtendedNum
	{[],ExtendedNum};
set_check_bit_ar(Order,CheckArray,ExtendedNum) ->
	{BitList,ModExtendedNum} = set_check_bit_ar(Order div 2, CheckArray, ExtendedNum),
	{Bit, NewExtendedNum} = set_check_bit(Order,CheckArray, ModExtendedNum),
	{[Bit|BitList], NewExtendedNum}.

operate_with_bit(Fun, Pos, ExtendedNum) ->
	% Apply action <Fun> on a bit at position Pos of ExtendedNum
	SecNum = (Pos-1) div 8, % get number of section
	BitNum = (Pos-1) rem 8,
	BitShift = 7-BitNum,
	list_to_bitstring(cond_map(Fun, BitShift, bitstring_to_list(ExtendedNum), SecNum)).

%%
%% Example: 11001100110011001100110011001100 (3435973836)
%% Control bit set: (0) (1) 1 (1) 100 (1) 1100110 (1) 011001100110011 (1) 001100
%%

encode_num(Num) ->
	ExtendedNum = get_extended_from_num(Num),
	CheckArray = get_check_arrays(),
	{BitList,ModExtendedNum} = set_check_bit_ar(32,CheckArray,ExtendedNum),
	{lists:reverse(BitList),ModExtendedNum}.

check_bit_at({Pos, Bit, ExtendedNum}) ->
	ActualBit = get_bit(ExtendedNum, Pos),
	if ActualBit =/= Bit ->
		false;
		true -> true
	end.

check_errors_at_control_bit({ {Pos, CheckArray}, ExtendedNum}) ->
	% For current position and checkarray return tuple like {2, false}
	% with checked bit's correctness.
	CheckBit = get_check_bit(CheckArray, ExtendedNum),
	CheckBitCorrectness = check_bit_at({Pos, CheckBit, ExtendedNum}),
	{Pos, CheckBitCorrectness}.

decode_num(ExtendedNum) ->
	%
	% hamming_code:decode_num(<<121,205,102,103,12:6>>): All correct
	% hamming_code:decode_num(<<89,205,102,103,12:6>>): Incorrect bit 3
	% hamming_code:decode_num(<<125,205,102,103,12:6>>): Incorrect bit 6
	%
	CheckArray = get_check_arrays(),
	CheckArrayListUnsorted = dict:to_list(CheckArray),
	CheckArrayList = lists:keysort(1, CheckArrayListUnsorted),
	CheckArrayListWithNum = lists:zip(CheckArrayList, lists:duplicate(length(CheckArrayList), ExtendedNum)),
	io:fwrite("CheckArrayListWithNum: ~W~n", [CheckArrayListWithNum,19]),
	CheckBitsArray = lists:map(fun check_errors_at_control_bit/1, CheckArrayListWithNum),
	IncorrectControlBitsArray = lists:filter(fun({_, Correct}) -> Correct =:= false end, CheckBitsArray),
	io:fwrite("IncorrectBitsArray: ~W~n", [IncorrectControlBitsArray,19]),
	IncorrectBit = lists:foldl(fun({Pos,_}, Acc) -> Acc + Pos end, 0, IncorrectControlBitsArray),
	io:fwrite("IncorrectBit: ~W~n", [IncorrectBit,19]),
	if IncorrectBit =/= 0 ->
		NewExtendedNum = operate_with_bit(fun change_bit/2, IncorrectBit, ExtendedNum);
		true -> NewExtendedNum = ExtendedNum
	end,
	io:fwrite("NewExtendedNum: ~W~n", [NewExtendedNum,19]),
	get_num_from_extended(NewExtendedNum).