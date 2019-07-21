%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Name : Hussein Muhammad El-Sayed				  %
% Academic Year : 4th Computer Engineering 2019   %
% Date : 24-4-2019								  %
% Projct : Simple parser for c++ Language         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


parser --> [].
parser --> ["main"],["("],[")"], ["{"], stmts,["}"] | data_type ,["main"],["("],[")"], ["{"], stmts,["}"] | data_type,["main"],["("],data_type,id,[")"], ["{"], stmts,["}"] .

stmts --> stmt, stmts | [].
stmt --> if_stmt | for_stmt | while_stmt | do_while_stmt | definition_stmt | assign_stmt | assign_stmt_special | cin_cout_stmt | return_stmt.

assign_stmt --> id, ["="], expr, [";"].
assign_stmt2 --> id, ["="], expr | id, ["+"], ["+"] | id, ["-"], ["-"] | ["+"], ["+"] ,id |  ["-"], ["-"] ,id .  % without semicol for some loops
assign_stmt_special --> id, ["+"], ["+"] , [";"] | id,  ["-"], ["-"], [";"] |  ["+"], ["+"] ,id , [";"] |  ["-"], ["-"] ,id , [";"] | id, ["+"],["="], expr,[";"]| id, ["-"],["="], expr,[";"]| id, ["*"],["="], expr,[";"]| id, ["/"],["="], expr,[";"] .

definition_stmt --> data_type , id ,[";"] | data_type , assign_stmt | data_type , id ,more_id,[";"] .
data_type --> ["char"] | ["int"] | ["float"] | ["double"] | ["bool"] | ["String"] | ["void"].
more_id --> [","],id, more_id | [].

if_stmt --> ["if"], ["("], condition , [")"], stmt, optional_else.
if_stmt --> ["if"], ["("], condition, [")"],["{"], stmts, ["}"], optional_else.
optional_else --> ["else"], stmt | ["else"],["{"], stmts , ["}"] | [].

for_stmt --> ["for"], ["("],assign_stmt,condition,[";"],assign_stmt2,[")"],stmt.
for_stmt --> ["for"], ["("],assign_stmt,condition,[";"],assign_stmt2,[")"],["{"],stmts,["}"].

while_stmt --> ["while"], ["("],condition,[")"], stmt.
while_stmt --> ["while"], ["("],condition,[")"],["{"], stmts,["}"].

do_while_stmt --> ["do"],["{"],stmts,["}"],["while"],["("],condition,[")"],[";"].
cin_cout_stmt --> ["cin"] , [">"], [">"],id,[";"] | ["cout"] , ["<"] , ["<"] , id,[";"].

return_stmt --> ["return"],expr , [";"] | ["return"] , [";"] .

id --> [X], { atom_string( Atom, X ),atom(Atom), not(memberchk(X,["main","for","if","else","true","false","char","int","double","String","void","{","}","(",")","[","]"])) }.
num --> [N], { atom_number(N,X) ,number(X) }.

expr --> term.
expr --> term, add_sub, expr.

term --> factor.
term --> factor, mul_div, term.

factor --> num | id.
factor --> ["("], expr, [")"].

add_sub --> [Op], { memberchk(Op, ["+", "-"]) }.
mul_div --> [Op], { memberchk(Op, ["*", "/"]) }.
add_sub_mul_div --> [Op], { memberchk(Op, ["+", "-","*", "/"]) }.

condition --> cond , optional_complex_cond.
cond --> id, cond_op , expr | ["("], id, cond_op , expr , [")"].
optional_complex_cond --> logic_op , cond | [].
% condition --> condition, logic_op, condition. 
cond_op --> ["<"] | [">"] | ["<"],["="] | [">"],["="] | ["="],["="] | ["!"],["="]. %% split atoms here
logic_op --> ["&"] | ["|"] | ["&"],["&"] | ["|"],["|"].

parse(X):-
	string_manipulation(X,Atoms),
	phrase(parser,Atoms),
	write("\n\nsyntax free");
	write("\n\nsyntax error").

parse_from_file:-
 	read_file_to_string("c:\\code.txt",X,[]),
 	write("\n================== Source Code ==================\n"),
 	write(X),
 	string_manipulation(X,Atoms),
	phrase(parser,Atoms),
	write("\n\nsyntax free");
	write("\n\nsyntax error").

string_manipulation(Input,Output):-
	normalize_space(atom(S0), Input),
	replace_in_string("+"," + ",S0,S1),
	replace_in_string("-"," - ",S1,S2),
	replace_in_string("*"," * ",S2,S3),
	replace_in_string("/"," / ",S3,S4),
	replace_in_string("{"," { ",S4,S5),
	replace_in_string("}"," } ",S5,S6),
	replace_in_string("("," ( ",S6,S7),
	replace_in_string(")"," ) ",S7,S8),
	replace_in_string(";"," ; ",S8,S9),
	replace_in_string("|"," | ",S9,S10),
	replace_in_string("&"," & ",S10,S11),
	replace_in_string("="," = ",S11,S12),
	replace_in_string(","," , ",S12,S13),
	replace_in_string("<"," < ",S13,S14),
	replace_in_string(">"," > ",S14,S15),
	normalize_space(atom(SF), S15),
	%atomic_list_concat(Output," ",SF),
	write("\n================== tokenz list ==================\n"),
	write(SF),
	split_string(SF, " ", "",Output). %returns a list of strings splited by space
	


replace_in_string(Old_Char,New_Char,String,New_String):-
	atomic_list_concat(Atoms,Old_Char, String),
	atomic_list_concat(Atoms,New_Char, New_String).

% facts %
id(a).	id(b).	id(c).	id(d).	id(e).	id(f).	id(g).	id(h).
id(i).	id(j).	id(k).	id(l).	id(m).	id(a).	id(n).	id(o).
id(p).	id(q).	id(r).	id(s).	id(t).	id(u).	id(v).	id(w).
id(x).	id(y).	id(z).

digit("0").	digit("1").	digit("2").	digit("3").	digit("4").
digit("5").	digit("6").	digit("7").	digit("8").	digit("9").
