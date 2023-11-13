%{
  let rec range a b = if a > b then [] else a::range (a+1) b
%}

%token S
%token B
%token SLASH
%token E
%token COMMA
%token <string> NUM
%token RANGE
%token EOF

%start <Rule.rule> rule

%%

num :
| n = NUM { int_of_string n }

range :
| a = num; RANGE; b = num { range a b }

num_or_range :
| n = num { [ n ] }
| ns = range { ns }

rule :
| S; sl = list(num); SLASH; B; bl = list(num); EOF { (sl,bl) }
| E; option(S); sl = separated_list(COMMA, num_or_range); SLASH; option(B); bl = separated_list(COMMA, num_or_range); EOF { (List.flatten sl, List.flatten bl) }