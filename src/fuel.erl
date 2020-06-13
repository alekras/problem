%% @author alexei
%% @doc @todo Add description to fuel.


-module(fuel).

%% ====================================================================
%% API functions
%% ====================================================================
-export([f/0, f1/0, calc_fuel/1, total_fuel/1]).

f() ->
	lists:sum([calc_fuel(Weight) || Weight <- modules()]).

f1() ->
	lists:sum([total_fuel(Weight) || Weight <- modules()]).

calc_fuel(Mass) ->
	(Mass div 3) - 2.

total_fuel(ModuleMass) ->
	Primary_Fuel = calc_fuel(ModuleMass),
	tot_fuel(Primary_Fuel, Primary_Fuel).

%% ====================================================================
%% Internal functions
%% ====================================================================

tot_fuel(Fuel, TF) ->
	Step = calc_fuel(Fuel),
	if Step > 0 -> tot_fuel(Step, Step + TF);
		 true -> TF
	end.

modules() ->
	[
131787,
116597,
71331,
101986,
56538,
105039,
119405,
87762,
113957,
69613,
63698,
117674,
72876,
105026,
83620,
132592,
137403,
96832,
58387,
97609,
50978,
52896,
145584,
140832,
74504,
52998,
64722,
143334,
89601,
89326,
85906,
117840,
91299,
50593,
74470,
141591,
61069,
130479,
69195,
77411,
106137,
80954,
117644,
113063,
127587,
148770,
71286,
123430,
133562,
121053,
64311,
52818,
148583,
107511,
92838,
79724,
122022,
122602,
50344,
56938,
102363,
123140,
105469,
72773,
96023,
53669,
70394,
100930,
55213,
53756,
62225,
57172,
56049,
64661,
112321,
59872,
111597,
115958,
105468,
62111,
72865,
80323,
103897,
137687,
70178,
113314,
122121,
128654,
136723,
77279,
104806,
103491,
92168,
119263,
128791,
102237,
86578,
92728,
104785,
116658
].
