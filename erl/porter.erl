%
% porter.erl
%
% Porter Stemming Algorithm Implementation in Erlang  
%
% Alden Dima (alden.dima@nist.gov)
% National Institute of Standards and Technology,
% Gaithersburg, MD
% September, 2007
%
% This software was developed at the National Institute of Standards
% and Technology by employees of the Federal Government in the course
% of their official duties. Pursuant to title 17 Section 105 of the
% United States Code this software is not subject to copyright
% protection and is in the public domain. This software is an
% experimental system. NIST assumes no responsibility whatsoever for
% its use by other parties, and makes no guarantees, expressed or
% implied, about its quality, reliability, or any other characteristic.
% We would appreciate acknowledgement if the software is used.
%
% This software can be redistributed and/or modified freely provided
% that any derivative works bear some notice that they are derived
% from it, and any modified versions bear some notice that they have
% been modified.
%
%
% The basic philosophy behind this implementation is to map the
% structure of Martin Porter's original description of his algorithm
% (http://tartarus.org/~martin/PorterStemmer) as closely as possible
% into Erlang. I made liberal use of Erlang's pattern matching
% facility. In order for this to work, there is one quirk - the
% word to be matched must be reversed before it is stemmed: "hopping"
% becomes "gnippoh". This is necessary because Erlang's pattern
% matching won't work with Stem ++ "ing" but instead requires
% "gni" ++ Mets (where Mets is the backwards stem).  Despite this
% quirk, the flipping the words allowed me to greatly simplify the
% rest of the coding, almost to the point of being a tedious translation
% of the textual description of the algorithm.
%
% Thanks to Paul Black for his helpful comments.
%

-module(porter).

-export([stem/1, stem_file/2]).

-import(conditions, [measure/1, ends_with/2, has_vowel/1,
        ends_with_double_cons/1, ends_with_cvc/1]).

-import(lists, [reverse/1]).

stem(Word) when length(Word) > 2 ->
    reverse(step5(step4(step3(step2(step1(reverse(Word)))))));

stem(Word) -> Word.


stem_lines(S_in, S_out) ->
    L = io:get_line(S_in, ''),
    case L of
        eof -> eof;
        _ ->
            io:format(S_out, "~s~n", [stem(string:strip(L, right, $\n))]),
            stem_lines(S_in, S_out)
        end.


%
% This function serves as the "main" function - call it via the
% Erlang shell as: porter:stem_file("input_file", "output_file").
% where input_file and output_file are replaced with the appropriate
% local file names. Note: I assume that all words are already in
% lower case.
%
stem_file(In_file, Out_file) ->
    {ok, S_in} = file:open(In_file, read),
    {ok, S_out} = file:open(Out_file, write),
    stem_lines(S_in, S_out),
    file:close(S_in),
    file:close(S_out).


%%%%%%%%%%%%%
%% Step 1a %%
%%%%%%%%%%%%%

% Some notational goofiness on my part:
% "Mets" means "Stem" backwards, "Drow" means "Word".
% Given that a word can be processed by several rules and that for
% some of the rules what matches as the "stem" isn't the final result,
% I'm using the terms word and stem loosely, but I don't have anything
% better.

step1a("sess" ++ Mets) -> "ss" ++ Mets;

step1a("sei" ++ Mets) -> "i" ++ Mets;

step1a(Drow = "ss" ++ _) -> Drow;

step1a("s" ++ Mets) -> Mets;

step1a(Drow) -> Drow.


%%%%%%%%%%%%%
%% Step 1b %%
%%%%%%%%%%%%%

step1b(Drow = "dee" ++ Mets) ->
    case measure(Mets) of
        M when M > 0 ->
            "ee" ++ Mets;
        0 ->
            Drow
    end;

step1b(Drow = "de" ++ Mets) ->
    case has_vowel(Mets) of
        true -> 
            step1b2(Mets);
        false ->
            Drow
    end;

step1b(Drow = "gni" ++ Mets) ->
    case has_vowel(Mets) of
        true -> 
            step1b2(Mets);
        false ->
            Drow
    end;

step1b(Drow) -> Drow.


step1b2("ta" ++ Mets) -> "eta" ++ Mets;

step1b2("lb" ++ Mets) -> "elb" ++ Mets;

step1b2("zi" ++ Mets) -> "ezi" ++ Mets;

step1b2(Drow = [C, C | Mets]) ->
    case ends_with_double_cons(Drow)
        and not(ends_with($l, Drow)
             or ends_with($s, Drow)
             or ends_with($z, Drow)) of
        true ->
            [C] ++ Mets;
        false ->
            Drow
    end;

step1b2(Drow) ->
    M = measure(Drow),
    O = ends_with_cvc(Drow),
    if
        (M == 1) and O -> "e" ++ Drow;
        true -> Drow
    end.


%%%%%%%%%%%%%
%% Step 1c %%
%%%%%%%%%%%%%

step1c(Drow = "y" ++ Mets) ->
    case has_vowel(Mets) of
        true -> "i" ++ Mets;
        false -> Drow
    end;
    
step1c(Drow) -> Drow.


step1(Drow) ->
    step1c(step1b(step1a(Drow))).


%%%%%%%%%%%%
%% Step 2 %%
%%%%%%%%%%%%

m_repl(N, Drow, Mets, Repl) ->
    M = measure(Mets),
    if
        M > N -> Repl ++ Mets;
        true -> Drow
    end.

% To make the code less cluttered, I've used macros here and elsewhere
% to define function clauses that are structurally identical but that
% pattern match on differing terms.

-define(step2(MATCH, REPL),
        step2(Drow = MATCH ++ Mets) -> m_repl(0, Drow, Mets, REPL)).

?step2("lanoita", "eta");
?step2("lanoit", "noit");
?step2("icne", "ecne");
?step2("icna", "ecna");
?step2("rezi", "ezi");
?step2("ilb", "elb");   % replacement rule from website
?step2("illa", "la");
?step2("iltne", "tne");
?step2("ile", "e");
?step2("ilsuo", "suo");
?step2("noitazi", "ezi");
?step2("noita", "eta");
?step2("rota", "eta");
?step2("msila", "la");
?step2("ssenevi", "evi");
?step2("ssenluf", "luf");
?step2("ssensuo", "suo");
?step2("itila", "la");
?step2("itivi", "evi");
?step2("itilib", "elb");
?step2("igol", "gol");  % new rule from website

step2(Drow) -> Drow.


%%%%%%%%%%%%
%% Step 3 %%
%%%%%%%%%%%%

-define(step3(MATCH, REPL),
        step3(Drow = MATCH ++ Mets) -> m_repl(0, Drow, Mets, REPL)).

?step3("etaci", "ci");
?step3("evita", "");
?step3("ezila", "la");
?step3("itici", "ci");
?step3("laci", "ci");
?step3("luf", "");
?step3("ssen", "");

step3(Drow) -> Drow.


%%%%%%%%%%%%
%% Step 4 %%
%%%%%%%%%%%%

m_chop(Drow, Mets) ->
    M = measure(Mets),
    if
        M > 1 -> Mets;
        true -> Drow
    end.

-define(step4(MATCH),
        step4(Drow = MATCH ++ Mets) -> m_chop(Drow, Mets)).

?step4("la");
?step4("ecna");
?step4("ecne");
?step4("re");
?step4("ci");
?step4("elba");
?step4("elbi");
?step4("tna");
?step4("tneme");
?step4("tnem");
?step4("tne");

step4(Drow = "noi" ++ Mets) ->
    Cond = ends_with($s, Mets) orelse ends_with($t, Mets),
    if
        Cond -> m_chop(Drow, Mets);
        true -> Drow
    end;

?step4("uo");
?step4("msi");
?step4("eta");
?step4("iti");
?step4("suo");
?step4("evi");
?step4("ezi");

step4(Drow) -> Drow.


%%%%%%%%%%%%%
%% Step 5a %%
%%%%%%%%%%%%%

step5a(Drow = "e" ++ Mets) ->
    M = measure(Mets),
    O = ends_with_cvc(Mets),
    if
        M > 1 -> Mets;
        M == 1, not(O) -> Mets;
        true -> Drow
    end;

step5a(Drow) -> Drow.


%%%%%%%%%%%%%
%% Step 5b %%
%%%%%%%%%%%%%

step5b(Drow = "ll" ++ Mets) ->
    M = measure(Drow),
    if M > 1 -> "l" ++ Mets;
        true -> Drow
    end;

step5b(Drow) -> Drow.


step5(Drow) -> step5b(step5a(Drow)).

