%
% conditions.erl
%
% Porter Stemming Algorithm Implementation in Erlang  
%
% Alden Dima (alden.dima@nist.gov)
% National Institute of Standards and Technology,
% Gaithersburg, MD
% September, 2007
%
% May, 2009 - Fixed ends_with/2 to prevent raising an exception when
%             the stem is empty - AD.
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
% This file implements the conditions found in the conditions part
% of the Porter Stemming algorithm.
%

-module(conditions).

-export([measure/1, ends_with/2, has_vowel/1, ends_with_double_cons/1,
        ends_with_cvc/1]).
        
-import(lists, [reverse/1, member/2]).

%
% Utility function to cut down on different sections code testing for vowels.
%
is_vowel(L, Y_is_vowel) ->
    case L of
        $a -> true;
        $e -> true;
        $i -> true;
        $o -> true;
        $u -> true;
        $y when Y_is_vowel -> true;
        _  -> false
    end.

%
% Implements m, the measure of a word or word part.
%
measure(Drow) -> measure(reverse(Drow), 0).


measure([], 0) -> 0;

measure([H|T], 0) ->
    case is_vowel(H, false) of
        true  -> found_vowel(T, 0);
        false -> found_leading_consonant(T)
    end.


found_leading_consonant([]) -> 0;

found_leading_consonant([H|T]) ->
    case is_vowel(H, true) of
        true  -> found_vowel(T, 0);
        false -> found_leading_consonant(T)
    end.


found_vowel([], M) -> M;

found_vowel([H|T], M) ->
    case is_vowel(H, false) of
        true  -> found_vowel(T, M);
        false -> found_consonant(T, M + 1)
    end.


found_consonant([], M) -> M;

found_consonant([H|T], M) ->
    case is_vowel(H, true) of
        true  -> found_vowel(T, M);
        false -> found_consonant(T, M)
    end.


%
% Implements *S - the stem ends with "s" (and similarly for other letters).
%
ends_with(_, [])   -> false;
ends_with(L, Mets) -> L == hd(Mets).


%
% Implements *v* - the stem contains a vowel
%
has_vowel(Mets) -> has_vowel1(reverse(Mets)).


has_vowel1([]) -> false;

has_vowel1([H|T]) ->
    case is_vowel(H, false) of
        true  -> true;
        false -> has_vowel2(T)
    end.


has_vowel2([]) -> false;

has_vowel2([H|T]) ->
    case is_vowel(H, true) of
        true  -> true;
        false -> has_vowel2(T)
    end.


%
% Implements *d - the stem ends with a double consonant.
%
ends_with_double_cons([C, C|_]) -> not is_vowel(C, true);
ends_with_double_cons(_)        -> false.


%
% Implements *o - the stem ends cvc, where the second c is not w, x, or y.
%
ends_with_cvc([C2, V, C1|_]) ->
    (not is_vowel(C1, false))
    andalso is_vowel(V, true)
    andalso not (is_vowel(C2, true) orelse member(C2, [$w, $x]));

ends_with_cvc(_) -> false.

