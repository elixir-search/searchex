-module(async_bm25).

%% API
-export([document_scores/3]).

%% These variables are expected for a proper Okapi BM25 implementation,
%% but for this demo it doesn't matter much if kept constant
-define(TOTAL_DOCUMENTS, 100).
-define(AVERAGE_DOCUMENT_TOKEN_LENGTH, 100).
-define(DOCUMENT_TOKEN_LENGTH, 100).

%% See the Zope implementation description for a discussion of these tuning constants.
-define(TERM_FREQUENCE_TUNING_FACTOR, 1.2).
-define(DOCUMENT_LENGTH_TUNING_PARAM, 0.72).

document_scores(Terms, DocumentMatches, MatchesPerTermAndDocument) ->
    lists:reverse(
      lists:keysort(2, [{DocumentId, document_score(Terms,
                                                    DocumentId,
                                                    DocumentMatches,
                                                    MatchesPerTermAndDocument,
                                                    ?AVERAGE_DOCUMENT_TOKEN_LENGTH,
                                                    ?TOTAL_DOCUMENTS)}
                        || DocumentId <- unique_document_ids(DocumentMatches)])).

document_score(Terms, DocumentId, DocumentMatches, MatchesPerTermAndDocument, AverageDocumentLength, TotalNumberOfDocuments) ->
    TermScores = lists:map( fun( Term ) ->
				    MatchCount =
					case dict:find({Term, DocumentId}, MatchesPerTermAndDocument) of
					    {ok, Count} -> Count;
					    _ -> 0
					end,
				    term_document_score(
				      MatchCount,
				      number_of_documents_with_term( Term, DocumentMatches),
				      avarage_document_length( ?DOCUMENT_TOKEN_LENGTH, AverageDocumentLength),
				      TotalNumberOfDocuments)
			    end,
			    Terms
			  ),
    lists:sum(TermScores).

term_document_score(NumberOfTimesTermAppearsInDocument, NumberOfDocumentsWithTerm, AverageDocumentLength, TotalNumberOfDocuments) ->
    TermFrequency = term_frequency( NumberOfTimesTermAppearsInDocument, AverageDocumentLength),
    Idf = inverse_document_frequency_of_term( TotalNumberOfDocuments, NumberOfDocumentsWithTerm ),
    TermFrequency * Idf.

unique_document_ids( DocumentMatches ) ->
    {_, DocumentIds} = lists:unzip(DocumentMatches),
    lists:usort(lists:flatten(DocumentIds)).

number_of_documents_with_term( Term, DocumentMatches) ->
    case lists:keyfind( Term, 1, DocumentMatches) of
	{ _, DocIds} ->
	    length(DocIds);
	false ->
	    0
    end.

inverse_document_frequency_of_term( _TotalNumberOfDocuments, 0) ->
    0;
inverse_document_frequency_of_term( TotalNumberOfDocuments, NumberOfDocumentsWithTerm) ->
    math:log(1.0 + TotalNumberOfDocuments / NumberOfDocumentsWithTerm).

term_frequency( 0, _AverageDocumentLength) ->
    0;
term_frequency( NumberTimesTermAppearsInDocument, AverageDocumentLength) ->
    (NumberTimesTermAppearsInDocument * ( ?TERM_FREQUENCE_TUNING_FACTOR + 1)) / (NumberTimesTermAppearsInDocument + AverageDocumentLength * ?TERM_FREQUENCE_TUNING_FACTOR ).

avarage_document_length( DocumentLengthInWords, AverageDocumentLenghtInWords) ->
    (1 - ?DOCUMENT_LENGTH_TUNING_PARAM) + ?DOCUMENT_LENGTH_TUNING_PARAM * DocumentLengthInWords/AverageDocumentLenghtInWords.
