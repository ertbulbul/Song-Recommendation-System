% ertuğrul bülbül
% 2016400219
% compiling: yes
% complete: yes

:- [artists, albums, tracks].


% artist(ArtistName, Genres, AlbumIds).
% album(AlbumId, AlbumName, ArtistNames, TrackIds).
% track(TrackId, TrackName, ArtistNames, AlbumName, [Explicit, Danceability, Energy,
%                                                    Key, Loudness, Mode, Speechiness,
%                                                    Acousticness, Instrumentalness, Liveness,
%                                                    Valence, Tempo, DurationMs, TimeSignature]).

features([explicit-0, danceability-1, energy-1,
          key-0, loudness-0, mode-1, speechiness-1,
       	  acousticness-1, instrumentalness-1,
          liveness-1, valence-1, tempo-0, duration_ms-0,
          time_signature-0]).

		  

filter_features(Features, Filtered) :- features(X), filter_features_rec(Features, X, Filtered).
filter_features_rec([], [], []).
filter_features_rec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filter_features_rec(FeatTail, Tail, FilteredTail),
    _-Use = Head,
    (
        (Use is 1, FilteredFeatures = [FeatHead|FilteredTail]);
        (Use is 0,
            FilteredFeatures = FilteredTail
        )
    ).
	

% getArtistTracks(+ArtistName, -TrackIds, -TrackNames) 5 points
getArtistTracks(ArtistName,TrackIds,TrackNames) :-
	artist(ArtistName, _, AlbumIds),
	findall(TrackId, (member(X1,AlbumIds),album(X1,_,_,X2),member(TrackId,X2)), TrackIds),
	findall(TrackName, (member(X3,TrackIds), track(X3,TrackName,_,_,_)), TrackNames).
	


	
% albumFeatures(+AlbumId, -AlbumFeatures) 5 points
albumFeatures(AlbumId, AlbumFeatures ) :-
	album(AlbumId,_,_,TrackIds),
	findall(Feature, (member(X1,TrackIds),track(X1,_,_,_,Feature)),Features),
	findall(Filtered,(member(X4,Features),filter_features(X4,Filtered)),Filtereds),
	averageCalculator(Filtereds, AlbumFeatures).


% artistFeatures(+ArtistName, -ArtistFeatures) 5 points
artistFeatures(ArtistName, ArtistFeatures) :-
	getArtistTracks(ArtistName,TrackIds,_),
	findall(Feature, (member(X1,TrackIds),track(X1,_,_,_,Feature)),Features),
	findall(Filtered,(member(X4,Features),filter_features(X4,Filtered)),Filtereds),
	averageCalculator(Filtereds, ArtistFeatures).
	



% trackDistance(+TrackId1, +TrackId2, -Score) 5 points
trackDistance(TrackId1, TrackId2, Score) :-
	track(TrackId1,_,_,_,FeatureTrack1),
	track(TrackId2,_,_,_,FeatureTrack2),
	filter_features(FeatureTrack1,Filtered1),
	filter_features(FeatureTrack2,Filtered2),
	elementWiseSubtract(Filtered1,Filtered2,FeatureResult),
	elementWiseSquare(FeatureResult,SquareResult),
	sumList(SquareResult,SumResult),
	Score is sqrt(SumResult).
	
	



% albumDistance(+AlbumId1, +AlbumId2, -Score) 5 points
albumDistance(AlbumId1, AlbumId2, Score) :-
	albumFeatures(AlbumId1, Feature1),
	albumFeatures(AlbumId2, Feature2),
	elementWiseSubtract(Feature1,Feature2,FeatureResult),
	elementWiseSquare(FeatureResult,SquareResult),
	sumList(SquareResult,SumResult),
	Score is sqrt(SumResult).
	
	
% artistDistance(+ArtistName1, +ArtistName2, -Score) 5 points
artistDistance(ArtistName1, ArtistName2, Score) :-
	artistFeatures(ArtistName1,Feature1),
	artistFeatures(ArtistName2,Feature2),
	elementWiseSubtract(Feature1,Feature2,SubtractResult),
	elementWiseSquare(SubtractResult,SquareResult),
	sumList(SquareResult,SumResult),
	Score is sqrt(SumResult).

% findMostSimilarTracks(+TrackId, -SimilarIds, -SimilarNames) 10 points
findMostSimilarTracks(TrackId, SimilarIds, SimilarNames) :-
	findall(Track, ((track(X1,_,_,_,_),X1 \= TrackId), trackDistance(TrackId,X1,Score),Track = Score-X1 ), Tracks),
	sort(0, @=<, Tracks,  ScoreAndIds),
	take(30, ScoreAndIds, FirstThirty),
	findall(Id,(member(X1,FirstThirty), _-Id = X1 ),SimilarIds),
	findall(Name,(member(Id,SimilarIds),track(Id,Name,_,_,_)),SimilarNames).
	
	
	
% findMostSimilarAlbums(+AlbumId, -SimilarIds, -SimilarNames) 10 points
findMostSimilarAlbums(AlbumId, SimilarIds, SimilarNames) :-
	findall(Album, ((album(X1,_,_,_),X1 \= AlbumId), albumDistance(AlbumId,X1,Score),Album = Score-X1 ), Albums),
	sort(0, @=<, Albums,  ScoreAndIds),
	take(30, ScoreAndIds, FirstThirty),
	findall(Id,(member(X1,FirstThirty), _-Id = X1 ),SimilarIds),
	findall(Name,(member(Id,SimilarIds),album(Id,Name,_,_)),SimilarNames).

% findMostSimilarArtists(+ArtistName, -SimilarArtists) 10 points
findMostSimilarArtists(ArtistName, SimilarArtists) :-
	findall(Artist, ((artist(X1,_,_),X1 \= ArtistName), artistDistance(ArtistName,X1,Score),Artist = Score-X1 ), Artists),
	sort(0, @=<, Artists,  AllSimilars),
	take(30,AllSimilars,FirstThirty),
	findall(Artist,(member(X1,FirstThirty), _-Artist = X1 ),SimilarArtists).

% filterExplicitTracks(+TrackList, -FilteredTracks) 5 points
filterExplicitTracks(TrackList, FilteredTracks) :-
	findall(Track, 
	(member(X1,TrackList),track(X1,_,_,_,X2),[H|_] = X2,H = 0 , Track=X1) ,
	FilteredTracks).
	


% getTrackGenre(+TrackId, -Genres) 5 points
getTrackGenre(TrackId, Genres) :-
	track(TrackId, _, ArtistNames, _, _),
	findall(Genre,(member(Artist,ArtistNames),artist(Artist, Genre,_ )), GenresList),
	flatten2(GenresList,Genres).

% discoverPlaylist(+LikedGenres, +DislikedGenres, +Features, +FileName, -Playlist) 30 points
discoverPlaylist(LikedGenres, DislikedGenres, Features, FileName, Playlist) :-
	findall(Track,(track(Track,_,_,_,_), getTrackGenre(Track,Genres), checkGenreList(Genres,LikedGenres)),LikedTracks),
	findall(Track,( member(Track,LikedTracks),track(Track,_,_,_,_),getTrackGenre(Track,Genres), checkGenreList2(Genres,DislikedGenres)),LikedMinusDislikedTracks),
	findall(Track,(member(X5,LikedMinusDislikedTracks),trackFeatureDistance(X5,Features,Score), Track = Score-X5),DistanceCalculated),
	sort(0, @=<, DistanceCalculated,  AllSimilars),
	take(30,AllSimilars,ScoreAndID),
	findall(Id,(member(A,ScoreAndID), _-Id = A),Playlist),
	findall(Distance,(member(B,ScoreAndID), Distance-_ = B),Distances),
	findall(TrackName,(member(C,Playlist),track(C,TrackName,_,_,_)),TrackNames),
	findall(Artist,(member(D,Playlist),track(D,_,Artist,_,_)),Artists),
	flatten2(Artists,Resu),
	open(FileName,write,OS),
	write(OS,Playlist),write(OS,"\n"),
	write(OS,TrackNames), write(OS,"\n"),
	write(OS,Resu), write(OS,"\n"),
	write(OS,Distances), write(OS,"\n"),
	close(OS).
	
	
% checkGenreList(+TrackGenre,+LikedGenre)
checkGenreList(TrackGenre,LikedGenre):-
	findall(Genre,(member(X,LikedGenre),member(Genre,TrackGenre),sub_string(Genre,_,_,_,X)),Genres),
	length(Genres,Z),
	Z > 0.
% checkGenreList2(+TrackGenre,+LikedGenre)	
checkGenreList2(TrackGenre,LikedGenre):-
	findall(Genre,(member(X,LikedGenre),member(Genre,TrackGenre),sub_string(Genre,_,_,_,X)),Genres),
	length(Genres,Z),
	Z = 0.
	


% trackFeatureDistance(+TrackId, +Feature, -Score)
% Takes Track id and feature list then calculates distances between them.
trackFeatureDistance(TrackId, Feature, Score) :-
	track(TrackId,_,_,_,FeatureTrack1),
	filter_features(FeatureTrack1,Filtered1),
	elementWiseSubtract(Filtered1,Feature,FeatureResult),
	elementWiseSquare(FeatureResult,SquareResult),
	sumList(SquareResult,SumResult),
	Score is sqrt(SumResult).	


% averageCalculator(+L, -AvgL)
% This method takes a list in list then calculates averages of this lists as element wise.
averageCalculator(L, AvgL) :-
    length(L,N),
    reduceWithSum(L,SL),
    N > 0, 
    elementWiseDivision(SL,N,AvgL).

% elementWiseDivision(+List, +D, -Result) :-
% This method takes a list and a number than divide all the elements in the list with that number.
elementWiseDivision([], _, []) :- !.
elementWiseDivision([H|R], D, [DH|DR]) :-
    !,
    DH is H / D,
    elementWiseDivision(R,D,DR).
 
% reduceWithSum(+List, -SL)
% This method takes list in the list than sum all the lists in the list as element wise.
reduceWithSum([L1], L1) :- !.
reduceWithSum([L1, L2| R], SL) :-
    !,
    elementWiseSum(L1, L2, S),
    append(R, [S], T),
    reduceWithSum(T, SL).
   
% elementWiseSum(+List1,+List2,-Result)
% Takes two list and make sum operation on the list as element wise then return a new list from results.
elementWiseSum([],[],[]) :- !.
elementWiseSum([H1|R1], [H2|R2], [HS|RS]) :-
    !,
    HS is H1 + H2,
    elementWiseSum(R1,R2,RS).

% elementWiseSubtract(+List1,+List2,-Result)
% Takes two list and make subtraction operation on the list as element wise then return a new list from results.
elementWiseSubtract([],[],[]) :- !.
elementWiseSubtract([H1|R1], [H2|R2], [HS|RS]) :-
    !,
    HS is H1 - H2,
    elementWiseSubtract(R1,R2,RS). 
	
% elementWiseSquare(+List1,-Result)
% This method takes a list then take square of all the elements in the list then make a new list from that items.
elementWiseSquare([],[]) :- !.
elementWiseSquare([H| T],[HR|TR]) :-
	!,
	HR is H * H,
    elementWiseSquare(T,TR).

% sumList(+List,-Sum)
% This methods takes a list then sum all of the elements in the list.
sumList([], 0).
sumList([H|T], Sum) :-
   sumList(T, Rest),
   Sum is H + Rest.
 
% flatten2(+List1,-List2)
% This method takes a list in list then concatenate this lists as a list then return the list.
flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

% take(+N,+List,-Front)
% This method takes a list and a number then return the first N number of the list as a list.
take(N, List, Front):- length(Front, N), append(Front, _, List).


