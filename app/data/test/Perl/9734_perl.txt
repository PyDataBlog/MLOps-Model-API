:- module(media_cache, []).

:- use_module(library(http/url_cache)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(thumbnail)).

:- http_handler(root(cache/original),
		http_original,  [spawn(media)]).
:- http_handler(root(cache/thumbnail),
		http_thumbnail,  [spawn(media)]).
:- http_handler(root(cache/medium),
		http_mediumscale,  [spawn(media)]).
:- http_handler(root(cache/fit),
		http_fit_thumbnail,  [spawn(media)]).
:- http_handler(root(cache/fitmedium),
		http_medium_fit,  [spawn(media)]).

%%      original(+Request)
%
%       HTTP handler providing original content for a URI. Used together
%       with PicturePoint to avoid  Java   security  issues. Also caches
%       results from our upstream (image) providers such as Artchive and
%       and the musea.

http_original(Request) :-
        http_parameters(Request,
                        [ uri(URI0, [description('URI of the original image')])
                        ]),
        map_uri(URI0, URI),
        url_cache(URI, File, MimeType),
        debug(url_cache, 'Original for ~w (~w)', [URI,MimeType]),
        throw(http_reply(file(MimeType, File))).

http_thumbnail(R)  :- do_http_thumbnail(thumbnail_size, R).
http_mediumscale(R):- do_http_thumbnail(medium_size, R).

do_http_thumbnail(Size, Request) :-
        http_parameters(Request,
                        [ uri(URI, [])
                        ]),
        debug(thumbnail, 'Thumbnail for ~w', [URI]),
        uri_thumbnail(URI, ThumbnailFile, Size),
        http_reply_file(ThumbnailFile, [unsafe(true)], Request).

/* http_fit_thumbnail(+Request)
* provides a fitted version of the image adhering given dimensions
*/
http_fit_thumbnail(Request)  :- do_http_fit(thumbnail_size, Request).
http_medium_fit(Request)  :- do_http_fit(medium_size, Request).

do_http_fit(Size, Request) :-
        http_parameters(Request,
                        [ uri(URI, [])
                        ]),
        debug(thumbnail, 'Do fit for ~w', [URI]),
        uri_fit_thumbnail(URI, ThumbnailFile, Size),
        http_reply_file(ThumbnailFile, [unsafe(true)], Request).


%%	map_uri(+URIin, -URIout) is det.
%
%	Hook to map media URIs to different URIs to work around known
%	problems (e.g. images that are known to be wrong).
%
map_uri(U,U).



