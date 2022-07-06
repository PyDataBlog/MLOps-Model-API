use bloggity ;
-- Delete old data
delete from Manages ;
delete from Pages ;
delete from Posts ;
delete from Users ;
delete from UserTypes ;

-- Populate test data
insert into UserTypes ( userType , description )
values
( 'Administrator' , 'Administrators manage all users, pages, and bloggity posts.' ) ,
( 'Editor' , 'Editors manage pages and posts.' )
;
insert into Users ( userID , password , userType , firstName , lastName , email )
values
( 'atheiman' , 'atheimanpass' , 'Administrator' , 'Austin' , 'Heiman' , 'austin@example.com' ) ,
( 'taylor' , 'taylorpass' , 'Editor' , 'Taylor' , 'Hogan' , 'taylor@example.org' ) ,
( 'steven' , 'stevenpass' , 'Editor' , 'Steven' , 'Mason' , 'steven@example.com' )
;
insert into Posts ( userID , postTitle , postContent )
values
( 'atheiman' , 'First Post' , 'This is the first post to the bloggity! Posting is easy, just navigate to the admin page and post away!' ) ,
( 'taylor' , 'Taylor''s First Post' , 'Hi there, this is an example of a post by an Editor. Enjoy using Bloggity!' ) ,
( 'steven' , 'Steven Posted Too' , 'Wow, posting on Bloggity sure is easy! Look at all these posts!' ) ,
( 'atheiman' , 'Austin posts again' , 'The posts continue to roll thru Bloggity. This is the coolest computer software I have ever used!' ) ,
( 'taylor' , 'You hit the keys really hard.' , 'This is freaking awesome! I can''t believe posting to my hashtag Bloggity is so easy!' )
;
insert into Pages ( pageTitle , pageContent )
values
( 'Test Page' , 'This page was created as a test. Hopefully you will be able to add your own html to this some day. This content will then be extremely configurable! You will notice that this page is only managed by taylor, yet atheiman can edit because he is administrator. steven cannot edit because he does not manage this page. atheiman  or another administrator will have to add him to the manage list or this page' )
;
insert into Manages ( userID , pageTitle )
values
( 'taylor' , 'Test Page' )
;
