if(!org) var org={};
if(!org.judison) org.judison={};
if(!org.judison.bmsp) org.judison.bmsp={};

with(org.judison.bmsp){

  init = function(){
    document.getElementById("bookmarks-view").place = "place:queryType=1&folder=" + window.top.PlacesUtils.bookmarksMenuFolderId;
  }

}