---
 layout: page
 title:  "Search"
 date:   2016-02-15 21:20:43 +0800
---

<!-- Html Elements for Search -->
<div id="search-container">
    <div id="search-wrap" class="search-wrap">
        <input type="text" id="search-input" placeholder="what are you looking for...">
        <div class="hor">
            <hr>
            <hr class="dyn-line">
        </div>
    </div>


<ul id="results-container"></ul>
</div>

<!-- Script pointing to jekyll-search.js -->
<script src="{{ site.baseurl }}/assets/js/vendor/simple-jekyll-search/dest/jekyll-search.js" type="text/javascript"></script>
<script>
    SimpleJekyllSearch({
      searchInput: document.getElementById('search-input'),
      resultsContainer: document.getElementById('results-container'),
      json: '/search.json'
    })
</script>