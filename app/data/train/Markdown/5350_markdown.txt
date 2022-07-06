object-filter_one
=======

returns first element that pass the test implemented by the provided function.


```javascript
var objectFilterOne = require("@nathanfaucett/object-filter_one");


objectFilterOne({
        a: "a",
        b: "b",
        c: "c"
    },
    function(value, key) {
        return key === "b";
    }
) === "b";
```
