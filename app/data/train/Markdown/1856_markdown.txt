
# Router

Provides the ability to perform actions based on changes in the URL hash.

```javascript
import {Router} from "estoolbox";

Router.when("/about", () => {
  // Show the about route content.
});

Router.when("/person/:username", (route) => {
  const username = route.variables.username;
  // Show the person content for this user.
});
```
