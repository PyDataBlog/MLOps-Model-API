#### fake-storage
[![Build Status](https://travis-ci.org/stpettersens/node-fake-storage.svg?branch=master)](https://travis-ci.org/stpettersens/node-fake-storage) [![npm version](https://badge.fury.io/js/ssp-fake-storage.svg)](http://npmjs.org/package/ssp-fake-storage)  [![Development Dependency Status](https://david-dm.org/stpettersens/node-fake-storage/dev-status.png?theme=shields.io)](https://david-dm.org/stpettersens/node-fake-storage#info=devDependencies) 

Node.js module to simulate the browser's Storage object (e.g. localStorage).

Use from JavaScript:

```js
var localStorage = require('ssp-fake-storage');
localStorage.setItem('my_key', 'my_value');
localStorage.getItem('my_key'); ==> 'myvalue'
localStorage.key(0); ==> 'my_key'
```

Use from [TypeScript](http://www.typescriptlang.org):

```ts
/// <require path="typings/ssp-fake-storage.d.ts" />
import localStorage = require('ssp-fake-storage');
localStorage.setItem('my_key', 'my_value');
localStorage.getItem('my_key'); ==> 'myvalue'
localStorage.key(0); ==> 'my_key'
```

Methods:

**Storage.setItem(key: string, value: string): void**

Add an key-value pair to Storage.
