# Cards
Componentized UI Architecture Using Knockout.js + Flux Patterns with TypeScript.

## Proposal

### Stores
```typescript
import {Store, StoreOf, listen} from 'cards';
import {CartService} from '../services/CartService';

@StoreOf(CartService)
class CartStore extends Store {
  
  cartCount = ko.observable(1).subscribe(this.emitChange);
  
  @listen
  addToCart(item) {
    this.getService(CartService).addToCart(item).then((data) => {
      this.cartCount(data.cartCount);
    });
  }
}
```

### Views
```typescript
import {View, ViewOf} from 'cards';
import {CartStore} from '../stores/CartStore';

@ViewOf(CartStore)
class AddToCartButton extends View {
  addToCart(productId) {
    this.emit('addToCart', {id: productId, quantity: 1});
    this.getStore(CartStore).watch((store) => {
      store.cartCount;
    });
  }
}

@ViewOf(CartStore)
class CartCount extends View {
  constructor() {
    this.getStore(CartStore).share(this);
  }
}
```

### Knockout
```html
<div data-view="CartCount">
  <span data-bind="text: cartCount">0</span>
</div>
```

```html
<div data-view="AddToCartButton">
  <button data-bind="click: addToCart.bind(1234)">Add To Cart</button>
</div>
```
