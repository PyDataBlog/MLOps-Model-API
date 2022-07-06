DROP TABLE IF EXISTS product_cart;

CREATE TABLE product_cart (
  product_id INTEGER REFERENCES product(id),
  cart_id INTEGER REFERENCES cart(id)
);
