# fusion-table
A Polymer 1.0 component for interacting with Google Fusion Tables.

This is a behind-the-scene component to provide methods to communicate with Google's Fusion Tables API. It requires Polymer 1.0 and a `google-signin` component be added somewhere on the page. On signing success, this component will attach the Authorize header to local `iron-ajax` components and provide methods to interact with those componnets.

## Ready

Once the Authorization header has been added to the `iron-ajax` elements, this component will fire a `fusion-table-ready` event. It is best to make your first query after that event has fired.

## Methods

### GET

The GET method sends an authenticated GET request to Google, and fires a `fusion-table-get` event on success with the response being the detail payload. It has only one paramter, the SQL to run. In the event of an error a `fusion-table-error` event will be fired.

### POST

The POST methos sends an authenticated POST request to Google, and fires a `fusion-table-post` event on success with the response being the detail payload. It has only one paramter, the SQL to run. In the event of an error a `fusion-table-error` event will be fired.

### format

A small printf variant to help formatting the SQL queries. It matches regular expresssions in a string to index items in a following array.

**Example:**

```
this.format('select * from {0} where ROWID = '{1}', [{TABLE_ID}, {ROWID}]);
```

will return .. 

```
select * from {TABLE_ID} where ROWID = '{ROW_ID}'
```

### toNestedObject

This function helps turn flat object, like those created by serializing forms, and returns a nested object by the path of each key.

**Example:**

Starting with an html form ...

```
<input type="text" name="name.first">
<input type="text" name="name.last">
```

Serializing this form with Polymer's help will return this ...

```
foo: {
  'name.first': {VALUE},
  'name.last': {VALUE}
}
```

Running that return through the `.toNestedObject(foo)` function will return ...

```
foo: {
  name: {
    first: {VALUE},
    last: {VALUE}
  }
}
```

This comes in handy when storing complex object as strigified JSON.

## Notes:

Depending on what you are putting into the system, you may need to encode characters and then decode them back. This component *does not* do that for you. For a good encoding library, I recommend [the he library](https://github.com/mathiasbynens/he).
