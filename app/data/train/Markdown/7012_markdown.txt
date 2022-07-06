# selector-no-qualifying-type

Disallow qualify a selector by type.

禁止使用类型对选择器进行限制。

```css
    a.foo {}
/** ↑
 * This type selector is qualifying the class */
```

A type selector is "qualifying" when it is compounded with (chained to) another selector (e.g. a.foo, a#foo). This rule does not regulate type selectors that are combined with other selectors via a combinator (e.g. a > .foo, a #foo).

类型选择器与其他选择器复合使用时，具有限制作用 (如，a.foo，a#foo)。该规则不会检查通过复合选择符组合使用的选择器。

## Options

### `true`

The following patterns are considered warnings:

以下模式被认为是个警告：

```css
a.foo {
  margin: 0
}
```

```css
a#foo {
  margin: 0
}
```

```css
input[type='button'] {
  margin: 0
}
```

The following patterns are *not* considered warnings:

以下模式正常：

```css
.foo {
  margin: 0
}
```

```css
#foo {
  margin: 0
}
```

```css
input {
  margin: 0
}
```

## Optional secondary options

### `ignore: ["attribute", "class", "id"]`

#### `"attribute"`

Allow attribute selectors qualified by type.

允许使用类型选择器对特性选择器进行限制。

The following patterns are *not* considered warnings:

以下模式被认为是个警告：

```css
input[type='button'] {
  margin: 0
}
```

#### `"class"`

Allow class selectors qualified by type.

允许使用类型选择器对类选择器进行限制。

The following patterns are *not* considered warnings:

以下模式正常：

```css
a.foo {
  margin: 0
}
```

#### `"id"`

Allow id selectors qualified by type.

允许使用类型选择器对 id 选择器进行限制。

The following patterns are *not* considered warnings:

以下模式正常：

```css
a#foo {
  margin: 0
}
```
