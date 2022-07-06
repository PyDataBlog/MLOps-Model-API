CSS only form element replacement for `checkbox` and `radio` `input` types.

Note that the `label` is immediately after the `input` as a sibling. The `label` must use a `for` attribute, and the `input` requires an `id`. This is better for accessibility anyways, and speaking of which, consider adding `aria-labelledby` to the `input` and an `id` to the `label`.
