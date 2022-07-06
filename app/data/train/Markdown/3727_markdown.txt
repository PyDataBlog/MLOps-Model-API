accessible-custom-form-controls
====================

Accessible Custom Form Controls with full keyboard support and proper accessibility roles/attributes/properties

## Custom Checkboxes
 - each custom checkbox receives the following roles/attributes/properties:
   - `role="checkbox`
   - `aria-labelledby="the-id-of-the-label-element"`
   - `aria-checked="true/false"`
   - `aria-setsize="(the amount of checkboxes in the group)"`
   - `aria-posinset="(the current index of the given checkbox"`
   - `

## Custom Radios
 - The group (`<ul />`) receives:
   - `role="radiogroup"`
   - `aria-required="true"`
   - `aria-labelledby="the-id-of-the-common-label"`
 - Each custom radio receives:
   - `role="radio"`
   - `aria-labelledby="the-id-of-the-label-element"`
   - `aria-checked="true"` when the radio is selected
   - `tabindex="0"` when the radio is selected
   - `aria-checked="false"` when the radio is NOT selected
   - `tabindex="-1"` when the radio is NOT selected
