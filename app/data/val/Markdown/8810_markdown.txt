---
title: "Release notes 1.2.0"
date: 2017-04-07
draft: false
weight: 270
---

Avola 1.2-20170406.4 was released on April 7th, 2017.

##### Features

* You can now manage your domain's knowledge sources. Navigate to "Domains" and find the new tab. You can attach your source documents, limited to 50MB per document. Contact us if this limit is a problem for your organisation.
* It is now possible to create value lists and pairs when you are creating the business data element that uses it, also directly from the decision logic. On top of that, value list items can be added directly when editing a decision logic condition cell value. 
* Percentages are no longer handled as their decimal values, which makes it a lot easier to work with them in the user interface. The API still expects (and returns) the value as the fraction. This means 10.75% is 0.1075 in the API.
* The Execution Result in the API has been extended with an extra field (FinalConclusionBusinessDataIds) to mark the main conclusions. It is an array because we plan to support multiple conclusion business data elements per decision (#185)

##### Enhancements / Changes

* performance enhancements in test execution for individual scenarios
* Decisions with pair conclusions now return the actual pair value instead of the corresponding boolean value. (#186)
* Better performance when uploading large scenario collection excel files. (#187, #190)
* API now accepts date without time when this is not required by the business data precision.
* Edit your date values in the decision logic, without opening the edit popup
* In the row expression that is passed to the API in the execution result, we now omit the "any..." parts. When a value is not tested, it is not mentioned anymore.
* Table headers in decision logic now have extra infomation about constraints and precision of the corresponding business data element
* Stricter handling of business data editing possibilities. (#197)
* Better error handling when it is not allowed to delete a business data element (#194)
* Business data questions in a test scenario are now grouped by business concept.
* The Administrator and Security Administrator role are merged into the administrator role

##### Fixes

* Don't allow empty date values in decision logic
* Correct handling of empty datetime values in scenario collection excel uploads 
* Don't allow saving a text operator when the operand is not defined
* It is no longer possible to save a condition with a "between" operator, without entering the "until" value.
* Several small but not breaking display issues in Firefox and Internet Explorer

##### Known Issues

* Some older (pre 1.1) scenario's, with date values, can not be cloned. please re-create these scenarios
* Scenarios with pair conclusions will not behave correctly until saved again (changed conclusion value)
* We are aware that the execution of very large scenario collections does not perform well, and can result in confusing time-out errors.
* The stricter handling of editing Business Data Elements removes some functionalities (like editing decimal places for a decimal value). We are working on audit trail (history) of business data edits that have an effect on Decision Services. We will release full editing of Business Data Elements with this feature.
