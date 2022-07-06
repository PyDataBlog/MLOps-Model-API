#####Use Case ID: UC-PAS07
#####Use Case Name: Verify Identification Document Validity

**Level:**                     User Level Goal

**Primary Actors:**            Clerk

**Stakeholders:**              Clerk, Person, Healthcare Provider

**Purpose:**                   The main intent of this use case is to verify the identification document validity.

**Pre Condition:**             Clerk must be identified and authenticated. 

**Post Condition:**            Verification of person’s identification document will be successfully validated.

**Frequency of Occurrence:**   Medium(Hourly)
__________________________________________________________
**Main Success Scenario: (Verify Identification Document Validity)**

1. Clerk requests verification of document validity.
2. System asks user to provide the identification document and the identification number of the verification document (i.e NP3473881).
3. Clerk providers the document and identification number and submits it.
4. System validates the document and indicates that the document is valid.

_______________________________________________________________________________
**Alternate Flows** 

**Alt-1:**

1. Identification number not valid.
2. Document not valid.

________________________________________________________________________
**Open Issues:**

Type of identification documents to be supported by PAS.

________________________________________________________________________
**Reference Hl7 V3 Interaction Identifiers (Domain: Patient Administration):**

PRPA_ST900101UV02
_______________________________________________________________
**Reference CCHIT Criteria:**

N/A

_______________________________________________________________
**Reference Hl7 RMIM (Domain: Patient Administration):**
[More Details](http://www.hl7.org/implement/standards/product_brief.cfm?product_id=306)

![identification-doc-hl7-rim](https://f.cloud.github.com/assets/5391320/1288116/88f4e302-3005-11e3-9678-698bcffed122.png)
_______________________________________________________________
**Reference FHIR Resource:**
[More Details](http://www.hl7.org/implement/standards/fhir/resourcelist.html)

![media-resource](https://f.cloud.github.com/assets/5391320/1288070/a09a5326-3004-11e3-8ddd-c5631815f517.png)
_______________________________________________________________
**Reference CDA Template:**
[More Details](http://www.hl7.org/Special/committees/structure/index.cfm)

N/A
_______________________________________________________________
**Reference OpenEHR Archetypes (Version 1.4):**
[More Details](http://www.openehr.org/ckm/)

N/A










