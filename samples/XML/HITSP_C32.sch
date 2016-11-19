<?xml version="1.0" encoding="UTF-8" standalone="yes"?>

<!--
Retrieved on 2016-08-30 from http://cda-validation.nist.gov/cda-validation/downloads.html.

Disclaimer from the National Institute of Standards and Technology website:

"Unless otherwise noted, this software was developed at the National Institute of Standards and Technology by employees of the Federal Government in the course of their official duties. Pursuant to title 17 Section 105 of the United States Code this software is not subject to copyright protection and is in the public domain. The CDA Guideline Validator is an experimental system. NIST assumes no responsibility whatsoever for its use by other parties, and makes no guarantees, expressed or implied, about its quality, reliability, or any other characteristic. We would appreciate acknowledgement if the software is used."
 -->

<!DOCTYPE schema [
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.1  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.1.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.2  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.2.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.3  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.3.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.4  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.4.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.5  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.5.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.6  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.6.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.7  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.7.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.8  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.8.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.9  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.9.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.10  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.10.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.11  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.11.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.12  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.12.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.13  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.13.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.14  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.14.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.15  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.15.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.16  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.16.ent'>
<!ENTITY ent-2.16.840.1.113883.3.88.11.32.17  SYSTEM 'templates/2.16.840.1.113883.3.88.11.32.17.ent'>
]>
<schema xmlns="http://www.ascc.net/xml/schematron" xmlns:cda="urn:hl7-org:v3">
    <!--
        To use iso schematron instead of schematron 1.5,
        change the xmlns attribute from
        "http://www.ascc.net/xml/schematron"
        to
        "http://purl.oclc.org/dsdl/schematron"
    -->
    <title>HITSP_C32</title>
    <ns prefix="cda" uri="urn:hl7-org:v3"/>
    <ns prefix="sdtc" uri="urn:hl7-org:sdtc"/>
    <ns prefix="xsi" uri="http://www.w3.org/2001/XMLSchema-instance"/>

    <phase id='errors'>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.1-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.2-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.3-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.4-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.5-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.6-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.7-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.8-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.9-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.10-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.11-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.12-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.13-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.14-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.15-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.16-errors'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.17-errors'/>
    </phase>

    <phase id='warning'>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.1-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.2-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.3-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.4-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.5-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.6-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.7-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.8-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.9-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.10-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.11-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.12-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.13-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.14-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.15-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.16-warning'/>
        <active pattern='p-2.16.840.1.113883.3.88.11.32.17-warning'/>
    </phase>

     <phase id='note'>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.1-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.2-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.3-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.4-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.5-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.6-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.7-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.8-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.9-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.10-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.11-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.12-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.13-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.14-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.15-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.16-note'/>
         <active pattern='p-2.16.840.1.113883.3.88.11.32.17-note'/>
     </phase>

     <phase id='violation'>

<active pattern='p-2.16.840.1.113883.3.88.11.32.4-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.5-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.6-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.7-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.8-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.9-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.10-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.11-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.12-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.13-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.14-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.15-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.16-violation'/>
<active pattern='p-2.16.840.1.113883.3.88.11.32.17-violation'/>

     </phase>

    <!-- Template_2.16.840.1.113883.3.88.11.32.1 -->
    <!-- HITSP/C32 Registration and Medication History -->

    &ent-2.16.840.1.113883.3.88.11.32.1;

    <!-- Template_2.16.840.1.113883.3.88.11.32.2 -->
    <!-- HITSP/C32 Language Spoken Module -->

    &ent-2.16.840.1.113883.3.88.11.32.2;

    <!-- Template_2.16.840.1.113883.3.88.11.32.3 -->
    <!-- HITSP/C32 Support Module -->

    &ent-2.16.840.1.113883.3.88.11.32.3;

    <!-- Template_2.16.840.1.113883.3.88.11.32.4 -->
    <!-- HITSP/C32 Healthcare Provider Module -->

    &ent-2.16.840.1.113883.3.88.11.32.4;

    <!-- Template_2.16.840.1.113883.3.88.11.32.5 -->
    <!-- HITSP/C32 Insurance Provider Module -->

    &ent-2.16.840.1.113883.3.88.11.32.5;

    <!-- Template_2.16.840.1.113883.3.88.11.32.6 -->
    <!-- HITSP/C32 Allergies and Drug Sensitivities Module -->

    &ent-2.16.840.1.113883.3.88.11.32.6;

    <!-- Template_2.16.840.1.113883.3.88.11.32.7 -->
    <!-- HITSP/C32 Conditions Module -->

    &ent-2.16.840.1.113883.3.88.11.32.7;

    <!-- Template_2.16.840.1.113883.3.88.11.32.8 -->
    <!-- HITSP/C32 Medications - Administration Information Module -->

    &ent-2.16.840.1.113883.3.88.11.32.8;

    <!-- Template_2.16.840.1.113883.3.88.11.32.9 -->
    <!-- HITSP/C32 Medications Module, Medication Information data element -->

    &ent-2.16.840.1.113883.3.88.11.32.9;

    <!-- Template_2.16.840.1.113883.3.88.11.32.10 -->
    <!-- HITSP/C32 Medications Module, Medication Information, Type of Medication data element -->

    &ent-2.16.840.1.113883.3.88.11.32.10;

    <!-- Template_2.16.840.1.113883.3.88.11.32.11 -->
    <!-- HITSP/C32 Order Information data element -->

    &ent-2.16.840.1.113883.3.88.11.32.11;

    <!-- Template_2.16.840.1.113883.3.88.11.32.12 -->
    <!-- HITSP/C32 Comments Module -->

    &ent-2.16.840.1.113883.3.88.11.32.12;

    <!-- Template_2.16.840.1.113883.3.88.11.32.13 -->
    <!-- HITSP/C32 Advance Directives Module -->

    &ent-2.16.840.1.113883.3.88.11.32.13;

    <!-- Template_2.16.840.1.113883.3.88.11.32.14 -->
    <!-- HITSP/C32 Immunizations Module -->

    &ent-2.16.840.1.113883.3.88.11.32.14;

    <!-- Template_2.16.840.1.113883.3.88.11.32.15 -->
    <!-- HITSP/C32 Vital Signs Module -->

    &ent-2.16.840.1.113883.3.88.11.32.15;

    <!-- Template_2.16.840.1.113883.3.88.11.32.16 -->
    <!-- HITSP/C32 Results Module -->

    &ent-2.16.840.1.113883.3.88.11.32.16;

    <!-- Template_2.16.840.1.113883.3.88.11.32.17 -->
    <!-- HITSP/C32 Encounters Module -->

    &ent-2.16.840.1.113883.3.88.11.32.17;

</schema>
