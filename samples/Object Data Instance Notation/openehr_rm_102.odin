-- 
--  component:   openEHR Archetype Project
--  description: openEHR Reference Model formal expression. This file is an ODIN serialisation of
--               the BMM object meta-model classes found at 
--               https://github.com/openEHR/bmm
--  keywords:    reference model, meta-model
--  author:      Thomas Beale
--  support:     Ocean Informatics <support@OceanInformatics.com>
--  copyright:   Copyright (c) 2009- openEHR Foundation
--  license:     See end of file
-- 

schema_revision = <"0.1">
schema_lifecycle_state = <"stable">

model_publisher = <"openehr">
model_name = <"rm">
model_release = <"1.0.2">

schema_description = <"openEHR Release 1.0.2 reference model, expressed in 'basic meta-model' format
                     described at http://www.openehr.org/svn/ref_impl_eiffel/BRANCHES/specialisation/libraries/common_libs/src/basic_meta_model"> 

packages = <
    ["ehr"] = <
        name = <"ehr">
        classes = <"COMPOSITION", ...> 
    >
    ["demographic"] = <
        name = <"demographic">
        classes = <"PARTY", "PARTY_RELATIONSHIP">
    >
>


------------------------------------------------------
-- primitive types
------------------------------------------------------

primitive_types = <
    ["Any"] = <
        name = <"Any">
        is_abstract = <True>
    >
    ["Ordered"] = <
        name = <"Ordered">
        is_abstract = <True>
        ancestors = </primitive_types["Any"], ...>
    >
    ["Numeric"] = <
        name = <"Numeric">
        is_abstract = <True>
        ancestors = </primitive_types["Any"], ...>
    >
    ["Ordered_Numeric"] = <
        name = <"Ordered_Numeric">
        is_abstract = <True>
        ancestors = </primitive_types["Numeric"], /primitive_types["Ordered"]>
    >
    ["Octet"] = <
        name = <"Octet">
        ancestors = </primitive_types["Any"], ...>
    >
    ["Boolean"] = <
        name = <"Boolean">
        ancestors = </primitive_types["Any"], ...>
    >
    ["Integer"] = <
        name = <"Integer">
        ancestors = </primitive_types["Ordered_Numeric"], ...>
    >
    ["Real"] = <
        name = <"Real">
        ancestors = </primitive_types["Ordered_Numeric"], ...>
    >
    ["Double"] = <
        name = <"Double">
        ancestors = </primitive_types["Ordered_Numeric"], ...>
    >
    ["Character"] = <
        name = <"Character">
        ancestors = </primitive_types["Any"], ...>
    >
    ["String"] = <
        name = <"String">
        ancestors = </primitive_types["Any"], ...>
    >
    ["List"] = <
        name = <"List">
        ancestors = </primitive_types["Any"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
            >
        >
    >
    ["Array"] = <
        name = <"Array">
        ancestors = </primitive_types["Any"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
            >
        >
    >
    ["Set"] = <
        name = <"Set">
        ancestors = </primitive_types["Any"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
            >
        >
    >
    ["Interval"] = <
        name = <"Interval">
        ancestors = </primitive_types["Any"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
                conforms_to_type = </primitive_types["Ordered"]>
            >
        >
        properties = <
            ["lower"] = (BMM_SINGLE_PROPERTY_OPEN) <
                name = <"lower">
                type = </primitive_types["Interval"]/generic_parameters["T"]>
            >
            ["upper"] = (BMM_SINGLE_PROPERTY_OPEN) <
                name = <"upper">
                type = </primitive_types["Interval"]/generic_parameters["T"]>
            >
            ["lower_unbounded"] = (BMM_SINGLE_PROPERTY) <
                name = <"lower_unbounded">
                type = </primitive_types["Boolean"]>
            >
            ["upper_unbounded"] = (BMM_SINGLE_PROPERTY) <
                name = <"upper_unbounded">
                type = </primitive_types["Boolean"]>
            >
            ["lower_included"] = (BMM_SINGLE_PROPERTY) <
                name = <"lower_included">
                type = </primitive_types["Boolean"]>
            >
            ["upper_included"] = (BMM_SINGLE_PROPERTY) <
                name = <"upper_included">
                type = </primitive_types["Boolean"]>
            >
        >
    >
    ["Hash"] = <
        name = <"Hash">
        ancestors = </primitive_types["Any"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
            >
            ["U"] = <
                name = <"U">
                conforms_to_type = </primitive_types["Ordered"]>
            >
        >
    >

    ["Aggregate"] = <
        name = <"Aggregate">
        ancestors = </primitive_types["Any"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
            >
        >
    >

    ["ISO8601_DATE"] = <
        name = <"ISO8601_DATE">
        ancestors = </primitive_types["Any"], ...>
    >

    ["ISO8601_TIME"] = <
        name = <"ISO8601_TIME">
        ancestors = </primitive_types["Any"], ...>
    >

    ["ISO8601_DATE_TIME"] = <
        name = <"ISO8601_DATE_TIME">
        ancestors = </primitive_types["Any"], ...>
    >

    ["ISO8601_DURATION"] = <
        name = <"ISO8601_DURATION">
        ancestors = </primitive_types["Any"], ...>
    >
>

------------------------------------------------------
-- classes
------------------------------------------------------

class_definitions = <

    ------------------------------------------------------------
    ------------------ rm.support.identification ---------------
    ------------------------------------------------------------

    ["OBJECT_REF"] = <
        name = <"OBJECT_REF">
        ancestors = </primitive_types["Any"], ...>
        properties = <
            ["namespace"] = (BMM_SINGLE_PROPERTY) <
                name = <"namespace">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
            ["type"] = (BMM_SINGLE_PROPERTY) <
                name = <"type">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["LOCATABLE_REF"] = <
        name = <"LOCATABLE_REF">
        ancestors = </class_definitions["OBJECT_REF"], ...>
        properties = <
            ["path"] = (BMM_SINGLE_PROPERTY) <
                name = <"path">
                type = </primitive_types["String"]>
            >
        >
    >

    ["PARTY_REF"] = <
        name = <"PARTY_REF">
        ancestors = </class_definitions["OBJECT_REF"], ...>
    >

    ["OBJECT_ID"] = <
        name = <"OBJECT_ID">
        is_abstract = <True>
        ancestors = </primitive_types["Any"], ...>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["TERMINOLOGY_ID"] = <
        name = <"TERMINOLOGY_ID">
        ancestors = </class_definitions["OBJECT_ID"], ...>
    >
    
    ["UID_BASED_ID"] = <
        name = <"UID_BASED_ID">
        ancestors = </class_definitions["OBJECT_ID"], ...>
        is_abstract = <True>
    >
    
    ["GENERIC_ID"] = <
        name = <"GENERIC_ID">
        ancestors = </class_definitions["OBJECT_ID"], ...>
    >
    
    ["ARCHETYPE_ID"] = <
        name = <"ARCHETYPE_ID">
        ancestors = </class_definitions["OBJECT_ID"], ...>
    >
    
    ["TEMPLATE_ID"] = <
        name = <"TEMPLATE_ID">
        ancestors = </class_definitions["OBJECT_ID"], ...>
    >
    
    ["OBJECT_VERSION_ID"] = <
        name = <"OBJECT_VERSION_ID">
        ancestors = </class_definitions["UID_BASED_ID"], ...>
    >
    
    ["HIER_OBJECT_ID"] = <
        name = <"HIER_OBJECT_ID">
        ancestors = </class_definitions["UID_BASED_ID"], ...>
    >
    
    ["VERSION_TREE_ID"] = <
        name = <"VERSION_TREE_ID">
        ancestors = </primitive_types["Any"], ...>
    >
    
    ["UID"] = <
        name = <"UID">
        is_abstract = <True>
        ancestors = </primitive_types["Any"], ...>
    >
    
    ["INTERNET_ID"] = <
        name = <"INTERNET_ID">
        ancestors = </class_definitions["UID"], ...>
    >
    
    ["UUID"] = <
        name = <"UUID">
        ancestors = </class_definitions["UID"], ...>
    >
    
    ["ISO_OID"] = <
        name = <"ISO_OID">
        ancestors = </class_definitions["UID"], ...>
    >
    
    ------------------------------------------------------------
    --------------------- rm.data_types ------------------------
    ------------------------------------------------------------

    --
    --------------------- rm.data_types.basic ------------------
    --

    ["DATA_VALUE"] = <
        name = <"DATA_VALUE">
        ancestors = </primitive_types["Any"], ...>
        is_abstract = <True>
    >

    ["DV_BOOLEAN"] = <
        name = <"DV_BOOLEAN">
        ancestors = </class_definitions["DATA_VALUE"], ...>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["Boolean"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_IDENTIFIER"] = <
        name = <"DV_IDENTIFIER">
        ancestors = </class_definitions["DATA_VALUE"], ...>
        properties = <
            ["issuer"] = (BMM_SINGLE_PROPERTY) <
                name = <"issuer">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
            ["id"] = (BMM_SINGLE_PROPERTY) <
                name = <"id">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
            ["type"] = (BMM_SINGLE_PROPERTY) <
                name = <"type">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
            ["assigner"] = (BMM_SINGLE_PROPERTY) <
                name = <"assigner">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_STATE"] = <
        name = <"DV_STATE">
        ancestors = </class_definitions["DATA_VALUE"], ...>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </class_definitions["DV_CODED_TEXT"]>
                is_mandatory = <True>
            >
            ["is_terminal"] = (BMM_SINGLE_PROPERTY) <
                name = <"is_terminal">
                type = </primitive_types["Boolean"]>
                is_mandatory = <True>
            >
        >
    >

    --
    --------------------- rm.data_types.text ------------------
    --

    ["TERM_MAPPING"] = <
        name = <"TERM_MAPPING">
        properties = <
            ["defining_code"] = (BMM_SINGLE_PROPERTY) <
                name = <"defining_code">
                type = </class_definitions["CODE_PHRASE"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_TEXT"] = <
        name = <"DV_TEXT">
        ancestors = </class_definitions["DATA_VALUE"], ...>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
            ["hyperlink"] = (BMM_SINGLE_PROPERTY) <
                name = <"hyperlink">
                type = </class_definitions["DV_URI"]>
            >
            ["language"] = (BMM_SINGLE_PROPERTY) <
                name = <"language">
                type = </class_definitions["CODE_PHRASE"]>
            >
            ["encoding"] = (BMM_SINGLE_PROPERTY) <
                name = <"encoding">
                type = </class_definitions["CODE_PHRASE"]>
            >
            ["formatting"] = (BMM_SINGLE_PROPERTY) <
                name = <"formatting">
                type = </primitive_types["String"]>
            >
            ["mappings"] = (BMM_CONTAINER_PROPERTY) <
                name = <"mappings">
                type = <
                    type = </class_definitions["TERM_MAPPING"]>
                    container_type = </primitive_types["Set"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["DV_CODED_TEXT"] = <
        name = <"DV_CODED_TEXT">
        ancestors = </class_definitions["DV_TEXT"], ...>
        properties = <
            ["defining_code"] = (BMM_SINGLE_PROPERTY) <
                name = <"defining_code">
                type = </class_definitions["CODE_PHRASE"]>
                is_mandatory = <True>
            >
        >
    >

    ["CODE_PHRASE"] = <
        name = <"CODE_PHRASE">
        properties = <
            ["terminology_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"terminology_id">
                type = </class_definitions["TERMINOLOGY_ID"]>
                is_mandatory = <True>
            >
            ["code_string"] = (BMM_SINGLE_PROPERTY) <
                name = <"code_string">
                type = </primitive_types["String"]>
            >
        >
    >

    --
    --------------------- rm.data_types.quantity ------------------
    --

    ["DV_INTERVAL"] = <
        name = <"DV_INTERVAL">
        ancestors = </primitive_types["Interval"], /class_definitions["DATA_VALUE"]>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
                conforms_to_type = </class_definitions["DV_ORDERED"]>
            >
        >
        properties = <
            ["lower"] = (BMM_SINGLE_PROPERTY_OPEN) <
                name = <"lower">
                type = </class_definitions["DV_INTERVAL"]/generic_parameters["T"]>
            >
            ["upper"] = (BMM_SINGLE_PROPERTY_OPEN) <
                name = <"upper">
                type = </class_definitions["DV_INTERVAL"]/generic_parameters["T"]>
            >
        >
    >

    ["REFERENCE_RANGE"] = <
        name = <"REFERENCE_RANGE">
        is_generic = <True>
        ancestors = </primitive_types["Any"], ...>
        generic_parameters = <
            ["T"] = <
                name = <"T">
                conforms_to_type = </class_definitions["DV_ORDERED"]>
            >
        >
        properties = <
            ["range"] = (BMM_SINGLE_PROPERTY) <
                name = <"range">
                type = </class_definitions["DV_INTERVAL"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_ORDERED"] = <
        name = <"DV_ORDERED">
        is_abstract = <True>
        ancestors = </primitive_types["Ordered"], /class_definitions["DATA_VALUE"]>
        properties = <
            ["normal_status"] = (BMM_SINGLE_PROPERTY) <
                name = <"normal_status">
                type = </class_definitions["CODE_PHRASE"]>
            >
            ["normal_range"] = (BMM_GENERIC_PROPERTY) <
                name = <"normal_range">
                type = <
                    root_type = </class_definitions["DV_INTERVAL"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["DV_INTERVAL"]/generic_parameters["T"]>
                    >
                >
            >
            ["other_reference_ranges"] = (BMM_GENERIC_PROPERTY) <
                name = <"other_reference_ranges">
                type = <
                    root_type = </class_definitions["REFERENCE_RANGE"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["REFERENCE_RANGE"]/generic_parameters["T"]>
                    >
                >
            >
        >
    >

    ["DV_QUANTIFIED"] = <
        name = <"DV_QUANTIFIED">
        is_abstract = <True>
        ancestors = </class_definitions["DV_ORDERED"], ...>
    >

    ["DV_ORDINAL"] = <
        name = <"DV_ORDINAL">
        ancestors = </class_definitions["DV_ORDERED"], ...>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["Integer"]>
                is_mandatory = <True>
            >
            ["symbol"] = (BMM_SINGLE_PROPERTY) <
                name = <"symbol">
                type = </class_definitions["DV_CODED_TEXT"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_AMOUNT"] = <
        name = <"DV_AMOUNT">
        is_abstract = <True>
        ancestors = </class_definitions["DV_QUANTIFIED"], ...>
        properties = <
            ["accuracy"] = (BMM_SINGLE_PROPERTY) <
                name = <"accuracy">
                type = </primitive_types["Real"]>
            >
            ["accuracy_is_percent"] = (BMM_SINGLE_PROPERTY) <
                name = <"accuracy_is_percent">
                type = </primitive_types["Boolean"]>
            >
        >
    >

    ["DV_ABSOLUTE_QUANTITY"] = <
        name = <"DV_ABSOLUTE_QUANTITY">
        is_abstract = <True>
        ancestors = </class_definitions["DV_QUANTIFIED"], ...>
        properties = <
            ["accuracy"] = (BMM_SINGLE_PROPERTY) <
                name = <"accuracy">
                type = </class_definitions["DV_AMOUNT"]>
            >
        >
    >

    ["DV_QUANTITY"] = <
        name = <"DV_QUANTITY">
        ancestors = </class_definitions["DV_AMOUNT"], ...>
        properties = <
            ["magnitude"] = (BMM_SINGLE_PROPERTY) <
                name = <"magnitude">
                type = </primitive_types["Double"]>
                is_mandatory = <True>
            >
            ["units"] = (BMM_SINGLE_PROPERTY) <
                name = <"units">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
            ["precision"] = (BMM_SINGLE_PROPERTY) <
                name = <"precision">
                type = </primitive_types["Integer"]>
            >
        >
    >

    ["DV_COUNT"] = <
        name = <"DV_COUNT">
        ancestors = </class_definitions["DV_AMOUNT"], ...>
        properties = <
            ["magnitude"] = (BMM_SINGLE_PROPERTY) <
                name = <"magnitude">
                type = </primitive_types["Integer"]>
            >
        >
    >

    ["DV_PROPORTION"] = <
        name = <"DV_PROPORTION">
        ancestors = </class_definitions["DV_AMOUNT"], ...>
        properties = <
            ["numerator"] = (BMM_SINGLE_PROPERTY) <
                name = <"numerator">
                type = </primitive_types["Real"]>
                is_mandatory = <True>
            >
            ["denominator"] = (BMM_SINGLE_PROPERTY) <
                name = <"denominator">
                type = </primitive_types["Real"]>
                is_mandatory = <True>
            >
            ["type"] = (BMM_SINGLE_PROPERTY) <
                name = <"type">
                type = </primitive_types["Integer"]>
                is_mandatory = <True>
            >
            ["precision"] = (BMM_SINGLE_PROPERTY) <
                name = <"precision">
                type = </primitive_types["Integer"]>
            >
            ["is_integral"] = (BMM_SINGLE_PROPERTY) <
                name = <"is_integral">
                type = </primitive_types["Boolean"]>
                is_computed = <True>
            >
        >
    >

    --
    --------------------- rm.data_types.quantity.date_time ------------------
    --

    ["DV_TEMPORAL"] = <
        name = <"DV_TEMPORAL">
        is_abstract = <True>
        ancestors = </class_definitions["DV_ABSOLUTE_QUANTITY"], ...>
    >

    ["DV_DATE"] = <
        name = <"DV_DATE">
        ancestors = </class_definitions["DV_TEMPORAL"], /primitive_types["ISO8601_DATE"]>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_TIME"] = <
        name = <"DV_TIME">
        ancestors = </class_definitions["DV_TEMPORAL"], /primitive_types["ISO8601_TIME"]>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_DATE_TIME"] = <
        name = <"DV_DATE_TIME">
        ancestors = </class_definitions["DV_TEMPORAL"], /primitive_types["ISO8601_DATE_TIME"]>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_DURATION"] = <
        name = <"DV_DURATION">
        ancestors = </class_definitions["DV_AMOUNT"], /primitive_types["ISO8601_DURATION"]>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    --
    --------------------- rm.data_types.encapsulated ------------------
    --

    ["DV_ENCAPSULATED"] = <
        name = <"DV_ENCAPSULATED">
        is_abstract = <True>
        ancestors = </class_definitions["DATA_VALUE"], ...>
        properties = <
            ["charset"] = (BMM_SINGLE_PROPERTY) <
                name = <"charset">
                type = </class_definitions["CODE_PHRASE"]>
            >
            ["language"] = (BMM_SINGLE_PROPERTY) <
                name = <"language">
                type = </class_definitions["CODE_PHRASE"]>
            >
        >
    >

    ["DV_MULTIMEDIA"] = <
        name = <"DV_MULTIMEDIA">
        ancestors = </class_definitions["DV_ENCAPSULATED"], ...>
        properties = <
            ["alternate_text"] = (BMM_SINGLE_PROPERTY) <
                name = <"alternate_text">
                type = </primitive_types["String"]>
            >
            ["uri"] = (BMM_SINGLE_PROPERTY) <
                name = <"uri">
                type = </class_definitions["DV_URI"]>
            >
            ["data"] = (BMM_CONTAINER_PROPERTY) <
                name = <"uri">
                type = <
                    type = </primitive_types["Octet"]>
                    container_type = </primitive_types["Array"]>
                    cardinality = <|>=0|>
                >
            >
            ["media_type"] = (BMM_SINGLE_PROPERTY) <
                name = <"media_type">
                type = </class_definitions["CODE_PHRASE"]>
                is_mandatory = <True>
            >
            ["compression_algorithm"] = (BMM_SINGLE_PROPERTY) <
                name = <"compression_algorithm">
                type = </class_definitions["CODE_PHRASE"]>
            >
            ["integrity_check"] = (BMM_CONTAINER_PROPERTY) <
                name = <"integrity_check">
                type = <
                    type = </primitive_types["Octet"]>
                    container_type = </primitive_types["Array"]>
                    cardinality = <|>=0|>
                >
            >
            ["integrity_check_algorithm"] = (BMM_SINGLE_PROPERTY) <
                name = <"integrity_check_algorithm">
                type = </class_definitions["CODE_PHRASE"]>
            >
            ["size"] = (BMM_SINGLE_PROPERTY) <
                name = <"size">
                type = </primitive_types["Integer"]>
            >
        >
    >

    ["DV_PARSABLE"] = <
        name = <"DV_PARSABLE">
        ancestors = </class_definitions["DV_ENCAPSULATED"], ...>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["String"]>
            >
            ["formalism"] = (BMM_SINGLE_PROPERTY) <
                name = <"formalism">
                type = </primitive_types["String"]>
            >
        >
    >

    --
    --------------------- rm.data_types.uri ------------------
    --

    ["DV_URI"] = <
        name = <"DV_URI">
        ancestors = </class_definitions["DATA_VALUE"], ...>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_EHR_URI"] = <
        name = <"DV_EHR_URI">
        ancestors = </class_definitions["DV_URI"], ...>
    >

    --
    --------------------- rm.data_types.time_specification ------------------
    --

    ["DV_TIME_SPECIFICATION"] = <
        name = <"DV_TIME_SPECIFICATION">
        is_abstract = <True>
        ancestors = </class_definitions["DATA_VALUE"], ...>
        properties = <
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </class_definitions["DV_PARSABLE"]>
                is_mandatory = <True>
            >
        >
    >

    ["DV_PERIODIC_TIME_SPECIFICATION"] = <
        name = <"DV_PERIODIC_TIME_SPECIFICATION">
        ancestors = </class_definitions["DV_TIME_SPECIFICATION"], ...>
    >

    ["DV_GENERAL_TIME_SPECIFICATION"] = <
        name = <"DV_GENERAL_TIME_SPECIFICATION">
        ancestors = </class_definitions["DV_TIME_SPECIFICATION"], ...>
    >


    ------------------------------------------------------------
    ---------------------- rm.data_structures ------------------
    ------------------------------------------------------------

    ["DATA_STRUCTURE"] = <
        name = <"DATA_STRUCTURE">
        is_abstract = <True>
        ancestors = </class_definitions["LOCATABLE"], ...>
    >

    ["ITEM_STRUCTURE"] = <
        name = <"ITEM_STRUCTURE">
        is_abstract = <True>
        ancestors = </class_definitions["DATA_STRUCTURE"], ...>
    >

    ["ITEM_SINGLE"] = <
        name = <"ITEM_SINGLE">
        ancestors = </class_definitions["ITEM_STRUCTURE"], ...>
        properties = <
            ["item"] = (BMM_SINGLE_PROPERTY) <
                name = <"item">
                type = </class_definitions["ELEMENT"]>
                is_mandatory = <True>
            >
        >
    >

    ["ITEM_LIST"] = <
        name = <"ITEM_LIST">
        ancestors = </class_definitions["ITEM_STRUCTURE"], ...>
        properties = <
            ["items"] = (BMM_CONTAINER_PROPERTY) <
                name = <"items">
                type = <
                    type = </class_definitions["ELEMENT"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["ITEM_TABLE"] = <
        name = <"ITEM_TABLE">
        ancestors = </class_definitions["ITEM_STRUCTURE"], ...>
        properties = <
            ["rows"] = (BMM_CONTAINER_PROPERTY) <
                name = <"rows">
                type = <
                    type = </class_definitions["CLUSTER"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["ITEM_TREE"] = <
        name = <"ITEM_TREE">
        ancestors = </class_definitions["ITEM_STRUCTURE"], ...>
        properties = <
            ["items"] = (BMM_CONTAINER_PROPERTY) <
                name = <"items">
                type = <
                    type = </class_definitions["ITEM"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["ITEM"] = <
        name = <"ITEM">
        ancestors = </class_definitions["LOCATABLE"], ...>
        is_abstract = <True>
    >

    ["CLUSTER"] = <
        name = <"CLUSTER">
        ancestors = </class_definitions["ITEM"], ...>
        properties = <
            ["items"] = (BMM_CONTAINER_PROPERTY) <
                name = <"items">
                type = <
                    type = </class_definitions["ITEM"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=1|>
                >
                is_mandatory = <True>
            >
        >
    >

    ["ELEMENT"] = <
        name = <"ELEMENT">
        ancestors = </class_definitions["ITEM"], ...>
        properties = <
            ["null_flavour"] = (BMM_SINGLE_PROPERTY) <
                name = <"null_flavour">
                type = </class_definitions["DV_CODED_TEXT"]>
            >
            ["value"] = (BMM_SINGLE_PROPERTY) <
                name = <"value">
                type = </class_definitions["DATA_VALUE"]>
            >
        >
    >

    ["HISTORY"] = <
        name = <"HISTORY">
        ancestors = </class_definitions["DATA_STRUCTURE"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
                conforms_to_type = </class_definitions["ITEM_STRUCTURE"]>
            >
        >
        properties = <
            ["origin"] = (BMM_SINGLE_PROPERTY) <
                name = <"origin">
                type = </class_definitions["DV_DATE_TIME"]>
                is_mandatory = <True>
            >
            ["period"] = (BMM_SINGLE_PROPERTY) <
                name = <"period">
                type = </class_definitions["DV_DURATION"]>
            >
            ["duration"] = (BMM_SINGLE_PROPERTY) <
                name = <"duration">
                type = </class_definitions["DV_DURATION"]>
            >
            ["summary"] = (BMM_SINGLE_PROPERTY) <
                name = <"summary">
                type = </class_definitions["ITEM_STRUCTURE"]>
            >
            ["events"] = (BMM_CONTAINER_PROPERTY) <
                name = <"events">
                type = <
                    type = </class_definitions["EVENT"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["EVENT"] = <
        name = <"EVENT">
        ancestors = </class_definitions["LOCATABLE"], ...>
        is_abstract = <True>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
                conforms_to_type = </class_definitions["ITEM_STRUCTURE"]>
            >
        >
        properties = <
            ["time"] = (BMM_SINGLE_PROPERTY) <
                name = <"time">
                type = </class_definitions["DV_DATE_TIME"]>
                is_mandatory = <True>
            >
            ["state"] = (BMM_SINGLE_PROPERTY) <
                name = <"state">
                type = </class_definitions["ITEM_STRUCTURE"]>
            >
            ["data"] = (BMM_SINGLE_PROPERTY_OPEN) <
                name = <"data">
                type = </class_definitions["EVENT"]/generic_parameters["T"]>
                is_mandatory = <True>
            >
            ["offset"] = (BMM_SINGLE_PROPERTY) <
                name = <"offset">
                is_computed = <True>
                type = </class_definitions["DV_DURATION"]>
            >
        >
    >

    ["POINT_EVENT"] = <
        name = <"POINT_EVENT">
        ancestors = </class_definitions["EVENT"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
            >
        >
    >

    ["INTERVAL_EVENT"] = <
        name = <"INTERVAL_EVENT">
        ancestors = </class_definitions["EVENT"], ...>
        is_generic = <True>
        generic_parameters = <
            ["T"] = <
                name = <"T">
            >
        >
        properties = <
            ["width"] = (BMM_SINGLE_PROPERTY) <
                name = <"width">
                type = </class_definitions["DV_DURATION"]>
                is_mandatory = <True>
            >
            ["sample_count"] = (BMM_SINGLE_PROPERTY) <
                name = <"sample_count">
                type = </primitive_types["Integer"]>
            >
            ["math_function"] = (BMM_SINGLE_PROPERTY) <
                name = <"math_function">
                type = </class_definitions["DV_CODED_TEXT"]>
                is_mandatory = <True>
            >
        >
    >

    ------------------------------------------------------------
    ---------------------- rm.common.generic -------------------
    ------------------------------------------------------------

    ["PARTICIPATION"] = <
        name = <"PARTICIPATION">
        ancestors = </primitive_types["Any"], ...>
        properties = <
            ["function"] = (BMM_SINGLE_PROPERTY) <
                name = <"function">
                type = </class_definitions["DV_TEXT"]>
                is_mandatory = <True>
            >
            ["time"] = (BMM_GENERIC_PROPERTY) <
                name = <"time">
                type = <
                    root_type = </class_definitions["DV_INTERVAL"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["DV_DATE_TIME"]>
                    >
                >
            >
            ["mode"] = (BMM_SINGLE_PROPERTY) <
                name = <"mode">
                type = </class_definitions["DV_CODED_TEXT"]>
                is_mandatory = <True>
            >
            ["performer"] = (BMM_SINGLE_PROPERTY) <
                name = <"performer">
                type = </class_definitions["PARTY_PROXY"]>
                is_mandatory = <True>
            >
        >
    >

    ["PARTY_PROXY"] = <
        name = <"PARTY_PROXY">
        is_abstract = <True>
        ancestors = </primitive_types["Any"], ...>
        properties = <
            ["external_ref"] = (BMM_SINGLE_PROPERTY) <
                name = <"external_ref">
                type = </class_definitions["PARTY_REF"]>
            >
        >
    >

    ["PARTY_IDENTIFIED"] = <
        name = <"PARTY_IDENTIFIED">
        ancestors = </class_definitions["PARTY_PROXY"], ...>
        properties = <
            ["name"] = (BMM_SINGLE_PROPERTY) <
                name = <"name">
                type = </primitive_types["String"]>
            >
            ["identifiers"] = (BMM_CONTAINER_PROPERTY) <
                name = <"identifiers">
                type = <
                    type = </class_definitions["DV_IDENTIFIER"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["PARTY_RELATED"] = <
        name = <"PARTY_RELATED">
        ancestors = </class_definitions["PARTY_IDENTIFIED"], ...>
        properties = <
            ["relationship"] = (BMM_SINGLE_PROPERTY) <
                name = <"relationship">
                type = </class_definitions["DV_CODED_TEXT"]>
                is_mandatory = <True>
            >
        >
    >

    ["PARTY_SELF"] = <
        name = <"PARTY_SELF">
        ancestors = </class_definitions["PARTY_PROXY"], ...>
    >

    ------------------------------------------------------------
    --------------------- rm.common.archetyped -----------------
    ------------------------------------------------------------

    ["PATHABLE"] = <
        name = <"PATHABLE">
        is_abstract = <True>
        ancestors = </primitive_types["Any"], ...>
    >

    ["LOCATABLE"] = <
        name = <"LOCATABLE">
        is_abstract = <True>
        ancestors = </class_definitions["PATHABLE"], ...>
        properties = <
            ["uid"] = (BMM_SINGLE_PROPERTY) <
                name = <"uid">
                type = </class_definitions["UID_BASED_ID"]>
            >
            ["archetype_node_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"archetype_node_id">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
            ["name"] = (BMM_SINGLE_PROPERTY) <
                name = <"name">
                type = </class_definitions["DV_TEXT"]>
                is_mandatory = <True>
            >
            ["archetype_details"] = (BMM_SINGLE_PROPERTY) <
                name = <"archetype_details">
                type = </class_definitions["ARCHETYPED"]>
            >
            ["feeder_audit"] = (BMM_SINGLE_PROPERTY) <
                name = <"feeder_audit">
                type = </class_definitions["FEEDER_AUDIT"]>
            >
            ["links"] = (BMM_CONTAINER_PROPERTY) <
                name = <"links">
                type = <
                    type = </class_definitions["LINK"]>
                    container_type = </primitive_types["Set"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["LINK"] = <
        name = <"LINK">
        ancestors = </primitive_types["Any"], ...>
        properties = <
            ["meaning"] = (BMM_SINGLE_PROPERTY) <
                name = <"meaning">
                type = </class_definitions["DV_TEXT"]>
                is_mandatory = <True>
            >
            ["type"] = (BMM_SINGLE_PROPERTY) <
                name = <"type">
                type = </class_definitions["DV_TEXT"]>
                is_mandatory = <True>
            >
            ["target"] = (BMM_SINGLE_PROPERTY) <
                name = <"target">
                type = </class_definitions["DV_EHR_URI"]>
                is_mandatory = <True>
            >
        >
    >

    ["ARCHETYPED"] = <
        name = <"ARCHETYPED">
        ancestors = </primitive_types["Any"], ...>
        properties = <
            ["archetype_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"archetype_id">
                type = </class_definitions["ARCHETYPE_ID"]>
                is_mandatory = <True>
            >
            ["template_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"template_id">
                type = </class_definitions["TEMPLATE_ID"]>
            >
            ["rm_version"] = (BMM_SINGLE_PROPERTY) <
                name = <"rm_version">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["FEEDER_AUDIT"] = <
        name = <"FEEDER_AUDIT">
        ancestors = </primitive_types["Any"], ...>
        properties = <
            ["originating_system_ids"] = (BMM_CONTAINER_PROPERTY) <
                name = <"originating_system_ids">
                type = <
                    type = </class_definitions["DV_IDENTIFIER"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
            ["feeder_system_item_ids"] = (BMM_CONTAINER_PROPERTY) <
                name = <"feeder_system_item_ids">
                type = <
                    type = </class_definitions["DV_IDENTIFIER"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
            ["original_content"] = (BMM_SINGLE_PROPERTY) <
                name = <"original_content">
                type = </class_definitions["DV_ENCAPSULATED"]>
            >
            ["originating_system_audit"] = (BMM_SINGLE_PROPERTY) <
                name = <"originating_system_audit">
                type = </class_definitions["FEEDER_AUDIT_DETAILS"]>
                is_mandatory = <True>
            >
            ["feeder_system_audit"] = (BMM_SINGLE_PROPERTY) <
                name = <"feeder_system_audit">
                type = </class_definitions["FEEDER_AUDIT_DETAILS"]>
            >
        >
    >

    ["FEEDER_AUDIT_DETAILS"] = <
        name = <"FEEDER_AUDIT_DETAILS">
        ancestors = </primitive_types["Any"], ...>
        properties = <
            ["system_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"system_id">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
            ["location"] = (BMM_SINGLE_PROPERTY) <
                name = <"location">
                type = </class_definitions["PARTY_IDENTIFIED"]>
            >
            ["provider"] = (BMM_SINGLE_PROPERTY) <
                name = <"provider">
                type = </class_definitions["PARTY_IDENTIFIED"]>
            >
            ["subject"] = (BMM_SINGLE_PROPERTY) <
                name = <"subject">
                type = </class_definitions["PARTY_PROXY"]>
            >
            ["time"] = (BMM_SINGLE_PROPERTY) <
                name = <"time">
                type = </class_definitions["DV_DATE_TIME"]>
            >
            ["version_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"version_id">
                type = </primitive_types["String"]>
            >
        >
    >

    ------------------------------------------------------------
    --------------------- rm.composition -----------------------
    ------------------------------------------------------------

    ["COMPOSITION"] = <
        name = <"COMPOSITION">
        ancestors = </class_definitions["LOCATABLE"], ...>
        properties = <
            ["language"] = (BMM_SINGLE_PROPERTY) <
                name = <"language">
                type = </class_definitions["CODE_PHRASE"]>
                is_mandatory = <True>
            >
            ["territory"] = (BMM_SINGLE_PROPERTY) <
                name = <"territory">
                type = </class_definitions["CODE_PHRASE"]>
                is_mandatory = <True>
            >
            ["category"] = (BMM_SINGLE_PROPERTY) <
                name = <"category">
                type = </class_definitions["DV_CODED_TEXT"]>
                is_mandatory = <True>
            >
            ["composer"] = (BMM_SINGLE_PROPERTY) <
                name = <"composer">
                type = </class_definitions["PARTY_PROXY"]>
                is_mandatory = <True>
            >
            ["context"] = (BMM_SINGLE_PROPERTY) <
                name = <"context">
                type = </class_definitions["EVENT_CONTEXT"]>
            >
            ["content"] = (BMM_CONTAINER_PROPERTY) <
                name = <"content">
                type = <
                    type = </class_definitions["CONTENT_ITEM"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["EVENT_CONTEXT"] = <
        name = <"EVENT_CONTEXT">
        ancestors = </class_definitions["PATHABLE"], ...>
        properties = <
            ["health_care_facility"] = (BMM_SINGLE_PROPERTY) <
                name = <"health_care_facility">
                type = </class_definitions["PARTY_IDENTIFIED"]>
            >
            ["start_time"] = (BMM_SINGLE_PROPERTY) <
                name = <"start_time">
                type = </class_definitions["DV_DATE_TIME"]>
                is_mandatory = <True>
            >
            ["end_time"] = (BMM_SINGLE_PROPERTY) <
                name = <"end_time">
                type = </class_definitions["DV_DATE_TIME"]>
            >
            ["participations"] = (BMM_CONTAINER_PROPERTY) <
                name = <"participations">
                type = <
                    type = </class_definitions["PARTICIPATION"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
            ["location"] = (BMM_SINGLE_PROPERTY) <
                name = <"location">
                type = </primitive_types["String"]>
            >
            ["setting"] = (BMM_SINGLE_PROPERTY) <
                name = <"setting">
                type = </class_definitions["DV_CODED_TEXT"]>
                is_mandatory = <True>
            >
            ["other_context"] = (BMM_SINGLE_PROPERTY) <
                name = <"other_context">
                type = </class_definitions["ITEM_STRUCTURE"]>
                is_mandatory = <True>
            >
        >
    >

    --
    --------------------- rm.data_types.composition.content ------------------
    --

    ["CONTENT_ITEM"] = <
        name = <"CONTENT_ITEM">
        ancestors = </class_definitions["LOCATABLE"], ...>
        is_abstract = <True>
    >

    --
    --------------- rm.data_types.composition.content.navigation -------------
    --

    ["SECTION"] = <
        name = <"SECTION">
        ancestors = </class_definitions["CONTENT_ITEM"], ...>
        properties = <
            ["items"] = (BMM_CONTAINER_PROPERTY) <
                name = <"items">
                type = <
                    type = </class_definitions["CONTENT_ITEM"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    --
    --------------- rm.data_types.composition.content.navigation -------------
    --

    ["ENTRY"] = <
        name = <"ENTRY">
        is_abstract = <True>
        ancestors = </class_definitions["CONTENT_ITEM"], ...>
        properties = <
            ["language"] = (BMM_SINGLE_PROPERTY) <
                name = <"language">
                type = </class_definitions["CODE_PHRASE"]>
                is_mandatory = <True>
            >
            ["encoding"] = (BMM_SINGLE_PROPERTY) <
                name = <"encoding">
                type = </class_definitions["CODE_PHRASE"]>
                is_mandatory = <True>
            >
            ["subject"] = (BMM_SINGLE_PROPERTY) <
                name = <"subject">
                type = </class_definitions["PARTY_PROXY"]>
                is_mandatory = <True>
            >
            ["provider"] = (BMM_SINGLE_PROPERTY) <
                name = <"provider">
                type = </class_definitions["PARTY_PROXY"]>
                is_mandatory = <True>
            >
            ["other_participations"] = (BMM_CONTAINER_PROPERTY) <
                name = <"other_participations">
                type = <
                    type = </class_definitions["PARTICIPATION"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
            ["workflow_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"workflow_id">
                type = </class_definitions["OBJECT_REF"]>
            >
        >
    >

    ["ADMIN_ENTRY"] = <
        name = <"ADMIN_ENTRY">
        ancestors = </class_definitions["ENTRY"], ...>
        properties = <
            ["data"] = (BMM_SINGLE_PROPERTY) <
                name = <"data">
                type = </class_definitions["ITEM_STRUCTURE"]>
                is_mandatory = <True>
            >
        >
    >
    
    ["CARE_ENTRY"] = <
        name = <"CARE_ENTRY">
        is_abstract = <True>
        ancestors = </class_definitions["ENTRY"], ...>
        properties = <
            ["protocol"] = (BMM_SINGLE_PROPERTY) <
                name = <"protocol">
                type = </class_definitions["ITEM_STRUCTURE"]>
            >
            ["guideline_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"guideline_id">
                type = </class_definitions["OBJECT_REF"]>
            >
        >
    >

    ["OBSERVATION"] = <
        name = <"OBSERVATION">
        ancestors = </class_definitions["CARE_ENTRY"], ...>
        properties = <
            ["data"] = (BMM_GENERIC_PROPERTY) <
                name = <"data">
                is_mandatory = <True>
                type = <
                    root_type = </class_definitions["HISTORY"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["ITEM_STRUCTURE"]>
                    >
                >
            >
            ["state"] = (BMM_GENERIC_PROPERTY) <
                name = <"state">
                type = <
                    root_type = </class_definitions["HISTORY"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["ITEM_STRUCTURE"]>
                    >
                >
            >
        >
    >

    ["EVALUATION"] = <
        name = <"EVALUATION">
        ancestors = </class_definitions["CARE_ENTRY"], ...>
        properties = <
            ["data"] = (BMM_SINGLE_PROPERTY) <
                name = <"data">
                type = </class_definitions["ITEM_STRUCTURE"]>
                is_mandatory = <True>
            >
        >
    >

    ["INSTRUCTION"] = <
        name = <"INSTRUCTION">
        ancestors = </class_definitions["CARE_ENTRY"], ...>
        properties = <
            ["narrative"] = (BMM_SINGLE_PROPERTY) <
                name = <"narrative">
                type = </class_definitions["DV_TEXT"]>
                is_mandatory = <True>
            >
            ["expiry_time"] = (BMM_SINGLE_PROPERTY) <
                name = <"expiry_time">
                type = </class_definitions["DV_DATE_TIME"]>
            >
            ["wf_definition"] = (BMM_SINGLE_PROPERTY) <
                name = <"wf_definition">
                type = </class_definitions["DV_PARSABLE"]>
            >
            ["activities"] = (BMM_CONTAINER_PROPERTY) <
                name = <"activities">
                type = <
                    type = </class_definitions["ACTIVITY"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
        >
    >

    ["ACTIVITY"] = <
        name = <"ACTIVITY">
        ancestors = </class_definitions["LOCATABLE"], ...>
        properties = <
            ["description"] = (BMM_SINGLE_PROPERTY) <
                name = <"description">
                type = </class_definitions["ITEM_STRUCTURE"]>
                is_mandatory = <True>
            >
            ["timing"] = (BMM_SINGLE_PROPERTY) <
                name = <"timing">
                type = </class_definitions["DV_PARSABLE"]>
                is_mandatory = <True>
            >
            ["action_archetype_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"action_archetype_id">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["ACTION"] = <
        name = <"ACTION">
        ancestors = </class_definitions["CARE_ENTRY"], ...>
        properties = <
            ["time"] = (BMM_SINGLE_PROPERTY) <
                name = <"time">
                type = </class_definitions["DV_DATE_TIME"]>
                is_mandatory = <True>
            >
            ["description"] = (BMM_SINGLE_PROPERTY) <
                name = <"description">
                type = </class_definitions["ITEM_STRUCTURE"]>
                is_mandatory = <True>
            >
            ["ism_transition"] = (BMM_SINGLE_PROPERTY) <
                name = <"ism_transition">
                type = </class_definitions["ISM_TRANSITION"]>
                is_mandatory = <True>
            >
            ["instruction_details"] = (BMM_SINGLE_PROPERTY) <
                name = <"instruction_details">
                type = </class_definitions["INSTRUCTION_DETAILS"]>
            >
        >
    >

    ["INSTRUCTION_DETAILS"] = <
        name = <"INSTRUCTION_DETAILS">
        ancestors = </class_definitions["PATHABLE"], ...>
        properties = <
            ["instruction_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"instruction_id">
                type = </class_definitions["LOCATABLE_REF"]>
                is_mandatory = <True>
            >
            ["wf_details"] = (BMM_SINGLE_PROPERTY) <
                name = <"wf_details">
                type = </class_definitions["ITEM_STRUCTURE"]>
            >
            ["activity_id"] = (BMM_SINGLE_PROPERTY) <
                name = <"activity_id">
                type = </primitive_types["String"]>
                is_mandatory = <True>
            >
        >
    >

    ["ISM_TRANSITION"] = <
        name = <"ISM_TRANSITION">
        ancestors = </class_definitions["PATHABLE"], ...>
        properties = <
            ["current_state"] = (BMM_SINGLE_PROPERTY) <
                name = <"current_state">
                type = </class_definitions["DV_CODED_TEXT"]>
                is_mandatory = <True>
            >
            ["transition"] = (BMM_SINGLE_PROPERTY) <
                name = <"transition">
                type = </class_definitions["DV_CODED_TEXT"]>
            >
            ["careflow_step"] = (BMM_SINGLE_PROPERTY) <
                name = <"careflow_step">
                type = </class_definitions["DV_CODED_TEXT"]>
            >
        >
    >

    ------------------------------------------------------------
    ----------------------- rm.demographics --------------------
    ------------------------------------------------------------
   
    ["PARTY"] = <
        name = <"PARTY">
        is_abstract = <True>
        ancestors = </class_definitions["LOCATABLE"], ...>
        properties = <
            ["details"] = (BMM_SINGLE_PROPERTY) <
                name = <"details">
                type = </class_definitions["ITEM_STRUCTURE"]>
            >
            ["identities"] = (BMM_CONTAINER_PROPERTY) <
                name = <"identities">
                type = <
                    type = </class_definitions["PARTY_IDENTITY"]>
                    container_type = </primitive_types["Set"]>
                    cardinality = <|>=1|>
                >
            >
            ["contacts"] = (BMM_CONTAINER_PROPERTY) <
                name = <"contacts">
                type = <
                    type = </class_definitions["CONTACT"]>
                    container_type = </primitive_types["Set"]>
                    cardinality = <|>=0|>
                >
            >
            ["relationships"] = (BMM_CONTAINER_PROPERTY) <
                name = <"relationships">
                type = <
                    type = </class_definitions["PARTY_RELATIONSHIP"]>
                    container_type = </primitive_types["Set"]>
                    cardinality = <|>=0|>
                >
            >
            ["reverse_relationships"] = (BMM_CONTAINER_PROPERTY) <
                name = <"reverse_relationships">
                type = <
                    type = </class_definitions["LOCATABLE_REF"]>
                    container_type = </primitive_types["Set"]>
                    cardinality = <|>=0|>
                >
            >
            ["type"] = (BMM_SINGLE_PROPERTY) <
                name = <"type">
                type = </class_definitions["DV_TEXT"]>
                is_computed = <True>
            >
        >
    >
    
    ["PARTY_IDENTITY"] = <
        name = <"PARTY_IDENTITY">
        ancestors = </class_definitions["LOCATABLE"], ...>
        properties = <
            ["details"] = (BMM_SINGLE_PROPERTY) <
                name = <"details">
                type = </class_definitions["ITEM_STRUCTURE"]>
                is_mandatory = <True>
            >
            ["purpose"] = (BMM_SINGLE_PROPERTY) <
                name = <"purpose">
                type = </class_definitions["DV_TEXT"]>
                is_computed = <True>
            >
        >
    >
    
    ["CONTACT"] = <
        name = <"CONTACT">
        ancestors = </class_definitions["LOCATABLE"], ...>
        properties = <
            ["time_validity"] = (BMM_GENERIC_PROPERTY) <
                name = <"time_validity">
                type = <
                    root_type = </class_definitions["DV_INTERVAL"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["DV_DATE"]>
                    >
                >
            >
            ["addresses"] = (BMM_CONTAINER_PROPERTY) <
                name = <"addresses">
                type = <
                    type = </class_definitions["ADDRESS"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=1|>
                >
            >
            ["purpose"] = (BMM_SINGLE_PROPERTY) <
                name = <"purpose">
                type = </class_definitions["DV_TEXT"]>
                is_computed = <True>
            >
        >
    >
    
    ["ADDRESS"] = <
        name = <"ADDRESS">
        ancestors = </class_definitions["LOCATABLE"], ...>
        properties = <
            ["details"] = (BMM_SINGLE_PROPERTY) <
                name = <"details">
                type = </class_definitions["ITEM_STRUCTURE"]>
                is_mandatory = <True>
            >
            ["type"] = (BMM_SINGLE_PROPERTY) <
                name = <"type">
                type = </class_definitions["DV_TEXT"]>
                is_computed = <True>
            >
        >
    >
    
    ["ACTOR"] = <
        name = <"ACTOR">
        is_abstract = <True>
        ancestors = </class_definitions["PARTY"], ...>
        properties = <
            ["roles"] = (BMM_CONTAINER_PROPERTY) <
                name = <"roles">
                type = <
                    type = </class_definitions["PARTY_REF"]>
                    container_type = </primitive_types["Set"]>
                    cardinality = <|>=1|>
                >
            >
            ["languages"] = (BMM_CONTAINER_PROPERTY) <
                name = <"languages">
                type = <
                    type = </class_definitions["DV_TEXT"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=1|>
                >
            >
        >
    >

    ["PERSON"] = <
        name = <"PERSON">
        ancestors = </class_definitions["ACTOR"], ...>
    >

    ["ORGANISATION"] = <
        name = <"ORGANISATION">
        ancestors = </class_definitions["ACTOR"], ...>
    >

    ["GROUP"] = <
        name = <"GROUP">
        ancestors = </class_definitions["ACTOR"], ...>
    >

    ["AGENT"] = <
        name = <"AGENT">
        ancestors = </class_definitions["ACTOR"], ...>
    >

    ["ROLE"] = <
        name = <"ROLE">
        ancestors = </class_definitions["PARTY"], ...>
        properties = <
            ["performer"] = (BMM_SINGLE_PROPERTY) <
                name = <"performer">
                type = </class_definitions["PARTY_REF"]>
                is_mandatory = <True>
            >
            ["capabilities"] = (BMM_CONTAINER_PROPERTY) <
                name = <"capabilities">
                type = <
                    type = </class_definitions["CAPABILITY"]>
                    container_type = </primitive_types["List"]>
                    cardinality = <|>=0|>
                >
            >
            ["time_validity"] = (BMM_GENERIC_PROPERTY) <
                name = <"time_validity">
                type = <
                    root_type = </class_definitions["DV_INTERVAL"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["DV_DATE"]>
                    >
                >
            >
        >
    >

    ["CAPABILITY"] = <
        name = <"CAPABILITY">
        ancestors = </class_definitions["LOCATABLE"], ...>
        properties = <
            ["credentials"] = (BMM_SINGLE_PROPERTY) <
                name = <"credentials">
                type = </class_definitions["ITEM_STRUCTURE"]>
                is_mandatory = <True>
            >
            ["time_validity"] = (BMM_GENERIC_PROPERTY) <
                name = <"time_validity">
                type = <
                    root_type = </class_definitions["DV_INTERVAL"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["DV_DATE"]>
                    >
                >
            >
        >
    >

    ["PARTY_RELATIONSHIP"] = <
        name = <"PARTY_RELATIONSHIP">
        ancestors = </class_definitions["LOCATABLE"], ...>
        properties = <
            ["source"] = (BMM_SINGLE_PROPERTY) <
                name = <"source">
                type = </class_definitions["PARTY_REF"]>
            >
            ["target"] = (BMM_SINGLE_PROPERTY) <
                name = <"target">
                type = </class_definitions["PARTY_REF"]>
            >
            ["details"] = (BMM_SINGLE_PROPERTY) <
                name = <"details">
                type = </class_definitions["ITEM_STRUCTURE"]>
            >
            ["time_validity"] = (BMM_GENERIC_PROPERTY) <
                name = <"time_validity">
                type = <
                    root_type = </class_definitions["DV_INTERVAL"]>
                    generic_parameters = <
                        ["T"] = </class_definitions["DV_DATE"]>
                    >
                >
            >
        >
    >
>
