    rm_publisher = <"openehr">
    rm_release = <"1.0.3">
    packages = <
        ["ORG.OPENEHR.RM.EHR"] = <
            name = <"org.openehr.rm.ehr">
            classes = <"EHR", "EHR_ACCESS", "EHR_STATUS", "ACCESS_CONTROL_SETTINGS">
        >
        ["ORG.OPENEHR.RM.COMPOSITION"] = <
            packages = <
                ["CONTENT"] = <
                    packages = <
                        ["NAVIGATION"] = <
                            name = <"navigation">
                            classes = <"SECTION", ...>
                        >
                        ["ENTRY"] = <
                            name = <"entry">
                            classes = <"ENTRY", "CARE_ENTRY", "ADMIN_ENTRY", "OBSERVATION", "EVALUATION", "INSTRUCTION", "ACTION", "ACTIVITY", "ISM_TRANSITION", "INSTRUCTION_DETAILS">
                        >
                        ["INTEGRATION"] = <
                            name = <"integration">
                            classes = <"GENERIC_ENTRY", ...>
                        >
                    >
                    name = <"content">
                    classes = <"CONTENT_ITEM", ...>
                >
            >
            name = <"org.openehr.rm.composition">
            classes = <"COMPOSITION", "EVENT_CONTEXT">
        >
    >
    schema_name = <"ehr">
    schema_revision = <"1.0.3.1">
    schema_lifecycle_state = <"stable">
    schema_author = <"Thomas Beale <thomas.beale@openehr.org>">
    schema_description = <"openEHR Release 1.0.3 EHR schema">
    bmm_version = <"2.3">
    model_name = <"EHR">
    includes = <
        ["1"] = <
            id = <"openehr_structures_1.0.3">
        >
    >
    primitive_types = <
        ["Any"] = <
            name = <"Any">
            source_schema_id = <"openehr_primitive_types_1.0.3">
            is_abstract = <True>
            uid = <396>
        >
        ["Ordered"] = <
            documentation = <"Ancestor of types with total order relation defined, i.e. '<' and '='.">
            name = <"Ordered">
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            is_abstract = <True>
            uid = <397>
        >
        ["Numeric"] = <
            documentation = <"Ancestor of numeric types.">
            name = <"Numeric">
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            is_abstract = <True>
            uid = <398>
        >
        ["Ordered_Numeric"] = <
            documentation = <"Ancestor of ordered numeric types.">
            name = <"Ordered_Numeric">
            ancestors = <"Numeric", "Ordered">
            source_schema_id = <"openehr_primitive_types_1.0.3">
            is_abstract = <True>
            uid = <399>
        >
        ["Byte"] = <
            name = <"Byte">
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <400>
        >
        ["Octet"] = <
            name = <"Octet">
            ancestors = <"Ordered", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <401>
        >
        ["Boolean"] = <
            name = <"Boolean">
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <402>
        >
        ["Integer"] = <
            name = <"Integer">
            ancestors = <"Ordered_Numeric", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <403>
        >
        ["Integer64"] = <
            name = <"Integer64">
            ancestors = <"Ordered_Numeric", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <404>
        >
        ["Real"] = <
            name = <"Real">
            ancestors = <"Ordered_Numeric", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <405>
        >
        ["Double"] = <
            name = <"Double">
            ancestors = <"Ordered_Numeric", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <406>
        >
        ["Character"] = <
            name = <"Character">
            ancestors = <"Ordered", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <407>
        >
        ["String"] = <
            name = <"String">
            ancestors = <"Ordered", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <408>
        >
        ["List"] = <
            name = <"List">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <409>
        >
        ["Array"] = <
            name = <"Array">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <410>
        >
        ["Set"] = <
            name = <"Set">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <411>
        >
        ["Interval"] = <
            documentation = <"Type defining an interval of any ordered type.">
            name = <"Interval">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                    conforms_to_type = <"Ordered">
                >
            >
            ancestors = <"Any", ...>
            properties = <
                ["lower"] = (P_BMM_SINGLE_PROPERTY_OPEN) <
                    name = <"lower">
                    type = <"T">
                >
                ["upper"] = (P_BMM_SINGLE_PROPERTY_OPEN) <
                    name = <"upper">
                    type = <"T">
                >
                ["lower_unbounded"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"lower_unbounded">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
                ["upper_unbounded"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"upper_unbounded">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
                ["lower_included"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"lower_included">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
                ["upper_included"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"upper_included">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <412>
        >
        ["Hash"] = <
            documentation = <"Type defining Hash table / hash map structure, whose type parameters V and K represent a value type and an Ordered key type respectively.">
            name = <"Hash">
            generic_parameter_defs = <
                ["V"] = <
                    name = <"V">
                >
                ["K"] = <
                    name = <"K">
                    conforms_to_type = <"Ordered">
                >
            >
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <413>
        >
        ["ISO8601_TYPE"] = <
            name = <"ISO8601_TYPE">
            ancestors = <"Ordered", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <414>
        >
        ["ISO8601_DATE"] = <
            documentation = <"Date type based on IS8601 representation.">
            name = <"ISO8601_DATE">
            ancestors = <"ISO8601_TYPE", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <415>
        >
        ["ISO8601_TIME"] = <
            documentation = <"Time type based on IS8601 representation.">
            name = <"ISO8601_TIME">
            ancestors = <"ISO8601_TYPE", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <416>
        >
        ["ISO8601_DATE_TIME"] = <
            documentation = <"Date Time type based on IS8601 representation.">
            name = <"ISO8601_DATE_TIME">
            ancestors = <"ISO8601_TYPE", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <417>
        >
        ["ISO8601_DURATION"] = <
            documentation = <"Duration type based on IS8601 representation.">
            name = <"ISO8601_DURATION">
            ancestors = <"ISO8601_TYPE", ...>
            source_schema_id = <"openehr_primitive_types_1.0.3">
            uid = <418>
        >
    >
    class_definitions = <
        ["EHR"] = <
            name = <"EHR">
            properties = <
                ["system_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"system_id">
                    type = <"HIER_OBJECT_ID">
                    is_mandatory = <True>
                >
                ["ehr_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"ehr_id">
                    type = <"HIER_OBJECT_ID">
                    is_mandatory = <True>
                >
                ["time_created"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"time_created">
                    type = <"DV_DATE_TIME">
                    is_mandatory = <True>
                >
                ["ehr_access"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"ehr_access">
                    type = <"OBJECT_REF">
                    is_mandatory = <True>
                >
                ["ehr_status"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"ehr_status">
                    type = <"OBJECT_REF">
                    is_mandatory = <True>
                >
                ["directory"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"directory">
                    type = <"OBJECT_REF">
                >
                ["compositions"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"compositions">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"OBJECT_REF">
                        container_type = <"List">
                    >
                >
                ["contributions"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"contributions">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"OBJECT_REF">
                        container_type = <"List">
                    >
                >
                ["most_recent_composition"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"most_recent_composition">
                    type = <"COMPOSITION">
                    is_computed = <True>
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <348>
        >
        ["EHR_ACCESS"] = <
            name = <"EHR_ACCESS">
            ancestors = <"LOCATABLE", ...>
            properties = <
                ["settings"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"settings">
                    type = <"ACCESS_CONTROL_SETTINGS">
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <349>
        >
        ["ACCESS_CONTROL_SETTINGS"] = <
            name = <"ACCESS_CONTROL_SETTINGS">
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_ehr_1.0.3">
            is_abstract = <True>
            uid = <350>
        >
        ["EHR_STATUS"] = <
            name = <"EHR_STATUS">
            ancestors = <"LOCATABLE", ...>
            properties = <
                ["subject"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"subject">
                    type = <"PARTY_SELF">
                    is_mandatory = <True>
                >
                ["is_queryable"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"is_queryable">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
                ["is_modifiable"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"is_modifiable">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
                ["other_details"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"other_details">
                    type = <"ITEM_STRUCTURE">
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <351>
        >
        ["COMPOSITION"] = <
            name = <"COMPOSITION">
            ancestors = <"LOCATABLE", ...>
            properties = <
                ["language"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"language">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
                ["territory"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"territory">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
                ["category"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"category">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
                ["composer"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"composer">
                    type = <"PARTY_PROXY">
                    is_mandatory = <True>
                >
                ["context"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"context">
                    type = <"EVENT_CONTEXT">
                >
                ["content"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"content">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"CONTENT_ITEM">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <352>
        >
        ["EVENT_CONTEXT"] = <
            name = <"EVENT_CONTEXT">
            ancestors = <"PATHABLE", ...>
            properties = <
                ["health_care_facility"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"health_care_facility">
                    type = <"PARTY_IDENTIFIED">
                >
                ["start_time"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"start_time">
                    type = <"DV_DATE_TIME">
                    is_mandatory = <True>
                >
                ["end_time"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"end_time">
                    type = <"DV_DATE_TIME">
                >
                ["participations"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"participations">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"PARTICIPATION">
                        container_type = <"List">
                    >
                >
                ["location"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"location">
                    type = <"String">
                >
                ["setting"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"setting">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
                ["other_context"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"other_context">
                    type = <"ITEM_STRUCTURE">
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <353>
        >
        ["CONTENT_ITEM"] = <
            name = <"CONTENT_ITEM">
            ancestors = <"LOCATABLE", ...>
            source_schema_id = <"openehr_ehr_1.0.3">
            is_abstract = <True>
            uid = <354>
        >
        ["SECTION"] = <
            name = <"SECTION">
            ancestors = <"CONTENT_ITEM", ...>
            properties = <
                ["items"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"items">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"CONTENT_ITEM">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <355>
        >
        ["ENTRY"] = <
            name = <"ENTRY">
            ancestors = <"CONTENT_ITEM", ...>
            properties = <
                ["language"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"language">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["encoding"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"encoding">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["subject"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"subject">
                    type = <"PARTY_PROXY">
                    is_mandatory = <True>
                >
                ["provider"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"provider">
                    type = <"PARTY_PROXY">
                >
                ["other_participations"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"other_participations">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"PARTICIPATION">
                        container_type = <"List">
                    >
                >
                ["workflow_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"workflow_id">
                    type = <"OBJECT_REF">
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            is_abstract = <True>
            uid = <356>
        >
        ["ADMIN_ENTRY"] = <
            name = <"ADMIN_ENTRY">
            ancestors = <"ENTRY", ...>
            properties = <
                ["data"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"data">
                    type = <"ITEM_STRUCTURE">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <357>
        >
        ["CARE_ENTRY"] = <
            documentation = <"Abstract ENTRY subtype corresponding to any type of ENTRY in the clinical care cycle.">
            name = <"CARE_ENTRY">
            ancestors = <"ENTRY", ...>
            properties = <
                ["protocol"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"protocol">
                    type = <"ITEM_STRUCTURE">
                >
                ["guideline_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"guideline_id">
                    type = <"OBJECT_REF">
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            is_abstract = <True>
            uid = <358>
        >
        ["OBSERVATION"] = <
            documentation = <"ENTRY subtype used to represent observation information in time, as either a single or multiple samples.">
            name = <"OBSERVATION">
            ancestors = <"CARE_ENTRY", ...>
            properties = <
                ["data"] = (P_BMM_GENERIC_PROPERTY) <
                    documentation = <"Data of the observation, in the form of a HISTORY of EVENTs.">
                    name = <"data">
                    type_def = <
                        root_type = <"HISTORY">
                        generic_parameters = <"ITEM_STRUCTURE", ...>
                    >
                    is_mandatory = <True>
                >
                ["state"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"state">
                    type_def = <
                        root_type = <"HISTORY">
                        generic_parameters = <"ITEM_STRUCTURE", ...>
                    >
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <359>
        >
        ["EVALUATION"] = <
            name = <"EVALUATION">
            ancestors = <"CARE_ENTRY", ...>
            properties = <
                ["data"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"data">
                    type = <"ITEM_STRUCTURE">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <360>
        >
        ["INSTRUCTION"] = <
            name = <"INSTRUCTION">
            ancestors = <"CARE_ENTRY", ...>
            properties = <
                ["narrative"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"narrative">
                    type = <"DV_TEXT">
                    is_mandatory = <True>
                >
                ["expiry_time"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"expiry_time">
                    type = <"DV_DATE_TIME">
                >
                ["wf_definition"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"wf_definition">
                    type = <"DV_PARSABLE">
                    is_im_runtime = <True>
                >
                ["activities"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"activities">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"ACTIVITY">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <361>
        >
        ["ACTIVITY"] = <
            name = <"ACTIVITY">
            ancestors = <"LOCATABLE", ...>
            properties = <
                ["description"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"description">
                    type = <"ITEM_STRUCTURE">
                    is_mandatory = <True>
                >
                ["timing"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"timing">
                    type = <"DV_PARSABLE">
                    is_mandatory = <True>
                >
                ["action_archetype_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"action_archetype_id">
                    type = <"String">
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <362>
        >
        ["ACTION"] = <
            name = <"ACTION">
            ancestors = <"CARE_ENTRY", ...>
            properties = <
                ["time"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"time">
                    type = <"DV_DATE_TIME">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
                ["description"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"description">
                    type = <"ITEM_STRUCTURE">
                    is_mandatory = <True>
                >
                ["ism_transition"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"ism_transition">
                    type = <"ISM_TRANSITION">
                    is_mandatory = <True>
                >
                ["instruction_details"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"instruction_details">
                    type = <"INSTRUCTION_DETAILS">
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <363>
        >
        ["INSTRUCTION_DETAILS"] = <
            name = <"INSTRUCTION_DETAILS">
            ancestors = <"PATHABLE", ...>
            properties = <
                ["instruction_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"instruction_id">
                    type = <"LOCATABLE_REF">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
                ["wf_details"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"wf_details">
                    type = <"ITEM_STRUCTURE">
                    is_im_runtime = <True>
                >
                ["activity_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"activity_id">
                    type = <"String">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <364>
        >
        ["ISM_TRANSITION"] = <
            name = <"ISM_TRANSITION">
            ancestors = <"PATHABLE", ...>
            properties = <
                ["current_state"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"current_state">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
                ["transition"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"transition">
                    type = <"DV_CODED_TEXT">
                >
                ["careflow_step"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"careflow_step">
                    type = <"DV_CODED_TEXT">
                >
                ["reason"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"reason">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"DV_TEXT">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <365>
        >
        ["GENERIC_ENTRY"] = <
            name = <"GENERIC_ENTRY">
            ancestors = <"CONTENT_ITEM", ...>
            properties = <
                ["data"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"data">
                    type = <"ITEM_TREE">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_ehr_1.0.3">
            uid = <366>
        >
        ["DATA_STRUCTURE"] = <
            name = <"DATA_STRUCTURE">
            ancestors = <"LOCATABLE", ...>
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <419>
        >
        ["ITEM_STRUCTURE"] = <
            name = <"ITEM_STRUCTURE">
            ancestors = <"DATA_STRUCTURE", ...>
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <420>
        >
        ["ITEM_SINGLE"] = <
            name = <"ITEM_SINGLE">
            ancestors = <"ITEM_STRUCTURE", ...>
            properties = <
                ["item"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"item">
                    type = <"ELEMENT">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <421>
        >
        ["ITEM_LIST"] = <
            name = <"ITEM_LIST">
            ancestors = <"ITEM_STRUCTURE", ...>
            properties = <
                ["items"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"items">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"ELEMENT">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <422>
        >
        ["ITEM_TABLE"] = <
            name = <"ITEM_TABLE">
            ancestors = <"ITEM_STRUCTURE", ...>
            properties = <
                ["rows"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"rows">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"CLUSTER">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <423>
        >
        ["ITEM_TREE"] = <
            name = <"ITEM_TREE">
            ancestors = <"ITEM_STRUCTURE", ...>
            properties = <
                ["items"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"items">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"ITEM">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <424>
        >
        ["ITEM"] = <
            name = <"ITEM">
            ancestors = <"LOCATABLE", ...>
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <425>
        >
        ["CLUSTER"] = <
            name = <"CLUSTER">
            ancestors = <"ITEM", ...>
            properties = <
                ["items"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"items">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"ITEM">
                        container_type = <"List">
                    >
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <426>
        >
        ["ELEMENT"] = <
            name = <"ELEMENT">
            ancestors = <"ITEM", ...>
            properties = <
                ["null_flavour"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"null_flavour">
                    type = <"DV_CODED_TEXT">
                >
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"DATA_VALUE">
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <427>
        >
        ["HISTORY"] = <
            name = <"HISTORY">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                    conforms_to_type = <"ITEM_STRUCTURE">
                >
            >
            ancestors = <"DATA_STRUCTURE", ...>
            properties = <
                ["origin"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"origin">
                    type = <"DV_DATE_TIME">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
                ["period"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"period">
                    type = <"DV_DURATION">
                >
                ["duration"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"duration">
                    type = <"DV_DURATION">
                >
                ["summary"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"summary">
                    type = <"ITEM_STRUCTURE">
                >
                ["events"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"events">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"EVENT">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <428>
        >
        ["EVENT"] = <
            name = <"EVENT">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                    conforms_to_type = <"ITEM_STRUCTURE">
                >
            >
            ancestors = <"LOCATABLE", ...>
            properties = <
                ["time"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"time">
                    type = <"DV_DATE_TIME">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
                ["state"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"state">
                    type = <"ITEM_STRUCTURE">
                >
                ["data"] = (P_BMM_SINGLE_PROPERTY_OPEN) <
                    name = <"data">
                    type = <"T">
                    is_mandatory = <True>
                >
                ["offset"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"offset">
                    type = <"DV_DURATION">
                    is_computed = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <429>
        >
        ["POINT_EVENT"] = <
            name = <"POINT_EVENT">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"EVENT", ...>
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <430>
        >
        ["INTERVAL_EVENT"] = <
            name = <"INTERVAL_EVENT">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"EVENT", ...>
            properties = <
                ["width"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"width">
                    type = <"DV_DURATION">
                    is_mandatory = <True>
                >
                ["sample_count"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"sample_count">
                    type = <"Integer">
                    is_im_runtime = <True>
                >
                ["math_function"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"math_function">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <431>
        >
        ["REVISION_HISTORY"] = <
            name = <"REVISION_HISTORY">
            ancestors = <"Any", ...>
            properties = <
                ["items"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"items">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"REVISION_HISTORY_ITEM">
                        container_type = <"List">
                    >
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <432>
        >
        ["REVISION_HISTORY_ITEM"] = <
            name = <"REVISION_HISTORY_ITEM">
            ancestors = <"Any", ...>
            properties = <
                ["version_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"version_id">
                    type = <"OBJECT_VERSION_ID">
                    is_mandatory = <True>
                >
                ["audits"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"audits">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"AUDIT_DETAILS">
                        container_type = <"List">
                    >
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <433>
        >
        ["AUDIT_DETAILS"] = <
            name = <"AUDIT_DETAILS">
            ancestors = <"Any", ...>
            properties = <
                ["system_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"system_id">
                    type = <"String">
                    is_mandatory = <True>
                >
                ["time_committed"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"time_committed">
                    type = <"DV_DATE_TIME">
                    is_mandatory = <True>
                >
                ["change_type"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"change_type">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
                ["description"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"description">
                    type = <"DV_TEXT">
                >
                ["committer"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"committer">
                    type = <"PARTY_PROXY">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <434>
        >
        ["ATTESTATION"] = <
            name = <"ATTESTATION">
            ancestors = <"AUDIT_DETAILS", ...>
            properties = <
                ["attested_view"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"attested_view">
                    type = <"DV_MULTIMEDIA">
                >
                ["proof"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"proof">
                    type = <"String">
                >
                ["items"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"items">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"DV_EHR_URI">
                        container_type = <"List">
                    >
                >
                ["reason"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"reason">
                    type = <"DV_TEXT">
                    is_mandatory = <True>
                >
                ["is_pending"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"is_pending">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <435>
        >
        ["PARTICIPATION"] = <
            name = <"PARTICIPATION">
            ancestors = <"Any", ...>
            properties = <
                ["function"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"function">
                    type = <"DV_TEXT">
                    is_mandatory = <True>
                >
                ["time"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"time">
                    type_def = <
                        root_type = <"DV_INTERVAL">
                        generic_parameters = <"DV_DATE_TIME", ...>
                    >
                    is_im_runtime = <True>
                >
                ["mode"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"mode">
                    type = <"DV_CODED_TEXT">
                >
                ["performer"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"performer">
                    type = <"PARTY_PROXY">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <436>
        >
        ["PARTY_PROXY"] = <
            name = <"PARTY_PROXY">
            ancestors = <"Any", ...>
            properties = <
                ["external_ref"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"external_ref">
                    type = <"PARTY_REF">
                    is_im_infrastructure = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <437>
        >
        ["PARTY_IDENTIFIED"] = <
            name = <"PARTY_IDENTIFIED">
            ancestors = <"PARTY_PROXY", ...>
            properties = <
                ["name"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"name">
                    type = <"String">
                >
                ["identifiers"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"identifiers">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"DV_IDENTIFIER">
                        container_type = <"List">
                    >
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <438>
        >
        ["PARTY_RELATED"] = <
            name = <"PARTY_RELATED">
            ancestors = <"PARTY_IDENTIFIED", ...>
            properties = <
                ["relationship"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"relationship">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <439>
        >
        ["PARTY_SELF"] = <
            name = <"PARTY_SELF">
            ancestors = <"PARTY_PROXY", ...>
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <440>
        >
        ["PATHABLE"] = <
            name = <"PATHABLE">
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <441>
        >
        ["LOCATABLE"] = <
            name = <"LOCATABLE">
            ancestors = <"PATHABLE", ...>
            properties = <
                ["uid"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"uid">
                    type = <"UID_BASED_ID">
                    is_im_infrastructure = <True>
                >
                ["archetype_node_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"archetype_node_id">
                    type = <"String">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["name"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"name">
                    type = <"DV_TEXT">
                    is_mandatory = <True>
                >
                ["archetype_details"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"archetype_details">
                    type = <"ARCHETYPED">
                    is_im_infrastructure = <True>
                >
                ["feeder_audit"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"feeder_audit">
                    type = <"FEEDER_AUDIT">
                    is_im_runtime = <True>
                >
                ["links"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"links">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"LINK">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <442>
        >
        ["LINK"] = <
            name = <"LINK">
            ancestors = <"Any", ...>
            properties = <
                ["meaning"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"meaning">
                    type = <"DV_TEXT">
                    is_mandatory = <True>
                >
                ["type"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"type">
                    type = <"DV_TEXT">
                    is_mandatory = <True>
                >
                ["target"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"target">
                    type = <"DV_EHR_URI">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <443>
        >
        ["ARCHETYPED"] = <
            name = <"ARCHETYPED">
            ancestors = <"Any", ...>
            properties = <
                ["archetype_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"archetype_id">
                    type = <"ARCHETYPE_ID">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["template_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"template_id">
                    type = <"TEMPLATE_ID">
                    is_im_infrastructure = <True>
                >
                ["rm_version"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"rm_version">
                    type = <"String">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <444>
        >
        ["FEEDER_AUDIT"] = <
            name = <"FEEDER_AUDIT">
            ancestors = <"Any", ...>
            properties = <
                ["originating_system_item_ids"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"originating_system_item_ids">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"DV_IDENTIFIER">
                        container_type = <"List">
                    >
                    is_im_runtime = <True>
                >
                ["feeder_system_item_ids"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"feeder_system_item_ids">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"DV_IDENTIFIER">
                        container_type = <"List">
                    >
                    is_im_runtime = <True>
                >
                ["original_content"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"original_content">
                    type = <"DV_ENCAPSULATED">
                    is_im_runtime = <True>
                >
                ["originating_system_audit"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"originating_system_audit">
                    type = <"FEEDER_AUDIT_DETAILS">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
                ["feeder_system_audit"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"feeder_system_audit">
                    type = <"FEEDER_AUDIT_DETAILS">
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <445>
        >
        ["FEEDER_AUDIT_DETAILS"] = <
            name = <"FEEDER_AUDIT_DETAILS">
            ancestors = <"Any", ...>
            properties = <
                ["system_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"system_id">
                    type = <"String">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
                ["location"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"location">
                    type = <"PARTY_IDENTIFIED">
                    is_im_runtime = <True>
                >
                ["provider"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"provider">
                    type = <"PARTY_IDENTIFIED">
                    is_im_runtime = <True>
                >
                ["subject"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"subject">
                    type = <"PARTY_PROXY">
                    is_im_runtime = <True>
                >
                ["time"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"time">
                    type = <"DV_DATE_TIME">
                    is_im_runtime = <True>
                >
                ["version_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"version_id">
                    type = <"String">
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <446>
        >
        ["FOLDER"] = <
            name = <"FOLDER">
            ancestors = <"LOCATABLE", ...>
            properties = <
                ["folders"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"folders">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"FOLDER">
                        container_type = <"List">
                    >
                >
                ["items"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"items">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"OBJECT_REF">
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <447>
        >
        ["CONTRIBUTION"] = <
            name = <"CONTRIBUTION">
            ancestors = <"Any", ...>
            properties = <
                ["uid"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"uid">
                    type = <"HIER_OBJECT_ID">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["audit"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"audit">
                    type = <"AUDIT_DETAILS">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["versions"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"versions">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"OBJECT_REF">
                        container_type = <"List">
                    >
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <448>
        >
        ["VERSIONED_OBJECT"] = <
            name = <"VERSIONED_OBJECT">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"Any", ...>
            properties = <
                ["uid"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"uid">
                    type = <"HIER_OBJECT_ID">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["owner_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"owner_id">
                    type = <"OBJECT_REF">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["time_created"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"time_created">
                    type = <"DV_DATE_TIME">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <449>
        >
        ["VERSION"] = <
            name = <"VERSION">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"Any", ...>
            properties = <
                ["contribution"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"contribution">
                    type = <"OBJECT_REF">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["commit_audit"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"commit_audit">
                    type = <"AUDIT_DETAILS">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["signature"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"signature">
                    type = <"String">
                    is_im_infrastructure = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <450>
        >
        ["ORIGINAL_VERSION"] = <
            name = <"ORIGINAL_VERSION">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"VERSION", ...>
            properties = <
                ["uid"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"uid">
                    type = <"OBJECT_VERSION_ID">
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
                ["preceding_version_uid"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"preceding_version_uid">
                    type = <"OBJECT_VERSION_ID">
                    is_im_infrastructure = <True>
                >
                ["other_input_version_uids"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"other_input_version_uids">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"OBJECT_VERSION_ID">
                        container_type = <"List">
                    >
                    is_im_infrastructure = <True>
                >
                ["attestations"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"attestations">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"ATTESTATION">
                        container_type = <"List">
                    >
                >
                ["lifecycle_state"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"lifecycle_state">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
                ["data"] = (P_BMM_SINGLE_PROPERTY_OPEN) <
                    name = <"data">
                    type = <"T">
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <451>
        >
        ["IMPORTED_VERSION"] = <
            name = <"IMPORTED_VERSION">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                >
            >
            ancestors = <"VERSION", ...>
            properties = <
                ["item"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"item">
                    type_def = <
                        root_type = <"ORIGINAL_VERSION">
                        generic_parameters = <"T", ...>
                    >
                    is_mandatory = <True>
                    is_im_infrastructure = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <452>
        >
        ["AUTHORED_RESOURCE"] = <
            name = <"AUTHORED_RESOURCE">
            ancestors = <"Any", ...>
            properties = <
                ["original_language"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"original_language">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
                ["is_controlled"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"is_controlled">
                    type = <"Boolean">
                >
                ["translations"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"translations">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"TRANSLATION_DETAILS">
                        container_type = <"List">
                    >
                >
                ["description"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"description">
                    type = <"RESOURCE_DESCRIPTION">
                >
                ["revision_history"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"revision_history">
                    type = <"REVISION_HISTORY">
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            is_abstract = <True>
            uid = <453>
        >
        ["TRANSLATION_DETAILS"] = <
            name = <"TRANSLATION_DETAILS">
            ancestors = <"Any", ...>
            properties = <
                ["language"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"language">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
                ["author"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"author">
                    type_def = <
                        root_type = <"Hash">
                        generic_parameters = <"String", "String">
                    >
                    is_mandatory = <True>
                >
                ["accreditation"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"accreditation">
                    type = <"String">
                >
                ["other_details"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"other_details">
                    type_def = <
                        root_type = <"Hash">
                        generic_parameters = <"String", "String">
                    >
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <454>
        >
        ["RESOURCE_DESCRIPTION"] = <
            name = <"RESOURCE_DESCRIPTION">
            ancestors = <"Any", ...>
            properties = <
                ["original_author"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"original_author">
                    type_def = <
                        root_type = <"Hash">
                        generic_parameters = <"String", "String">
                    >
                    is_mandatory = <True>
                >
                ["other_contributors"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"other_contributors">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"String">
                        container_type = <"List">
                    >
                >
                ["lifecycle_state"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"lifecycle_state">
                    type = <"String">
                    is_mandatory = <True>
                >
                ["resource_package_uri"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"resource_package_uri">
                    type = <"String">
                >
                ["other_details"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"other_details">
                    type_def = <
                        root_type = <"Hash">
                        generic_parameters = <"String", "String">
                    >
                >
                ["parent_resource"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"parent_resource">
                    type = <"AUTHORED_RESOURCE">
                    is_mandatory = <True>
                >
                ["details"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"details">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"RESOURCE_DESCRIPTION_ITEM">
                        container_type = <"List">
                    >
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <455>
        >
        ["RESOURCE_DESCRIPTION_ITEM"] = <
            name = <"RESOURCE_DESCRIPTION_ITEM">
            ancestors = <"Any", ...>
            properties = <
                ["language"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"language">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
                ["purpose"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"purpose">
                    type = <"String">
                    is_mandatory = <True>
                >
                ["keywords"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"keywords">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"String">
                        container_type = <"List">
                    >
                >
                ["use"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"use">
                    type = <"String">
                >
                ["misuse"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"misuse">
                    type = <"String">
                >
                ["copyright"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"copyright">
                    type = <"String">
                >
                ["original_resource_uri"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"original_resource_uri">
                    cardinality = <|>=0|>
                    type_def = <
                        type_def = (P_BMM_GENERIC_TYPE) <
                            root_type = <"Hash">
                            generic_parameters = <"String", "String">
                        >
                        container_type = <"List">
                    >
                >
                ["other_details"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"other_details">
                    type_def = <
                        root_type = <"Hash">
                        generic_parameters = <"String", "String">
                    >
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_structures_1.0.3">
            uid = <456>
        >
        ["OBJECT_REF"] = <
            name = <"OBJECT_REF">
            ancestors = <"Any", ...>
            properties = <
                ["id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"id">
                    type = <"OBJECT_ID">
                    is_mandatory = <True>
                >
                ["namespace"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"namespace">
                    type = <"String">
                    is_mandatory = <True>
                >
                ["type"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"type">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <286>
        >
        ["LOCATABLE_REF"] = <
            name = <"LOCATABLE_REF">
            ancestors = <"OBJECT_REF", ...>
            properties = <
                ["id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"id">
                    type = <"UID_BASED_ID">
                    is_mandatory = <True>
                >
                ["path"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"path">
                    type = <"String">
                    is_im_infrastructure = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <287>
        >
        ["PARTY_REF"] = <
            name = <"PARTY_REF">
            ancestors = <"OBJECT_REF", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <288>
        >
        ["ACCESS_GROUP_REF"] = <
            name = <"ACCESS_GROUP_REF">
            ancestors = <"OBJECT_REF", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <289>
        >
        ["OBJECT_ID"] = <
            name = <"OBJECT_ID">
            ancestors = <"Any", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <290>
        >
        ["TERMINOLOGY_ID"] = <
            name = <"TERMINOLOGY_ID">
            ancestors = <"OBJECT_ID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <291>
        >
        ["UID_BASED_ID"] = <
            name = <"UID_BASED_ID">
            ancestors = <"OBJECT_ID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <292>
        >
        ["GENERIC_ID"] = <
            name = <"GENERIC_ID">
            ancestors = <"OBJECT_ID", ...>
            properties = <
                ["scheme"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"scheme">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <293>
        >
        ["ARCHETYPE_ID"] = <
            name = <"ARCHETYPE_ID">
            ancestors = <"OBJECT_ID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <294>
        >
        ["TEMPLATE_ID"] = <
            name = <"TEMPLATE_ID">
            ancestors = <"OBJECT_ID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <295>
        >
        ["OBJECT_VERSION_ID"] = <
            name = <"OBJECT_VERSION_ID">
            ancestors = <"UID_BASED_ID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <296>
        >
        ["HIER_OBJECT_ID"] = <
            name = <"HIER_OBJECT_ID">
            ancestors = <"UID_BASED_ID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <297>
        >
        ["VERSION_TREE_ID"] = <
            name = <"VERSION_TREE_ID">
            ancestors = <"Any", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <298>
        >
        ["UID"] = <
            name = <"UID">
            ancestors = <"Any", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <299>
        >
        ["INTERNET_ID"] = <
            name = <"INTERNET_ID">
            ancestors = <"UID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <300>
        >
        ["UUID"] = <
            name = <"UUID">
            ancestors = <"UID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <301>
        >
        ["ISO_OID"] = <
            name = <"ISO_OID">
            ancestors = <"UID", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <302>
        >
        ["DATA_VALUE"] = <
            name = <"DATA_VALUE">
            ancestors = <"Any", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <303>
        >
        ["DV_BOOLEAN"] = <
            name = <"DV_BOOLEAN">
            ancestors = <"DATA_VALUE", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <304>
        >
        ["DV_IDENTIFIER"] = <
            name = <"DV_IDENTIFIER">
            ancestors = <"DATA_VALUE", ...>
            properties = <
                ["issuer"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"issuer">
                    type = <"String">
                >
                ["id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"id">
                    type = <"String">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
                ["type"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"type">
                    type = <"String">
                >
                ["assigner"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"assigner">
                    type = <"String">
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <305>
        >
        ["DV_STATE"] = <
            name = <"DV_STATE">
            ancestors = <"DATA_VALUE", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
                ["is_terminal"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"is_terminal">
                    type = <"Boolean">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <306>
        >
        ["TERM_MAPPING"] = <
            name = <"TERM_MAPPING">
            properties = <
                ["match"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"match">
                    type = <"Character">
                    is_mandatory = <True>
                >
                ["purpose"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"purpose">
                    type = <"DV_CODED_TEXT">
                >
                ["target"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"target">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <307>
        >
        ["DV_TEXT"] = <
            name = <"DV_TEXT">
            ancestors = <"DATA_VALUE", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
                ["hyperlink"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"hyperlink">
                    type = <"DV_URI">
                    is_im_runtime = <True>
                >
                ["language"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"language">
                    type = <"CODE_PHRASE">
                    is_im_infrastructure = <True>
                >
                ["encoding"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"encoding">
                    type = <"CODE_PHRASE">
                    is_im_infrastructure = <True>
                >
                ["formatting"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"formatting">
                    type = <"String">
                    is_im_runtime = <True>
                >
                ["mappings"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"mappings">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"TERM_MAPPING">
                        container_type = <"List">
                    >
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <308>
        >
        ["DV_CODED_TEXT"] = <
            name = <"DV_CODED_TEXT">
            ancestors = <"DV_TEXT", ...>
            properties = <
                ["defining_code"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"defining_code">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <309>
        >
        ["CODE_PHRASE"] = <
            name = <"CODE_PHRASE">
            properties = <
                ["terminology_id"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"terminology_id">
                    type = <"TERMINOLOGY_ID">
                    is_mandatory = <True>
                >
                ["code_string"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"code_string">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <310>
        >
        ["DV_PARAGRAPH"] = <
            name = <"DV_PARAGRAPH">
            ancestors = <"DATA_VALUE", ...>
            properties = <
                ["items"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"items">
                    cardinality = <|>=1|>
                    type_def = <
                        type = <"DV_TEXT">
                        container_type = <"List">
                    >
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <311>
        >
        ["DV_INTERVAL"] = <
            name = <"DV_INTERVAL">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                    conforms_to_type = <"DV_ORDERED">
                >
            >
            ancestors = <"Interval", "DATA_VALUE">
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <312>
        >
        ["REFERENCE_RANGE"] = <
            name = <"REFERENCE_RANGE">
            generic_parameter_defs = <
                ["T"] = <
                    name = <"T">
                    conforms_to_type = <"DV_ORDERED">
                >
            >
            ancestors = <"Any", ...>
            properties = <
                ["range"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"range">
                    type_def = <
                        root_type = <"DV_INTERVAL">
                        generic_parameters = <"T", ...>
                    >
                    is_mandatory = <True>
                >
                ["meaning"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"meaning">
                    type = <"DV_TEXT">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <313>
        >
        ["DV_ORDERED"] = <
            name = <"DV_ORDERED">
            ancestors = <"Ordered", "DATA_VALUE">
            properties = <
                ["normal_status"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"normal_status">
                    type = <"CODE_PHRASE">
                >
                ["normal_range"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"normal_range">
                    type_def = <
                        root_type = <"DV_INTERVAL">
                        generic_parameters = <"DV_ORDERED", ...>
                    >
                >
                ["other_reference_ranges"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"other_reference_ranges">
                    cardinality = <|>=1|>
                    type_def = <
                        type_def = (P_BMM_GENERIC_TYPE) <
                            root_type = <"REFERENCE_RANGE">
                            generic_parameters = <"DV_ORDERED", ...>
                        >
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <314>
        >
        ["DV_QUANTIFIED"] = <
            name = <"DV_QUANTIFIED">
            ancestors = <"DV_ORDERED", ...>
            properties = <
                ["magnitude_status"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"magnitude_status">
                    type = <"String">
                    is_im_runtime = <True>
                >
                ["accuracy"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"accuracy">
                    type = <"Any">
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <315>
        >
        ["DV_ORDINAL"] = <
            name = <"DV_ORDINAL">
            ancestors = <"DV_ORDERED", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"Integer">
                    is_mandatory = <True>
                >
                ["symbol"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"symbol">
                    type = <"DV_CODED_TEXT">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <316>
        >
        ["DV_AMOUNT"] = <
            name = <"DV_AMOUNT">
            ancestors = <"DV_QUANTIFIED", ...>
            properties = <
                ["accuracy"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"accuracy">
                    type = <"Real">
                    is_im_runtime = <True>
                >
                ["accuracy_is_percent"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"accuracy_is_percent">
                    type = <"Boolean">
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <317>
        >
        ["DV_ABSOLUTE_QUANTITY"] = <
            name = <"DV_ABSOLUTE_QUANTITY">
            ancestors = <"DV_QUANTIFIED", ...>
            properties = <
                ["accuracy"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"accuracy">
                    type = <"DV_AMOUNT">
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <318>
        >
        ["DV_QUANTITY"] = <
            name = <"DV_QUANTITY">
            ancestors = <"DV_AMOUNT", ...>
            properties = <
                ["magnitude"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"magnitude">
                    type = <"Real">
                    is_mandatory = <True>
                >
                ["property"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"property">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
                ["units"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"units">
                    type = <"String">
                    is_mandatory = <True>
                >
                ["precision"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"precision">
                    type = <"Integer">
                >
                ["normal_range"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"normal_range">
                    type_def = <
                        root_type = <"DV_INTERVAL">
                        generic_parameters = <"DV_QUANTITY", ...>
                    >
                >
                ["other_reference_ranges"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"other_reference_ranges">
                    type_def = <
                        type_def = (P_BMM_GENERIC_TYPE) <
                            root_type = <"REFERENCE_RANGE">
                            generic_parameters = <"DV_QUANTITY", ...>
                        >
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <319>
        >
        ["DV_COUNT"] = <
            name = <"DV_COUNT">
            ancestors = <"DV_AMOUNT", ...>
            properties = <
                ["magnitude"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"magnitude">
                    type = <"Integer64">
                    is_mandatory = <True>
                >
                ["normal_range"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"normal_range">
                    type_def = <
                        root_type = <"DV_INTERVAL">
                        generic_parameters = <"DV_COUNT", ...>
                    >
                >
                ["other_reference_ranges"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"other_reference_ranges">
                    type_def = <
                        type_def = (P_BMM_GENERIC_TYPE) <
                            root_type = <"REFERENCE_RANGE">
                            generic_parameters = <"DV_COUNT", ...>
                        >
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <320>
        >
        ["DV_PROPORTION"] = <
            name = <"DV_PROPORTION">
            ancestors = <"DV_AMOUNT", ...>
            properties = <
                ["numerator"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"numerator">
                    type = <"Real">
                    is_mandatory = <True>
                >
                ["denominator"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"denominator">
                    type = <"Real">
                    is_mandatory = <True>
                >
                ["type"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"type">
                    type = <"PROPORTION_KIND">
                    is_mandatory = <True>
                >
                ["precision"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"precision">
                    type = <"Integer">
                >
                ["is_integral"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"is_integral">
                    type = <"Boolean">
                    is_computed = <True>
                >
                ["normal_range"] = (P_BMM_GENERIC_PROPERTY) <
                    name = <"normal_range">
                    type_def = <
                        root_type = <"DV_INTERVAL">
                        generic_parameters = <"DV_PROPORTION", ...>
                    >
                >
                ["other_reference_ranges"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"other_reference_ranges">
                    cardinality = <|>=1|>
                    type_def = <
                        type_def = (P_BMM_GENERIC_TYPE) <
                            root_type = <"REFERENCE_RANGE">
                            generic_parameters = <"DV_PROPORTION", ...>
                        >
                        container_type = <"List">
                    >
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <321>
        >
        ["PROPORTION_KIND"] = (P_BMM_ENUMERATION_INTEGER) <
            name = <"PROPORTION_KIND">
            ancestors = <"Integer", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            item_names = <"pk_ratio", "pk_unitary", "pk_percent", "pk_fraction", "pk_integer_fraction">
            uid = <322>
        >
        ["DV_TEMPORAL"] = <
            name = <"DV_TEMPORAL">
            ancestors = <"DV_ABSOLUTE_QUANTITY", ...>
            properties = <
                ["accuracy"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"accuracy">
                    type = <"DV_DURATION">
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <323>
        >
        ["DV_DATE"] = <
            name = <"DV_DATE">
            ancestors = <"DV_TEMPORAL", "ISO8601_DATE">
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <324>
        >
        ["DV_TIME"] = <
            name = <"DV_TIME">
            ancestors = <"DV_TEMPORAL", "ISO8601_TIME">
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <325>
        >
        ["DV_DATE_TIME"] = <
            name = <"DV_DATE_TIME">
            ancestors = <"DV_TEMPORAL", "ISO8601_DATE_TIME">
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <326>
        >
        ["DV_DURATION"] = <
            name = <"DV_DURATION">
            ancestors = <"DV_AMOUNT", "ISO8601_DURATION">
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <327>
        >
        ["DV_ENCAPSULATED"] = <
            name = <"DV_ENCAPSULATED">
            ancestors = <"DATA_VALUE", ...>
            properties = <
                ["charset"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"charset">
                    type = <"CODE_PHRASE">
                    is_im_runtime = <True>
                >
                ["language"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"language">
                    type = <"CODE_PHRASE">
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <328>
        >
        ["DV_MULTIMEDIA"] = <
            name = <"DV_MULTIMEDIA">
            ancestors = <"DV_ENCAPSULATED", ...>
            properties = <
                ["alternate_text"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"alternate_text">
                    type = <"String">
                >
                ["uri"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"uri">
                    type = <"DV_URI">
                >
                ["data"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"data">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"Octet">
                        container_type = <"Array">
                    >
                >
                ["media_type"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"media_type">
                    type = <"CODE_PHRASE">
                    is_mandatory = <True>
                >
                ["compression_algorithm"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"compression_algorithm">
                    type = <"CODE_PHRASE">
                >
                ["integrity_check"] = (P_BMM_CONTAINER_PROPERTY) <
                    name = <"integrity_check">
                    cardinality = <|>=0|>
                    type_def = <
                        type = <"Octet">
                        container_type = <"Array">
                    >
                >
                ["integrity_check_algorithm"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"integrity_check_algorithm">
                    type = <"CODE_PHRASE">
                >
                ["thumbnail"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"thumbnail">
                    type = <"DV_MULTIMEDIA">
                >
                ["size"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"size">
                    type = <"Integer">
                    is_mandatory = <True>
                    is_im_runtime = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <329>
        >
        ["DV_PARSABLE"] = <
            name = <"DV_PARSABLE">
            ancestors = <"DV_ENCAPSULATED", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
                ["formalism"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"formalism">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <330>
        >
        ["DV_URI"] = <
            name = <"DV_URI">
            ancestors = <"DATA_VALUE", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"String">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <331>
        >
        ["DV_EHR_URI"] = <
            name = <"DV_EHR_URI">
            ancestors = <"DV_URI", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <332>
        >
        ["DV_TIME_SPECIFICATION"] = <
            name = <"DV_TIME_SPECIFICATION">
            ancestors = <"DATA_VALUE", ...>
            properties = <
                ["value"] = (P_BMM_SINGLE_PROPERTY) <
                    name = <"value">
                    type = <"DV_PARSABLE">
                    is_mandatory = <True>
                >
            >
            source_schema_id = <"openehr_basic_types_1.0.3">
            is_abstract = <True>
            uid = <333>
        >
        ["DV_PERIODIC_TIME_SPECIFICATION"] = <
            name = <"DV_PERIODIC_TIME_SPECIFICATION">
            ancestors = <"DV_TIME_SPECIFICATION", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <334>
        >
        ["DV_GENERAL_TIME_SPECIFICATION"] = <
            name = <"DV_GENERAL_TIME_SPECIFICATION">
            ancestors = <"DV_TIME_SPECIFICATION", ...>
            source_schema_id = <"openehr_basic_types_1.0.3">
            uid = <335>
        >
    >
    passed = <True>
    missed_class_count = <0>