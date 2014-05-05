{smcl}
{* *! version 1.1.0 Matthew White 05jan2014}{...}
{title:Title}

{phang}
{cmd:odkmeta} {hline 2} Create a do-file to import ODK data


{marker syntax}{...}
{title:Syntax}

{p 8 10 2}
{cmd:odkmeta}
{cmd:using}
{it:{help filename}}{cmd:,}
{opt csv(csvfile)}
{* Using -help histogram- as a template.}{...}
{* -help histogram- notwithstanding, "opts" is rarely preceded by an underscore.}{...}
{cmdab:s:urvey(}{it:surveyfile}{cmd:,}
{it:{help odkmeta##surveyopts:surveyopts}}{cmd:)}
{cmdab:cho:ices(}{it:choicesfile}{cmd:,}
{it:{help odkmeta##choicesopts:choicesopts}}{cmd:)}
[{it:options}]

{* Using -help odbc- as a template.}{...}
{* 36 is the position of the last character in the first column + 3.}{...}
{synoptset 36 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{* Using -help heckman- as a template.}{...}
{p2coldent:* {opt csv(csvfile)}}name of the .csv file that
contains the ODK data{p_end}
{p2coldent:* {cmdab:s:urvey(}{it:surveyfile}{cmd:,} {it:{help odkmeta##surveyopts:surveyopts}}{cmd:)}}import
metadata from the {it:survey} worksheet {it:surveyfile}{p_end}
{p2coldent:* {cmdab:cho:ices(}{it:choicesfile}{cmd:,} {it:{help odkmeta##choicesopts:choicesopts}}{cmd:)}}import
metadata from the {it:choices} worksheet {it:choicesfile}{p_end}

{syntab:Fields}
{synopt:{opt drop:attrib(headers)}}do not import field attributes with
the column headers {it:headers}{p_end}
{synopt:{opt keep:attrib(headers)}}import only field attributes with
the column headers {it:headers}{p_end}
{synopt:{opt rel:ax}}ignore fields in {it:surveyfile} that
do not exist in {it:csvfile}{p_end}

{syntab:Lists}
{* Using -help ca- as a template.}{...}
{synopt:{cmdab:oth:er(}{it:{help odkmeta##other:other}}{cmd:)}}Stata value of
{cmd:other} values of {cmd:select or_other} fields; default is {cmd:max}{p_end}
{synopt:{opt one:line}}write each list on a single line{p_end}

{syntab:Options}
{synopt:{opt replace}}overwrite existing {it:{help filename}}{p_end}
{synoptline}
{p2colreset}{...}
{* Using -help heckman- as a template.}{...}
{p 4 6 2}* {opt csv()}, {opt survey()}, and {opt choices()} are required.{p_end}

{marker surveyopts}{...}
{* Change in {synoptset}: using -help odbc- as a template.}{...}
{synoptset 23 tabbed}{...}
{synopthdr:surveyopts}
{synoptline}
{syntab:Main}
{synopt:{opt t:ype(header)}}column header of the {it:type} field attribute;
default is {cmd:type}{p_end}
{synopt:{opt name(header)}}column header of the {it:name} field attribute;
default is {cmd:name}{p_end}
{synopt:{opt la:bel(header)}}column header of the {it:label} field attribute;
default is {cmd:label}{p_end}
{synopt:{opt d:isabled(header)}}column header of
the {it:disabled} field attribute; default is {cmd:disabled}{p_end}
{synoptline}
{p2colreset}{...}

{marker choicesopts}{...}
{synoptset 23 tabbed}{...}
{synopthdr:choicesopts}
{synoptline}
{syntab:Main}
{synopt:{opt li:stname(header)}}column header of
the {it:list_name} list attribute; default is {cmd:list_name}{p_end}
{synopt:{opt name(header)}}column header of the {it:name} list attribute;
default is {cmd:name}{p_end}
{synopt:{opt la:bel(header)}}column header of the {it:label} list attribute;
default is {cmd:label}{p_end}
{synoptline}
{p2colreset}{...}

{marker other}{...}
{synoptset 23 tabbed}{...}
{synopthdr:other}
{synoptline}
{synopt:{opt max}}maximum value of each list: maximum list value plus one{p_end}
{synopt:{opt min}}minimum value of each list: minimum list value minus
one{p_end}
{synopt:{it:#}}constant value for all value labels{p_end}
{synoptline}
{p2colreset}{...}


{marker description}{...}
{title:Description}

{pstd}
{cmd:odkmeta} creates a do-file to import ODK data,
using the metadata from the {it:survey} and {it:choices} worksheets of
the XLSForm. The do-file, saved to {it:filename},
completes the following tasks in order:

{* Using -help anova- as a template.}{...}
{phang2}o  Import lists as {help label:value labels}{p_end}
{phang2}o  Add {cmd:other} values to value labels{p_end}
{phang2}o  Import field attributes as {help char:characteristics}{p_end}
{phang2}o  Split {cmd:select_multiple} variables{p_end}
{phang2}o  Drop {cmd:note} variables{p_end}
{phang2}o  {help format:Format} {cmd:date}, {cmd:time}, and
{cmd:datetime} variables{p_end}
{phang2}o  Attach value labels{p_end}
{phang2}o  Attach field labels as
{help label:variable labels} and {help notes}{p_end}
{phang2}o  {help merge:Merge} repeat groups{p_end}

{pstd}
After {cmd:select_multiple} variables have been split,
tasks can be removed from the do-file without affecting other tasks.
User-written supplements to the do-file may make use of any field attributes,
which are imported as characteristics.


{marker remarks}{...}
{title:Remarks}

{pstd}
The {cmd:odkmeta} do-file uses {helpb insheet} to import data.
Fields that are long strings of digits, such as {cmd:simserial} fields,
will be imported as numeric even if they are more than 16 digits.
As a result, they will lose {help precision}.

{pstd}
The do-file makes limited use of {help mata:Mata} to
manage variable labels, value labels, and characteristics and
to import field attributes and lists that contain difficult characters.

{pstd}
The do-file starts with the definitions of several {help local:local macros};
these are constants that the do-file uses.
For instance, local macro {cmd:`datemask'} is the {help date():mask} of
date values in the .csv files.
The local macros are automatically set to default values, but
they may need to be changed depending on the data.


{* Using xtdpd_postspecial2b.ihlp as a template.}{...}
{marker remarks_field_names}{...}
{title:Remarks for field names}

{pstd}
ODK field names follow different conventions from
Stata's {help varname:constraints on variable names}.
Further, the field names in the .csv files are the fields' "long names,"
which are formed by concatenating the list of the {it:groups}
in which the field is nested with the field's "short name."
ODK long names are often much longer than the length limit on variable names,
which is 32 characters.

{pstd}
These differences in convention lead to three kinds of
problematic field names:

{* Using -help 663- as a template.}{...}
{phang2}1.  Long field names that involve an invalid combination of characters,
for example, a name that begins with a colon followed by a number.
{helpb insheet} will not convert these to Stata names,
instead naming each variable {cmd:v} concatenated with a positive integer,
for example, {cmd:v1}.{p_end}
{phang2}2.  Long field names that are unique ODK names but
when converted to Stata names and truncated to 32 characters become duplicates.
{cmd:insheet} will again convert these to {cmd:v}{it:#} names.{p_end}
{phang2}3.  Long field names of the form {cmd:v}{it:#} that
become duplicates with other variables that cannot be converted,
for which {cmd:insheet} chooses {cmd:v}{it:#} names.
These will be converted to different {cmd:v}{it:#} names.{p_end}

{pstd}
Because of problem 3,
it is recommended that you do not name fields as {cmd:v}{it:#}.

{pstd}
If a field name cannot be imported,
its characteristic {helpb odkmeta##Odk_bad_name:Odk_bad_name} is {cmd:1};
otherwise it is {cmd:0}.

{pstd}
Most tasks that the {cmd:odkmeta} do-file completes do not depend on
variable names. There are two exceptions:

{phang2}1.  The do-file uses {helpb split} to split
{cmd:select_multiple} variables. {cmd:split} will result in an error
if a {cmd:select_multiple} variable has a long name or
if splitting it would result in duplicate variable names.{p_end}
{phang2}2.  The do-file uses {helpb reshape} and {helpb merge} to merge
repeat groups. {cmd:reshape} will result in an error
if there are long variable names. The merging code will result in an error
if there are duplicate variable names in two datasets.{p_end}

{pstd}
Where variable names result in an error, renaming is left to the user.
The section of the do-file for splitting is preceded by
a designated area for renaming.
In the section for reshaping and merging, each repeat group has
its own area for renaming.

{pstd}
Many forms do not require any variable renaming.
For others, only a few variables need to be renamed;
such renaming should go in the designated areas.
However, some forms,
usually because of many nested groups or groups with long names,
have many long field names that become duplicate Stata names (problem 2 above).
In this case, it may work best to use fields' short names where possible.
The following code attempts to rename variables to their field short names.
Place it as-is before the renaming for {cmd:split}:

{cmd}{...}
{phang}foreach var of varlist _all {{p_end}
{phang2}if "`:char `var'[Odk_group]'" != "" {{p_end}
{phang3}local name = "`:char `var'[Odk_name]'" + ///{p_end}
{p 16 20 2}cond(`:char `var'[Odk_is_other]', "_other", "") + ///{p_end}
{p 16 20 2}"`:char `var'[Odk_geopoint]'"{p_end}
{phang3}local newvar = strtoname("`name'"){p_end}
{phang3}capture rename `var' `newvar'{p_end}
{phang2}}{p_end}
{phang}}{p_end}
{txt}{...}


{marker remarks_lists}{...}
{title:Remarks for lists}

{pstd}
ODK list names are not necessarily valid Stata names.
However, {cmd:odkmeta} uses list names as value label names, and
it requires that all ODK list names be Stata names.

{pstd}
ODK lists are lists of associations of names and labels.
There are two broad categories of lists:
those whose names are all integer and those with at least one noninteger name.
In the former case, the values of the value label are
the same as the names of the list.
In the latter, the values of the value label indicate
the order of the names within the list:
the first name will equal {cmd:1}, the second {cmd:2}, and so on.
For such lists, the value of the value label may differ
from the name of the list even if the name is a valid value label value;
what matters is whether all names of the list are integer.

{pstd}
However, the value labels of these lists are easy to modify.
Simply change the values of the value labels in the do-file;
the rest of the do-file will be unaffected.
Do not change the value label text.

{pstd}
Certain names do not interact well with {helpb insheet},
which the {cmd:odkmeta} do-file uses to import the data.

{pstd}
For instance, it is not always possible to distinguish a name of {cmd:"."} from
{helpb missing:sysmiss}. When it is unclear, the do-file assumes that
values equal the name {cmd:"."} and not {cmd:sysmiss}.
The problem arises when {cmd:insheet} imports {cmd:select} fields whose
names in the data are the same as the values of a Stata numeric variable:
real numbers, {cmd:sysmiss}, and {help missing:extended missing values}.
{cmd:insheet} imports such fields as numeric,
converting blank values ({cmd:""}) as {cmd:sysmiss},
thereby using the same Stata value for the name {cmd:"."} and for blank values.

{pstd}
{cmd:insheet} does not always interact well with list values' names that
look like numbers with leading zeros, for example, {cmd:01} or {cmd:0.5}.
If {cmd:insheet} imports a {cmd:select} field as numeric,
it will remove such leading zeros, leading to incorrect values or
an error in the do-file. For similar reasons,
trailing zeros after a decimal point may be problematic.

{pstd}
List values' names that look like decimals may also not interact well with
{cmd:insheet}. If {cmd:insheet} imports a {cmd:select} field as numeric,
the do-file will convert it to string. However, for {help precision} reasons,
the resulting string may differ from the original name
if the decimal has no exact finite-digit representation in binary.

{pstd}
Generally, names that look like numbers that cannot be stored precisely as
{helpb data_types:double} are problematic.
This includes numbers large in magnitude.


{marker remarks_variants}{...}
{title:Remarks for ODK variants}

{pstd}
{cmd:odkmeta} is not designed for features specific to ODK variants,
such as SurveyCTO or formhub.
However, it is often possible to modify the {cmd:odkmeta} do-file to account
for these features,
especially as all field attributes are imported as characteristics.

{pstd}
{ul:SurveyCTO}

{pstd}
For instance, the {cmd:odkmeta} do-file will result in an error for
SurveyCTO forms that contain dynamic choice lists.
One solution is to make the following changes to the do-file in order
to import {cmd:select} fields with dynamic lists as string variables.

{pstd}
One section of the {cmd:odkmeta} do-file encodes
{cmd:select} fields whose list contains a noninteger name.
Here, remove dynamic lists from the list of such lists:

{phang}{cmd:* Encode fields whose list contains a noninteger name.}{p_end}
{phang}{cmd:local lists list1 list2 list3 ...}{p_end}
{phang}...{p_end}

{pstd}
Above, if {cmd:list3} were a dynamic list, it should be removed.

{pstd}
The next section of the do-file attaches value labels to variables:

{cmd}{...}
{phang}* Attach value labels.{p_end}
{phang}ds, not(vallab){p_end}
{phang}if "`r(varlist)'" != "" ///{p_end}
{phang2}ds `r(varlist)', has(char Odk_list_name){p_end}
{phang}foreach var in `r(varlist)' {{p_end}
{phang2}if !`:char `var'[Odk_is_other]' {{p_end}
{phang}...{p_end}
{txt}{...}

{pstd}
Add a line to the second {helpb ifcmd:if} command to exclude fields whose
{it:appearance} attribute contains a {cmd:search()} expression:

{cmd}{...}
{phang}* Attach value labels.{p_end}
{phang}ds, not(vallab){p_end}
{phang}if "`r(varlist)'" != "" ///{p_end}
{phang2}ds `r(varlist)', has(char Odk_list_name){p_end}
{phang}foreach var in `r(varlist)' {{p_end}
{phang2}if !`:char `var'[Odk_is_other]' & ///{p_end}
{phang3}!strmatch("`:char `var'[Odk_appearance]'", "*search(*)*") {{p_end}
{phang}...{p_end}
{txt}{...}

{pstd}
The do-file will now import fields with dynamic lists without
resulting in an error.

{pstd}
{ul:formhub}

{pstd}
formhub does not export {cmd:note} fields in the .csv files;
specify option {cmd:relax} to {cmd:odkmeta}.

{pstd}
formhub exports blank values as {cmd:"n/a"}.
Multiple sections of the {cmd:odkmeta} do-file must be modified
to accommodate these.

{pstd}
Immediately before this line in the section for
formatting {cmd:date}, {cmd:time}, and {cmd:datetime} variables:

{phang}
{cmd:if inlist("`type'", "date", "today") {c -(}}

{pstd}
add the following line:

{phang}
{cmd:replace `var' = "" if `var' == "n/a"}

{pstd}
Immediately before this line in the section for attaching value labels:

{phang}
{cmd:replace `var' = ".o" if `var' == "other"}

{pstd}
add the following line:

{phang}
{cmd:replace `var' = "" if `var' == "n/a"}

{pstd}
These lines replace {cmd:"n/a"} values with blank ({cmd:""}).


{marker remarks_missing}{...}
{title:Remarks for "don't know," refusal, and other missing values}

{pstd}
ODK lists may contain missing values, including "don't know" and refusal values.
These will be imported as nonmissing in Stata.
However, if the lists use largely consistent names or labels for the values,
it may be possible to automate the conversion of the values to
{help missing:extended missing values} in Stata.
The following {help SSC} programs may be helpful:

{p2colset 5 18 22 2}{...}
{p2col:{cmd:labmvs}}{bf:{stata ssc install labutil2}}{p_end}
{p2col:{cmd:labmv}}{bf:{stata ssc install labutil2}}{p_end}
{p2col:{cmd:labrecode}}{bf:{stata ssc install labutil2}}{p_end}
{p2col:{cmd:labelmiss}}{bf:{stata ssc install labelmiss}}{p_end}
{p2colreset}{...}


{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{cmd:survey(}{it:surveyfile}{cmd:,} {it:surveyopts}{cmd:)} imports
the field metadata from the XLSForm's {it:survey} worksheet.
{opt survey()} requires {it:surveyfile} to be a comma-separated text file.
Strings with embedded commas, double quotes, or end-of-line characters must
be enclosed in quotes, and
embedded double quotes must be preceded by another double quote.

{pmore}
Each attribute in the {it:survey} worksheet has
its own column and column header.
Use the suboptions {opt type()}, {opt name()}, {opt label()}, and
{opt disabled()} to specify alternative column headers for
the {it:type}, {it:name}, {it:label}, and
{it:disabled} attributes, respectively.
All field attributes are imported as {help char:characteristics}.

{pmore}
If the {it:survey} worksheet has duplicate column headers,
only the first column for each column header is used.

{pmore}
The {it:type} characteristic is standardized as follows:

{phang2}o  {cmd:select one} is replaced as {cmd:select_one}.{p_end}
{phang2}o  {cmd:select or other} is replaced as {cmd:select or_other}:
{cmd:select_one} {it:list_name} {cmd:or other} is replaced as
{cmd:select_one} {it:list_name} {cmd:or_other}, and
{cmd:select_multiple} {it:list_name} {cmd:or other} is replaced as
{cmd:select_multiple} {it:list_name} {cmd:or_other}.{p_end}
{phang2}o  {cmd:begin_group} is replaced as {cmd:begin group};
{cmd:end_group} is replaced as {cmd:end group};
{cmd:begin_repeat} is replaced as {cmd:begin repeat};
and {cmd:end_repeat} is replaced as {cmd:end repeat}.{p_end}

{pmore}
In addition to the attributes specified in the {it:survey} worksheet,
{cmd:odkmeta} attaches these characteristics to variables:

{pmore2}
{marker Odk_bad_name}{...}
{cmd:Odk_bad_name} is {cmd:1}
if the variable's name differs from its ODK field name and {cmd:0} if not.
See the {help odkmeta##remarks_field_names:remarks for field names} above.

{pmore2}
{cmd:Odk_group} contains a list of the {it:groups}
in which the variable is nested, in order of the {it:group} level.

{pmore2}
{cmd:Odk_long_name} contains the field's "long name,"
which is formed by concatenating
the list of the {it:groups} in which the field is nested
with the field "short name," with elements separated by {cmd:"-"}.

{pmore2}
{cmd:Odk_repeat} contains the (long) name of the repeat group
in which the variable is nested.

{pmore2}
{cmd:Odk_list_name} contains the name of a {cmd:select} field's list.

{pmore2}
{cmd:Odk_or_other} is {cmd:1}
if the variable is a {cmd:select or_other} field and {cmd:0} if not.

{pmore2}
{cmd:Odk_is_other} is {cmd:1}
if the variable is a free-text {cmd:other} variable associated with
a {cmd:select or_other} field; otherwise it is {cmd:0}.

{pmore2}
For {cmd:geopoint} variables, {cmd:Odk_geopoint} is
the variable's {cmd:geopoint} component:
{cmd:Latitude}, {cmd:Longitude}, {cmd:Altitude}, or {cmd:Accuracy}.
For variables that are not type {cmd:geopoint}, {cmd:Odk_geopoint} is blank.

{phang}
{cmd:choices(}{it:choicesfile}{cmd:,} {it:choicesopts}{cmd:)} imports
the list metadata from the XLSForm's {it:choices} worksheet.
{cmd:choices()} requires {it:choicesfile} to be a comma-separated text file.
Strings with embedded commas, double quotes, or end-of-line characters must
be enclosed in quotes, and
embedded double quotes must be preceded by another double quote.

{pmore}
Each attribute in the {it:choices} worksheet has
its own column and column header.
Use the suboptions {opt listname()}, {opt name()}, and {opt label()} to specify
alternative column headers for the {it:list_name}, {it:name}, and
{it:label} attributes, respectively.
List attributes are imported as value labels.

{pmore}
If the {it:choices} worksheet has duplicate column headers,
only the first column for each column header is used.

{dlgtab:Fields}

{phang}
{opt dropattrib(headers)} specifies the column headers of field attributes that
should not be imported as characteristics.
{cmd:_all} specifies that all characteristics be dropped.

{phang}
{opt keepattrib(headers)} specifies the column headers of
field attributes to import as characteristics.
{cmd:_all} means all column headers. Other attributes are not imported.

{phang}
{opt relax} specifies that fields mentioned in {it:surveyfile} that
do not exist in {it:csvfile} be ignored.
By default, the do-file attempts to attach the characteristics to
these variables, resulting in an error if the variable does not exist.
For fields associated with multiple variables, for example,
{cmd:geopoint} fields, {opt relax} attempts to attach
the characteristics to as many variables as possible:
an error does not result if some but not all variables exist.

{dlgtab:Lists}

{phang}
{opt other(other)} specifies the Stata value of {cmd:other} values of
{cmd:select or_other} fields.

{pmore}
{cmd:max}, the default, specifies that the Stata value of {cmd:other} vary by
the field's list.
For each list, {cmd:other} will be the maximum value of the list plus one.

{pmore}
{cmd:min} specifies that the Stata value of {cmd:other} vary by
the field's list.
For each list, {cmd:other} will be the minimum value of the list minus one.

{pmore}
{it:#} specifies a constant value for {cmd:other} that
will be used for all lists.

{phang}
{opt oneline} specifies that each list's value label definition be written on
one line, rather than on multiple using {helpb delimit:#delimit ;}.

{dlgtab:Other}

{phang}
{opt replace} specifies that the {cmd:odkmeta} do-file be replaced
if it already exists.


{marker examples}{...}
{title:Examples}

{pstd}
Create a do-file named {cmd:import.do} that imports ODK data,
including the metadata in {cmd:survey.csv} and {cmd:choices.csv}
{p_end}
{phang2}{cmd}
. odkmeta using import.do,
csv("ODKexample.csv") survey("survey.csv") choices("choices.csv")
{txt}

{pstd}
Same as the previous {cmd:odkmeta} command,
but specifies that the field {it:name} attribute appears in
the {cmd:fieldname} column of {cmd:survey_fieldname.csv}
{p_end}
{phang2}{cmd}
. odkmeta using import.do,
csv("ODKexample.csv") survey("survey_fieldname.csv", name(fieldname))
choices("choices.csv") replace
{txt}

{pstd}
Same as the previous {cmd:odkmeta} command,
but specifies that the list {it:name} attribute appears in
the {cmd:valuename} column of {cmd:choices_valuename.csv}
{p_end}
{phang2}{cmd}
. odkmeta using import.do,
csv("ODKexample.csv") survey("survey_fieldname.csv", name(fieldname))
choices("choices_valuename.csv", name(valuename)) replace
{txt}

{pstd}
Create a do-file that imports all field attributes except for {it:hint}
{p_end}
{phang2}{cmd}
. odkmeta using import.do,
csv("ODKexample.csv") survey("survey.csv") choices("choices.csv")
dropattrib(hint) replace
{txt}

{pstd}
Same as the previous {cmd:odkmeta} command,
but does not import any field attributes
{p_end}
{phang2}{cmd}
. odkmeta using import.do,
csv("ODKexample.csv") survey("survey.csv") choices("choices.csv")
dropattrib(_all) replace
{txt}

{pstd}
Create a do-file that
imports {cmd:other} values of {cmd:select or_other} fields as {cmd:99}
{p_end}
{phang2}{cmd}
. odkmeta using import.do,
csv("ODKexample.csv") survey("survey.csv") choices("choices.csv")
other(99) replace
{txt}


{marker acknowledgements}{...}
{title:Acknowledgements}

{pstd}
Lindsey Shaughnessy of Innovations for Poverty Action assisted in
almost all aspects of {cmd:odkmeta}'s development.
She collaborated on the structure of the program, was a very helpful tester, and
contributed information about ODK.


{marker author}{...}
{title:Author}

{pstd}Matthew White, Innovations for Poverty Action{p_end}
{pstd}mwhite@poverty-action.org{p_end}
