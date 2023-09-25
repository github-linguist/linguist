*** Settings ***
Documentation       Example keywords file.
...                 It is recommended to use another ending than `.robot` for resource files.
...                 By convention, the file ending for resource files is `.resource`.
...                 Another resource than `.robot` makes maintaining large projects easier. It also prevents
...                 the parser to search for tasks or test cases in resource files, which saves some startup performance.

Library             Collections


*** Variables ***
${variable}     ${EMPTY}
${some_list}    @{EMPTY}
${some_dict}    &{EMPTY}


*** Keywords ***
Dictionary Should Contain ${key}=${value}
    [Arguments]    ${dict}
    Dictionary Should Contain Key    ${dict}    ${key}
    Should Be Equal    ${dict}[key]    ${value}    msg=Dictionary does not hold expected item.
