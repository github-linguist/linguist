*** Settings ***
Documentation       Example

Library             Collections


*** Variables ***
${variable}     ${EMPTY}
${some_list}    @{EMPTY}
${some_dict}    &{EMPTY}


*** Keywords ***
Dictionary Should Contain ${key}=${value}
    [Arguments]    ${dict}
    Dictionary Should Contain Key    ${dict}    ${key}
    Should Be Equal    ${dict}[key]    ${value}
