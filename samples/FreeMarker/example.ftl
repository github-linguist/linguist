<#import "layout.ftl" as layout>

<#assign results = [
        {
            "title": "Example Result",
            "description": "Lorem ipsum dolor sit amet, pede id pellentesque, sollicitudin turpis sed in sed sed, libero dictum."
        }
    ] />

<@layout.page title="FreeMarker Example">
    <#if results?size == 0>
        There were no results.
    <#else>
        <ul>
            <#list results as result>
                <li>
                    <strong>${result.title}</strong>
                    <p>${result.description}</p>
                </li>
            </#list>
        </ul>
    </#if>

    <#-- This is a FreeMarker comment -->
    <@currentTime />
</@layout.page>


<#macro currentTime>
    ${.now?string.full}
</#macro>
