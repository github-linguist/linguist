<#ftl strip_text=true />

<#macro page title>
    <!doctype html>
    <html lang="${.lang}">
        <head>
            <title>${title}</title>
            <@metaTags />
        </head>
        <body>
            <#nested />
            <@footer />
        </body>
    </html>
</#macro>


<#---
  Default meta tags
-->
<#macro metaTags>
    <#compress>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width,initial-scale=1">
        <meta name="format-detection" content="telephone=no">
    </#compress>
</#macro>

<#macro footer>
    <p>This page is using FreeMarker v${.version}</p>
</#macro>
