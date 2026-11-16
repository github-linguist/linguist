<#-- @ftlvariable name="classes" type="String" -->
<#-- @ftlvariable name="url" type="String" -->
<#-- @ftlvariable name="image" type="Object" -->
<#-- @ftlvariable name="image.src" type="String" -->
<#-- @ftlvariable name="image.altText" type="String" -->
<#-- @ftlvariable name="image.width" type="String" -->
<#-- @ftlvariable name="image.height" type="String" -->
<#-- @ftlvariable name="kicker" type="String" -->
<#-- @ftlvariable name="title" type="String" -->
<#-- @ftlvariable name="text" type="String" -->

<div class="blocks-card ${classes!}">
  <@linkOrDiv href=url!"" class="blocks-card--card">
    <#if image??>
      <figure class="blocks-card--image">
        <img
          src="${image.src}"
          alt="${image.altText!}"
          <#if image.width??>width="${image.width}"</#if>
          <#if image.height??>height="${image.height}"</#if>
        />
      </figure>
    </#if>

    <#if kicker?has_content || title?has_content || text?has_content>
      <div class="blocks-card--body html-area">
        <#if kicker?has_content>
          <small class="blocks-card--kicker">
            ${kicker}
          </small>
        </#if>

        <#if title?has_content>
          <h3 class="blocks-card--title">
            ${title}
          </h3>
        </#if>

        <#if text?has_content>
          <div class="blocks-card--text flow">
            ${text?no_esc}
          </div>
        </#if>
      </div>
    </#if>
  </@linkOrDiv>
</div>

<#macro linkOrDiv href class>
  <#if href?has_content>
    <a href="${href}" class="${class}">
      <#nested>
    </a>
  <#else>
    <div class="${class}">
      <#nested>
    </div>
  </#if>
</#macro>
