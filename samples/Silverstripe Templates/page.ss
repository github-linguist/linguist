<!doctype html>
<html lang="$ContentLocale">
<head>
    <% base_tag %>
    <%-- Required meta tags --%>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    $MetaTags(false)

    <% include Favicons %>
    <% require themedCSS('startup') %>

    <title><% if $MetaTitle %>$MetaTitle<% else %>$Title<% end_if %> | $SiteConfig.Title</title>
</head>
<body <% if $i18nScriptDirection %>dir="$i18nScriptDirection"<% end_if %>>
    <% include Header %>
    <main id="main" class="container container--page" tabindex="-1">
        <% if not $isHomePage %>
            $Breadcrumbs
        <% end_if %>
        <div class="page">
            $Layout
        </div>
        <%t SilverStripe\\CMS\\Model\\SiteTree.ExampleLocalisation 'Too many children ({count})' count=$Children.Count %>
    </main>

    <% include Footer IncludeVar1=$Title IncludeVar2="raw string value" %>
    <% if $HasPerm('CMS_ACCESS') || $SomeNumber >= 5 %>
        $SilverStripeNavigator
    <% else_if $OverridePermissions %>
        Ooooh fancy permission override
    <% else %>
        Not authorised to see the navigator
    <% end_if %>
    <script type="module" src="{$themedResourceURL('js/startup.js')}" defer></script>
</body>
</html>
<%-- This example is from https://github.com/silverstripe/startup-theme/blob/1/templates/Page.ss but was modified for this example --%>