@import static org.example.common.utils.JteHelper.*
@import static org.example.global.ExampleConstants.*
@import gg.jte.Content

@param String pageTitle = null
@param Content body

!{pageTitle = pageTitle == null ? APP_NAME : pageTitle;}

!{addCssFile("main");}
!{addScriptFile("layout");}

!{var renderedBody = preRenderContent(body);}
<!DOCTYPE html>
<html lang="${getLang()}" data-template="jte">
<head>
	<title>${pageTitle}</title>

	<meta charset="utf-8"/>
	<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>

	<link rel="icon" href="${assetHelper().path("favicon.ico")}"/>
	<link rel="stylesheet" href="${assetHelper().path("js/jquery-ui.css")}"/>

	@for(var resource : assetHelper().bundle("globals").getAssets().getCss())
    <link rel="stylesheet" href="${assetHelper().bundlePath(resource)}"/>
	@endfor

	<%--	Ensure our css files have precedence over global ones --%>
	@for(var file : getCssFiles())
		<link rel="stylesheet" href="${assetHelper().path("css/" + file + ".css")}" />
	@endfor

	@for(var file : getBundleFiles())
		@for(var resource : assetHelper().bundle(file).getAssets().getCss())
			<link rel="stylesheet" href="${assetHelper().bundlePath(resource)}"/>
		@endfor
	@endfor

	@for(var style : getInlineStyles())
		${style}
	@endfor
</head>
<body>
<script>
	window.FTC_ENV = ${jsonEncode(ftcEnv())};
</script>
@for(var resource : assetHelper().bundle("sentry").getAssets().getJs())
	<script src="${assetHelper().bundlePath(resource)}" type="module"></script>
@endfor
<script src="${assetHelper().path("translations/" + getLang() + ".js")}"></script>
<script src="${assetHelper().path("js/jquery-1.12.4.js")}"></script>
<script src="${assetHelper().path("js/jquery-ui.js")}"></script>
<script src="${assetHelper().path("bootstrap/js/bootstrap.bundle.min.js")}"></script>
<script src="${assetHelper().path("bootstrap/js/bootstrap-table.min.js")}"></script>
<script src="${assetHelper().path("js/general.js")}"></script>
@for(var resource : assetHelper().bundle("globals").getAssets().getJs())
	<script src="${assetHelper().bundlePath(resource)}" type="module"></script>
@endfor

$unsafe{renderedBody}

@for(var file : getScriptFiles())
	<script src="${assetHelper().path("js/" + file + ".js")}"></script>
@endfor
@for(var file : getDeferScriptFiles())
<%-- Defer to ensure it runs *after* globals have loaded --%>
	<script src="${assetHelper().path("js/" + file + ".js")}" defer></script>
@endfor

@for(var script : getInlineScripts())
	${script}
@endfor

@for(var file : getBundleFiles())
	@for(var resource : assetHelper().bundle(file).getAssets().getJs())
		<script src="${assetHelper().bundlePath(resource)}" type="module"></script>
	@endfor
@endfor
</body>
</html>
