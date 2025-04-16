<%@ tag language="java" pageEncoding="ISO-8859-1"
	description="Base Page"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<!DOCTYPE html>
<html>

<head>
<meta charset='utf-8'>
<title>WavyArch</title>
<meta http-equiv="X-UA-Compatible" content="ie=edge">
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta name='viewport' content='width=device-width, initial-scale=1'
	, shrink-to-fit=no>
<link rel="shortcut icon"
	href="<c:url value="/resources/img/logo.svg"/>" type="image/x-icon">
<link rel="stylesheet"
	href="<c:url value="/resources/css/bootstrap.min.css"/>">
<link rel="stylesheet" href="<c:url value="/resources/css/all.css"/>">
<link rel="stylesheet" href="<c:url value="/resources/css/main.css"/>">
</head>

<body>
	<jsp:doBody />

	<script src="<c:url value="/resources/js/jquery-3.5.1.slim.min.js"/>"></script>
	<script src="<c:url value="/resources/js/popper.min.js"/>"></script>
	<script src="<c:url value="/resources/js/bootstrap.min.js"/>"></script>
	<script src="<c:url value="/resources/js/validation.js"/>"></script>
	<script src="<c:url value="/resources/js/main.js"/>"></script>
</body>

</html>