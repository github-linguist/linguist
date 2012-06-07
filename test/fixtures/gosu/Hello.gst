<%!-- defined in Hello.gst --%>

<%@ params( users : Collection <User> ) %>

<%  for( user in users ) { %>

${user.LastName}, ${user.FirstName}, ${user.Department}  <%  } %>