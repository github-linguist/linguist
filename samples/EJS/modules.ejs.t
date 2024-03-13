<% 
const moduleImports = [];
modules.forEach(mod => { %>
import * as <%- mod.saveName %> from '@<%- mod.packageName %>/<%- mod.name %>';
<% });
%>
export const modulePackages = [
<%- modules.map(x => {
return `  {
    moduleName: "${x.name}",
    module: ${x.saveName},
    isEntry: ${x.isEntry},
    isLibraryOnly: ${x.isLibraryOnly},
    parentGahModule: ${x.parentGahModule ? "\"" + x.parentGahModule + "\"" : null}
  }`;
  }).join(',\n') %>
];

export const gahModules = [
<%- modules.filter(x => !x.isLibraryOnly).map(x => '  ' + x.saveName + '.' + x.baseModuleName + x.staticModuleInit).join(',\n') %>
];
