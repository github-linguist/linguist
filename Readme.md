Lingüista
Estado de acciones

Esta biblioteca se usa en GitHub.com para detectar lenguajes blob, ignorar archivos binarios o vendored, suprimir archivos generados en diffs y generar gráficos de desglose de idiomas.

Consulte Solución de problemas y CONTRIBUTING.mdantes de presentar un problema o crear una solicitud de extracción.

Cómo trabaja el lingüista
Linguist toma la lista de idiomas que conoce languages.ymly utiliza una serie de métodos para intentar determinar el idioma utilizado por cada archivo y el desglose general del repositorio.

Linguist comienza revisando todos los archivos en un repositorio y excluye todos los archivos que determina que son datos binarios, código vendido , código generado , documentación o que están definidos como data(por ejemplo, SQL) o prose(por ejemplo, Markdown) lenguajes, teniendo en cuenta cualquier anulación .

Si se ha utilizado una anulación de idioma explícita , ese idioma se utiliza para los archivos coincidentes. El idioma de cada archivo restante se determina utilizando las siguientes estrategias, en orden, con cada paso identificando el idioma preciso o reduciendo la cantidad de idiomas probables que se pasan a la siguiente estrategia:

Línea de modelos Vim o Emacs,
nombre de archivo de uso común,
shebang de concha,
extensión de archivo,
Encabezado XML,
sección de página de manual,
heurísticas,
clasificación bayesiana ingenua
El resultado de este análisis se utiliza para producir la barra de estadísticas de idioma que muestra los porcentajes de idiomas para los archivos en el repositorio. Los porcentajes se calculan en función de los bytes de código para cada idioma según lo informado por la API List Languages .

barra de estadísticas de idioma

Cómo funciona Linguist en GitHub.com
Cuando envías cambios a un repositorio en GitHub.com, se pone en cola un trabajo en segundo plano de baja prioridad para analizar tu repositorio como se explicó anteriormente. Los resultados de este análisis se almacenan en caché durante la vida útil de su repositorio y solo se actualizan cuando se actualiza el repositorio. Dado que este análisis se realiza mediante un trabajo en segundo plano de baja prioridad, la barra de estadísticas de idioma puede tardar un poco en reflejar los cambios, especialmente durante los períodos de mucha actividad.

Uso
Instalación
Instala la gema:

gem install github-linguist
Dependencias
Linguist utiliza charlock_holmespara codificación de caracteres y ruggedpara enlaces libgit2 para Ruby. Estos componentes tienen sus propias dependencias.

charlock_holmes
hacer
pkg-config
UCI
zlib
escabroso
libcurl
OpenSSL
Es posible que deba instalar las dependencias que faltan antes de poder instalar Linguist. Por ejemplo, en macOS con Homebrew :

brew instalar cmake pkg-config icu4c
En Ubuntu:

sudo apt-get install cmake pkg-config libicu-dev zlib1g-dev libcurl4-openssl-dev libssl-dev ruby-dev
Uso de la aplicación
Linguist se puede utilizar en su aplicación de la siguiente manera:

requieren  'robusto' 
requieren  'lingüista'

repo  =  Rugged :: Repository . nueva ( '' ) 
del proyecto  =  lingüista :: Repositorio . nueva ( repo ,  repo . cabeza . target_id ) 
proyecto . language        # => 
Proyecto "Ruby" . idiomas       # => {"Ruby" => 119387}
Uso de la línea de comandos
Repositorio de Git
Las estadísticas de idiomas de un repositorio también se pueden evaluar desde la línea de comandos utilizando el github-linguistejecutable. Sin ninguna opción, github-linguistgenerará el desglose que se correlaciona con lo que se muestra en la barra de estadísticas del idioma. La --breakdownbandera también mostrará el desglose de archivos por idioma.

cd / ruta-al-repositorio /
github-lingüista
Puede intentar ejecutar github-linguisten el directorio raíz de este repositorio:

$ github-linguist - breakdown 
68.57% Ruby 
22.90% C 
6.93% Go 
1.21% Lex 
0.39% Shell

Ruby: 
Gemfile 
Rakefile 
bin / git-linguist 
bin / github-linguist 
ext / linguist / extconf.rb 
github-linguist.gemspec 
lib / linguist.rb 
…
Fila india
Alternativamente, puede encontrar estadísticas para un solo archivo usando el github-linguistejecutable.

Puede intentar ejecutar github-linguisten archivos en este repositorio:

$ github-linguist grammars.yml 
grammars.yml: 884 líneas (884 sloc) 
  tipo: Texto 
  mime tipo: texto / x-yaml 
  idioma: YAML
Estibador
Si tiene Docker instalado, puede crear una imagen y ejecutar Linguist dentro de un contenedor:

$ docker build -t linguist . 
$ docker ejecutar --rm -v $ ( pwd ) : $ ( pwd ) -w $ ( pwd ) -t lingüista 
68.57% Ruby 
22.90% C 
6.93% Go 
1.21% Lex 
0.39% Shell 
$ docker run --rm -v $ ( pwd ) : $ ( pwd ) -w $ ( pwd ) -t lingüista github-lingüista --desglose 
68.57% Ruby 
22.90% C 
6.93% Go 
1.21% Lex 
0.39% Shell

Ruby: 
Gemfile 
Rakefile 
bin / git-linguist 
bin / github-linguist 
ext / linguist / extconf.rb 
github-linguist.gemspec 
lib / linguist.rb 
…
Solución de problemas
Mi repositorio se detecta como el idioma incorrecto
Si la barra de estadísticas de idioma indica un idioma que no espera:

Haga clic en el nombre del idioma en la barra de estadísticas para ver una lista de los archivos identificados como ese idioma. Tenga en cuenta que esto realiza una búsqueda, por lo que las restricciones de búsqueda de código pueden provocar que los archivos identificados en las estadísticas del idioma no aparezcan en los resultados de la búsqueda. Instalar Linguist localmente y ejecutarlo desde la línea de comandos le dará resultados precisos.
Si ve archivos que no escribió en los resultados de la búsqueda, considere mover los archivos a una de las rutas para el código vendido o use la función de anulaciones manuales para ignorarlos.
Si los archivos están mal clasificados, busque problemas abiertos para ver si alguien más ya informó el problema. Cualquier información que pueda agregar, especialmente enlaces a repositorios públicos, es útil. También puede utilizar la función de anulaciones manuales para clasificarlos correctamente en su repositorio.
Si no se informan problemas de esta clasificación errónea, abra un problema e incluya un enlace al repositorio o una muestra del código que se está clasificando erróneamente.
Tenga en cuenta que las estadísticas de idioma del repositorio solo se actualizan cuando realiza cambios y los resultados se almacenan en caché durante la vida útil de su repositorio. Si no ha realizado ningún cambio en su repositorio por un tiempo, es posible que al presionar otro cambio se corrijan las estadísticas.

Mi repositorio no muestra mi idioma
El lingüista no considera el código vendido , el código generado , la documentación o data(por ejemplo, SQL) o prose(por ejemplo, Markdown) los lenguajes (según lo definido por el typeatributo en languages.yml) al calcular las estadísticas del idioma del repositorio.

Si la barra de estadísticas de idioma no muestra su idioma en absoluto, podría deberse a varias razones:

El lingüista no conoce tu idioma.
La extensión que ha elegido no está asociada con su idioma en languages.yml.
Todos los archivos de su repositorio pertenecen a una de las categorías enumeradas anteriormente que Linguist excluye de forma predeterminada.
Si Linguist no conoce el idioma o la extensión que está usando, considere contribuir a Linguist abriendo una solicitud de extracción para agregar soporte para su idioma o extensión. Para todo lo demás, puede utilizar la función de anulaciones manuales para indicarle a Linguist que incluya sus archivos en las estadísticas del idioma.

Hay un problema con el resaltado de sintaxis de un archivo
Linguist detecta el idioma de un archivo, pero el resaltado de sintaxis real está impulsado por un conjunto de gramáticas del idioma que se incluyen en este proyecto como un conjunto de submódulos como se enumeran aquí .

Si experimenta un problema con el resaltado de sintaxis en GitHub, informe el problema al repositorio de gramática ascendente, no aquí. Las gramáticas se actualizan cada vez que creamos la gema Linguist, por lo que las correcciones de errores iniciales se incorporan automáticamente a medida que se corrigen.

Recibo un error al usar Linguist en un directorio que no es un repositorio de Git
Linguist solo funciona en repositorios Git y archivos individuales. Su uso principal es en GitHub.com, que usa repositorios simples y, por lo tanto, los cambios deben confirmarse ya que los archivos individuales no se muestran en el sistema de archivos.

Como solución, puede inicializar un repositorio de Git temporal en su directorio como se muestra en este script .

Alternativamente, puede ejecutar Linguist en archivos individuales, consulte más arriba .

Anulaciones
Linguist admite varias estrategias de anulación personalizadas diferentes para definiciones de idioma y rutas de archivo.

Usando gitattributes
Añadir un .gitattributesarchivo a su proyecto y utilizar comparadores de trayectoria al estilo git estándar para los archivos que desea reemplazar el uso de los linguist-documentation, linguist-language, linguist-vendored, linguist-generated y linguist-detectableatributos. .gitattributesse utilizará para determinar las estadísticas del idioma y se utilizará para resaltar la sintaxis de los archivos. También puede configurar manualmente el resaltado de sintaxis usando modelos de Vim o Emacs .

Al realizar pruebas con una instalación local de Linguist, tenga en cuenta que los atributos agregados no entrarán en vigor hasta que el .gitattributesarchivo se haya enviado a su repositorio.

Las rutas de archivo y carpeta en el interior .gitattributesse calculan en relación con la posición del .gitattributesarchivo.

# Ejemplo de un archivo `.gitattributes` que reclasifica archivos` .rb` como Java: 
* .rb  linguist-language = Java

# Reemplace cualquier espacio en blanco en el nombre del idioma con guiones: 
* .glyphs  linguist-language = OpenStep-Property-List
Código vendido
Verificar el código que no escribiste, como las bibliotecas de JavaScript, en tu repositorio de git es una práctica común, pero esto a menudo infla las estadísticas de idioma de tu proyecto e incluso puede hacer que tu proyecto sea etiquetado como otro idioma. De forma predeterminada, Linguist trata todas las rutas definidas en vendor.ymlcomo vendidas y, por lo tanto, no las incluye en las estadísticas de idioma de un repositorio.

Utilice el linguist-vendoredatributo para rutas de proveedor o no proveedor:

ruta-especial-vendored / *  linguist-vendored 
jquery.js  - linguist-vendored
Documentación
Al igual que los archivos vendidos, Linguist excluye los archivos de documentación de las estadísticas de idioma de su proyecto. documentation.ymlenumera las rutas de documentación comunes y las excluye de las estadísticas de idioma de su repositorio.

Utilice el linguist-documentationatributo para marcar o desmarcar rutas como documentación:

project-docs / *  linguist-documentation 
docs / formatter.rb  - linguist-documentation
Código generado
No todos los archivos de texto sin formato son verdaderos archivos de origen. Los archivos generados como JavaScript minificado y CoffeeScript compilado se pueden detectar y excluir de las estadísticas del lenguaje. Como beneficio adicional, a diferencia de los archivos de documentación y de venta, estos archivos se suprimen en diffs. generated.rbenumera las rutas comunes generadas y las excluye de las estadísticas de idioma de su repositorio.

Utilice el linguist-generatedatributo para marcar o desmarcar rutas como generadas.

Api.elm  generado por lingüistas
Detectable
Solo los lenguajes de programación se incluyen en las estadísticas de idiomas. Los idiomas de un tipo diferente (como se define en languages.yml) no son "detectables", lo que hace que no se incluyan en las estadísticas de idiomas.

Utilice el linguist-detectableatributo para marcar o desmarcar rutas como detectables:

* .kicad_pcb  linguist-detectable 
* .sch  linguist-detectable 
tools / export_bom.py  - linguist-detectable
Usando modelos de Emacs o Vim
Si no desea utilizar .gitattributespara anular el resaltado de sintaxis utilizado en GitHub.com, puede usar modelos de estilo Vim o Emacs para establecer el idioma de un solo archivo. Las modelinas se pueden colocar en cualquier lugar dentro de un archivo y se respetan al determinar cómo resaltar la sintaxis de un archivo en GitHub.com

Empuje
# Some examples of various styles:
vim: syntax=java
vim: set syntax=ruby:
vim: set filetype=prolog:
vim: set ft=cpp:
Emacs
-*- mode: php; -*-
-*- c++ -*-
Contribuyendo
Consulte nuestras pautas de contribución .

Licencia
Las gramáticas del lenguaje incluidas en esta gema están cubiertas por las respectivas licencias de sus repositorios. vendor/README.mdenumera el repositorio de cada gramática.

Todos los demás archivos están cubiertos por la licencia MIT, consulte LICENSE.
