#Lynette García Pérez
#Febrero 2019
#Script que sirve para descargar y unir archivos de datos de importación de vehículos de la SAT

#Paquetes necesarios
#lubridate
#stringr

library("tools")
library("lubridate")
library("stringr")


setwd("carpeta donde va a guardar los archivos")
# Para obtener los links usé la siguiente shiny apps disponible en:https://spannbaueradam.shinyapps.io/r_regex_tester/ con el 
# código fuente de la página de la SAT y el siguiente patrón: https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/[[:digit:]]{4}/importacion_de_vehiculos_[[:digit:]]{4}_[[:lower:]]+.zip
links<-"https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/37823/importacion_de_vehiculos_2019_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/37823/importacion_de_vehiculos_2019_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/36826/importacion_de_vehiculos_2019_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/36826/importacion_de_vehiculos_2019_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/35652/importacion_de_vehiculos_2018_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/35652/importacion_de_vehiculos_2018_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/34638/importacion_de_vehiculos_2018_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/34638/importacion_de_vehiculos_2018_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/31996/importacion_de_vehiculos_2018_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/31996/importacion_de_vehiculos_2018_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/30363/importacion_de_vehiculos_2018_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/30363/importacion_de_vehiculos_2018_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/29593/importacion_de_vehiculos_2018_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/29593/importacion_de_vehiculos_2018_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/28314/importacion_de_vehiculos_2018_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/28314/importacion_de_vehiculos_2018_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/27204/importacion_de_vehiculos_2018_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/27204/importacion_de_vehiculos_2018_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/25863/importacion_de_vehiculos_2018_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/25863/importacion_de_vehiculos_2018_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/25042/importacion_de_vehiculos_2018_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/25042/importacion_de_vehiculos_2018_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/24875/importacion_de_vehiculos_2018_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/24875/importacion_de_vehiculos_2018_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/21875/importacion_de_vehiculos_2018_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/21875/importacion_de_vehiculos_2018_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/20731/importacion_de_vehiculos_2018_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/20731/importacion_de_vehiculos_2018_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/18843/importacion_de_vehiculos_2017_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/18843/importacion_de_vehiculos_2017_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/18243/importacion_de_vehiculos_2017_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/18243/importacion_de_vehiculos_2017_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/17540/importacion_de_vehiculos_2017_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/17540/importacion_de_vehiculos_2017_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/16683/importacion_de_vehiculos_2017_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/16683/importacion_de_vehiculos_2017_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4870/importacion_de_vehiculos_2017_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4870/importacion_de_vehiculos_2017_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4863/importacion_de_vehiculos_2017_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4863/importacion_de_vehiculos_2017_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4857/importacion_de_vehiculos_2017_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4857/importacion_de_vehiculos_2017_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4850/importacion_de_vehiculos_2017_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4850/importacion_de_vehiculos_2017_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4843/importacion_de_vehiculos_2017_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4843/importacion_de_vehiculos_2017_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4836/importacion_de_vehiculos_2017_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4836/importacion_de_vehiculos_2017_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4829/importacion_de_vehiculos_2017_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4829/importacion_de_vehiculos_2017_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4822/importacion_de_vehiculos_2017_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4822/importacion_de_vehiculos_2017_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4890/importacion_de_vehiculos_2016_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4890/importacion_de_vehiculos_2016_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4884/importacion_de_vehiculos_2016_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4884/importacion_de_vehiculos_2016_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4881/importacion_de_vehiculos_2016_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4881/importacion_de_vehiculos_2016_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4875/importacion_de_vehiculos_2016_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4875/importacion_de_vehiculos_2016_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4869/importacion_de_vehiculos_2016_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4869/importacion_de_vehiculos_2016_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4862/importacion_de_vehiculos_2016_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4862/importacion_de_vehiculos_2016_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4855/importacion_de_vehiculos_2016_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4855/importacion_de_vehiculos_2016_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4849/importacion_de_vehiculos_2016_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4849/importacion_de_vehiculos_2016_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4842/importacion_de_vehiculos_2016_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4842/importacion_de_vehiculos_2016_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4835/importacion_de_vehiculos_2016_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4835/importacion_de_vehiculos_2016_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4828/importacion_de_vehiculos_2016_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4828/importacion_de_vehiculos_2016_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4821/importacion_de_vehiculos_2016_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4821/importacion_de_vehiculos_2016_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4889/importacion_de_vehiculos_2015_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4889/importacion_de_vehiculos_2015_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4885/importacion_de_vehiculos_2015_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4885/importacion_de_vehiculos_2015_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4879/importacion_de_vehiculos_2015_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4879/importacion_de_vehiculos_2015_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4874/importacion_de_vehiculos_2015_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4874/importacion_de_vehiculos_2015_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4868/importacion_de_vehiculos_2015_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4868/importacion_de_vehiculos_2015_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4861/importacion_de_vehiculos_2015_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4861/importacion_de_vehiculos_2015_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4854/importacion_de_vehiculos_2015_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4854/importacion_de_vehiculos_2015_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4848/importacion_de_vehiculos_2015_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4848/importacion_de_vehiculos_2015_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4841/importacion_de_vehiculos_2015_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4841/importacion_de_vehiculos_2015_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4834/importacion_de_vehiculos_2015_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4834/importacion_de_vehiculos_2015_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4827/importacion_de_vehiculos_2015_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4827/importacion_de_vehiculos_2015_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4820/importacion_de_vehiculos_2015_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4820/importacion_de_vehiculos_2015_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4888/importacion_de_vehiculos_2014_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4888/importacion_de_vehiculos_2014_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4883/importacion_de_vehiculos_2014_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4883/importacion_de_vehiculos_2014_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4878/importacion_de_vehiculos_2014_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4878/importacion_de_vehiculos_2014_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4873/importacion_de_vehiculos_2014_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4873/importacion_de_vehiculos_2014_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4867/importacion_de_vehiculos_2014_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4867/importacion_de_vehiculos_2014_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4860/importacion_de_vehiculos_2014_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4860/importacion_de_vehiculos_2014_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4853/importacion_de_vehiculos_2014_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4853/importacion_de_vehiculos_2014_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4847/importacion_de_vehiculos_2014_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4847/importacion_de_vehiculos_2014_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4840/importacion_de_vehiculos_2014_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4840/importacion_de_vehiculos_2014_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4833/importacion_de_vehiculos_2014_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4833/importacion_de_vehiculos_2014_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4826/importacion_de_vehiculos_2014_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4826/importacion_de_vehiculos_2014_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4877/importacion_de_vehiculos_2013_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4877/importacion_de_vehiculos_2013_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4872/importacion_de_vehiculos_2013_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4872/importacion_de_vehiculos_2013_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4866/importacion_de_vehiculos_2013_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4866/importacion_de_vehiculos_2013_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4859/importacion_de_vehiculos_2013_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4859/importacion_de_vehiculos_2013_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4852/importacion_de_vehiculos_2013_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4852/importacion_de_vehiculos_2013_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4846/importacion_de_vehiculos_2013_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4846/importacion_de_vehiculos_2013_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4839/importacion_de_vehiculos_2013_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4839/importacion_de_vehiculos_2013_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4832/importacion_de_vehiculos_2013_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4832/importacion_de_vehiculos_2013_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4825/importacion_de_vehiculos_2013_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4825/importacion_de_vehiculos_2013_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4819/importacion_de_vehiculos_2013_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4819/importacion_de_vehiculos_2013_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4887/importacion_de_vehiculos_2012_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4887/importacion_de_vehiculos_2012_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4882/importacion_de_vehiculos_2012_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4882/importacion_de_vehiculos_2012_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4876/importacion_de_vehiculos_2012_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4876/importacion_de_vehiculos_2012_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4871/importacion_de_vehiculos_2012_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4871/importacion_de_vehiculos_2012_septiembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4864/importacion_de_vehiculos_2012_agosto-zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4864/importacion_de_vehiculos_2012_agosto-zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4858/importacion_de_vehiculos_2012_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4858/importacion_de_vehiculos_2012_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4851/importacion_de_vehiculos_2012_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4851/importacion_de_vehiculos_2012_junio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4845/importacion_de_vehiculos_2012_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4845/importacion_de_vehiculos_2012_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4838/importacion_de_vehiculos_2012_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4838/importacion_de_vehiculos_2012_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4831/importacion_de_vehiculos_2012_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4831/importacion_de_vehiculos_2012_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4824/importacion_de_vehiculos_2012_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4824/importacion_de_vehiculos_2012_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4818/importacion_de_vehiculos_2012_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4818/importacion_de_vehiculos_2012_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4886/importacion_de_vehiculos_2011_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4886/importacion_de_vehiculos_2011_diciembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4880/importacion_de_vehiculos_2011_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4880/importacion_de_vehiculos_2011_noviembre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4891/importacion_de_vehiculos_2011_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4891/importacion_de_vehiculos_2011_octubre.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4865/importacion_de_vehiculos_2011_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4865/importacion_de_vehiculos_2011_agosto.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4856/importacion_de_vehiculos_2011_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4856/importacion_de_vehiculos_2011_julio.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4844/importacion_de_vehiculos_2011_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4844/importacion_de_vehiculos_2011_mayo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4837/importacion_de_vehiculos_2011_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4837/importacion_de_vehiculos_2011_abril.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4830/importacion_de_vehiculos_2011_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4830/importacion_de_vehiculos_2011_marzo.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4823/importacion_de_vehiculos_2011_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4823/importacion_de_vehiculos_2011_febrero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4817/importacion_de_vehiculos_2011_enero.zip

https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/4817/importacion_de_vehiculos_2011_enero.zip"




links<-str_trim(unlist(strsplit(links,"[[:cntrl:]]")))
links<-links[links!=""]

for (vinculo in links) {
  if (!file.exists(paste("datos/",basename(vinculo)))){
    download.file(vinculo,paste("datos/",basename(vinculo)))
    Sys.sleep(5)
  }
}

#Extraer primero los archivos .zip y porner el working directory 
# de R a leer de la carpeta donde est?n los txt
# PAra leer y unir todo

listaArchivos<-list.files(getwd())
head(listaArchivos,30)
dataset <-data.frame()
for (archivo in listaArchivos){
  print (archivo)
  # if (file_ext(archivo) == "zip")
  #   archivo<-unzip(archivo)
  print (archivo)
  if (file_ext(archivo) == "txt"){
    if (!exists("dataset")){
      dataset<-read.table(archivo,sep = "|", stringsAsFactors = F)
    }
    else{
      temp_dataset <-read.table(archivo,sep = "|", stringsAsFactors = F,row.names = NULL)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
}


names(dataset)<-names(dataset)[2:length(names(dataset))]
dataset<-dataset[,1:ncol(dataset)-1]
dataset$DatePoliza<-dmy(dataset$Fecha.de.la.Poliza)
dataset$Anio<-year(dataset$DatePoliza)
dataset$Mes<-month(dataset$DatePoliza)
dataset$Dia<-day(dataset$DatePoliza)
dataset$DiaSem<-wday(dataset$DatePoliza)
dataset$DatePoliza<-NULL

dataset[dataset$Modelo.del.Vehiculo == 3015,"Modelo.del.Vehiculo"]<-2015
write.csv(dataset, file="importacionesVehiculosSAT.csv",row.names = F)
save(dataset, file="importacionesSAT.RData")
