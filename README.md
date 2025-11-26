Este script utiliza microdados da Amostra de Pessoas do Censo Demográfico de 2010, produzidos pelo Instituto Brasileiro de Geografia e Estatística (IBGE), especificamente referentes aos 14 municípios analisados no trabalho.

Os arquivos de dados não foram incluídos neste repositório devido ao seu tamanho elevado e ao caráter oficial da base. Para que o código funcione corretamente, é necessário que o usuário baixe tanto este repositório quanto os microdados diretamente no site do IBGE e insira os arquivos exigidos na pasta dados/ do projeto.

Primeiro, faça o download deste repositório clicando em Code → Download ZIP no GitHub. Após o download, descompacte o arquivo ZIP em uma pasta de sua preferência. A pasta que contém o arquivo “Código Final - Econometria.R” será a pasta principal do projeto.

Em seguida, faça o download dos microdados no site oficial do IBGE, pelo seguinte link:
https://www.ibge.gov.br/estatisticas/sociais/trabalho/9662-censo-demografico-2010.html?=&t=microdados

Ao acessar a página, role até encontrar a seção “Microdados para os 14 municípios com as áreas redefinidas” e realize o download do arquivo correspondente. Após o download, extraia (descompacte) o arquivo ZIP. É comum que o sistema crie uma pasta dentro de outra com o mesmo nome; nesse caso, utilize a pasta mais interna, que contém efetivamente os arquivos e subpastas.

Dentro dessa pasta, localize a estrutura chamada microdados_14_municipios_com_areas_redefinidas. A partir dela, siga exatamente este caminho:
primeiro, abra a pasta Dados e copie o arquivo Amostra_Pessoas_14munic.txt;
em seguida, volte à pasta principal, entre em Documentação e depois em Layout, e copie o arquivo Layout_microdados_Amostra_14_munic_20160301.xls.

Depois disso, abra a pasta do projeto deste repositório (a que contém o arquivo “Código Final - Econometria.R”) e cole esses dois arquivos dentro da pasta dados/.

Ao final, a pasta dados/ deverá conter exatamente os seguintes arquivos: Amostra_Pessoas_14munic.txt e Layout_microdados_Amostra_14_munic_20160301.xls.

Com os arquivos corretamente posicionados, o script “Código Final - Econometria.R” poderá ser executado normalmente no RStudio, permitindo a replicação integral das análises, estimativas econométricas e geração das tabelas e gráficos apresentados no trabalho.
