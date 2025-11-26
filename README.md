# Trabalho-Econometria---Gabriel-Gusmao-e-Sofia-Torneri
Este script utiliza microdados da Amostra de Pessoas do Censo Demográfico de 2010, produzidos pelo Instituto Brasileiro de Geografia e Estatística (IBGE), especificamente referentes aos 14 municípios analisados no trabalho.

Os arquivos de dados não foram incluídos neste repositório devido ao seu tamanho elevado e ao caráter oficial da base. Para que o código funcione corretamente, é necessário que o usuário baixe os microdados diretamente no site do IBGE e insira os arquivos exigidos na pasta dados/ deste projeto.

O download deve ser feito a partir do seguinte link oficial do IBGE:
https://www.ibge.gov.br/estatisticas/sociais/trabalho/9662-censo-demografico-2010.html?=&t=microdados

Ao acessar a página, role até encontrar a seção “Microdados para os 14 municípios com as áreas redefinidas” e realize o download do arquivo correspondente. Após o download, descompacte a pasta e localize a estrutura chamada microdados_14_municipios_com_areas_redefinidas.

Dentro dessa pasta, siga exatamente este caminho:
Primeiro, abra a pasta principal e entre em Dados, onde deverá copiar o arquivo Amostra_Pessoas_14munic.txt.
Em seguida, volte à pasta principal, entre em Documentação e depois em Layout, onde deverá copiar o arquivo Layout_microdados_Amostra_14_munic_20160301.xls.

Esses dois arquivos devem ser colados dentro da pasta dados/ presente neste repositório do GitHub.

Ao final, a pasta dados/ deverá conter exatamente os seguintes arquivos:
Amostra_Pessoas_14munic.txt
Layout_microdados_Amostra_14_munic_20160301.xls

Com os arquivos corretamente posicionados, o script “Código Final - Econometria.R” poderá ser executado normalmente no RStudio, permitindo a replicação integral das análises, estimativas econométricas e geração das tabelas e gráficos apresentados no trabalho.
