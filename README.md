# Projeto-1-Analise-de-Dados
Estudo sobre Analise de Dados usando R

- Tratamento, Exploração e Análise dos dados COVID-SP-Master.
  
Meu objetivo foi análisar três municipios perto de onde eu moro e análisar sobre os casos e obitos durante a época do inicio da pandemia.

Processos do Projeto:
- Com a linguagem R foi capaz de algumas análises estatiticas e ver a media, media movel, mediana, moda.
- Criação de Histograma para a verificação de frequência dos casos e dos obitos.
- Medidas de posição que podem dar informações sobre o minimo, maximo, amplitude total, quartis, amplitude interquartil que pode ajudar com o desenvolvimento do Box Plot, que ultiliza a análise desses quartis para a variação dos dados.
- Com a existência de Outliers que com o R consiguimos excluir pois são dados que podem atrapalhar nos resultados estatisticos.
- Medidas de Dispersão podemos ver a Variança dos dados de obitos e casos e também o Desvio Padrão.
- Seguindo para os testes de normalidade dos dados, que existem quatro testes conhecidos Shapiro-Wilk, Anderson-Darling, Kolmogorov_Smirnov e Cramer-Von Mises
- A Correlação Linear pode-se ver a parte do grau de relacionamento de duas variaveis que são os obitos e casos, nesse caso vamos usar o metodo spearman por conta da grande quantidade de dados.
- Fiz o uso do package ggplot que é uma forma mais atualizada de fazer gráficos em R, também tem como fazer a matriz de correlação.
- E por ultimo um Grafico Linear por cidade.

Repositório de onde peguei os dados: https://github.com/seade-R/dados-covid-sp
