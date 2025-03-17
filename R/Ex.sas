* Exemplo de cálculo de matriz de Covariâncias e de Correlações amostrais, além da distância de Mahalanobis.

proc iml;
y1 = {72,60,56,41,32,30,39,42,37,33,32,63,54,47, 91,56,79,81,78,46, 39,32,60,35,39,50,43,48};
y2 = {66,53,57,29,32,35,39,43,40,29,30,45,46,51, 79,68,65,80,55,38, 35,30,50,37,36,34,37,54};
y3 = {76,66,64,36,35,34,31,31,31,27,34,74,60,52,100,47,70,68,67,37, 34,30,67,48,39,37,39,57};
y4 = {77,63,58,38,36,26,27,25,25,36,28,63,52,43, 75,50,61,58,60,38, 37,32,54,39,31,40,50,43};

Y = y1||y2||y3||y4;
create Cork var {North East South West};
append from Y;
Close Cork;
