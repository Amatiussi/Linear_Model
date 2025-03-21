%let r=-0.75; * Fixa o coeficiente de correlação entre y1 e y2;
data Normal;
 pi=3.1416;
 do y1=-4 to 4 by 0.1;
  do y2=-4 to 4 by 0.1; 
  phi=exp(-(y1*y1-2*&r*y1*y2+y2*y2)/2/(1-&r*&r))/2/pi/sqrt(1-&r*&r);
  output;
  end;
 end;
run;

goptions reset=all border;
proc g3d data=Normal;
 title 'Densidade Normal Bivariada (r =' &r ')';
 plot y1*y2=phi / rotate=-20;
run;
