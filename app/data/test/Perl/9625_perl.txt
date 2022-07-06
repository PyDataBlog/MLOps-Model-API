conecta(1,1).
conecta(x,0).
conecta(0,0).
conecta(0,1).
conecta(0,y).
conecta(x,1).
conecta(1,y).
viceversa(X,Y):-conecta(X,Y);conecta(Y,X).

sol :- camino([x],Sol),write(Sol).

camino([y|RestoDelCamino],[y|RestoDelCamino]).
camino([PosActual|RestoDelCamino],Sol) :- conectado(PosActual,PosSiguiente),\+ miembro(PosSiguiente,RestoDelCamino),
					   camino([PosSiguiente,PosActual|RestoDelCamino],Sol),PosSiguiente\=1.
