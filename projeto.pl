
%Seletores da estrutura ilha
coords_ilha(ilha(_,(X,Y)),(X,Y)).
obter_pontes(ilha(N,_),N).
%2.1
extrai_ilhas_linha(Num_Linha,Linha,Ilhas):-
    extrai_ilhas_linha(Num_Linha,0,Linha,Ilhas).

extrai_ilhas_linha(_,_,[],[]).

extrai_ilhas_linha(Num_Linha,N_C,[P|Q],[ilha(P,(Num_Linha,New_C))|Aux]):-
    P\==0,
    New_C is N_C+1,
    extrai_ilhas_linha(Num_Linha,New_C,Q,Aux).

extrai_ilhas_linha(Num_Linha,N_C,[P|Q],Aux):-
    P==0,
    New_C is N_C+1,
    extrai_ilhas_linha(Num_Linha,New_C,Q,Aux).

%2.2

ilhas(Puz,Ilhas):-
    ilhas(Puz,0,Aux),
    flatten(Aux,Ilhas).

ilhas([],_,[]).

ilhas([P|Q],Cont,[Aux|Ilhas]):-
    New_Cont is Cont+1,
    extrai_ilhas_linha(New_Cont,P,Aux),
    ilhas(Q,New_Cont,Ilhas).
 
%2.3
vizinhas(Ilhas,Ilha,Vizinhas):-
    nth1(Index,Ilhas,Ilha),
    findall(Membro,(member(Membro,Ilhas),mesmo_y(Membro,Ilha),nth1(Index_atual,Ilhas,Membro),Index_atual=<Index),C_Temp),
    findall(Membro,(member(Membro,C_Temp),nextto(Membro,Ilha,C_Temp)),C),
    findall(Membro,(member(Membro,Ilhas),mesmo_x(Membro,Ilha),nth1(Index_atual,Ilhas,Membro),Index_atual=<Index),E_Temp),
    findall(Membro,(member(Membro,E_Temp),nextto(Membro,Ilha,E_Temp)),E),
    findall(Membro,(member(Membro,Ilhas),mesmo_x(Membro,Ilha),nth1(Index_atual,Ilhas,Membro),Index_atual>=Index),D_Temp),
    findall(Membro,(member(Membro,D_Temp),nextto(Ilha,Membro,D_Temp)),D),
    findall(Membro,(member(Membro,Ilhas),mesmo_y(Membro,Ilha),nth1(Index_atual,Ilhas,Membro),Index_atual>=Index),B_Temp),
    findall(Membro,(member(Membro,B_Temp),nextto(Ilha,Membro,B_Temp)),B),
    append(C,E,C_E),
    append(D,B,D_B),
    append(C_E,D_B,Vizinhas).

mesmo_x(ilha(_,(X,_)),ilha(_,(X,_))).
mesmo_y(ilha(_,(_,Y)),ilha(_,(_,Y))).

%2.4

estado(Ilhas,Estado):-
    estado(Ilhas,Ilhas,Estado).

estado([],_,[]).

estado([P|Q],Ilhas,[[P,Vizinhas,[]]|Aux]):-
    vizinhas(Ilhas,P,Vizinhas),
    estado(Q,Ilhas,Aux).

%2.5
posicoes_entre(Pos1,Pos2,Posicoes):-
    posicoes_entre_aux(Pos1,Pos2,Pos),
    reverse(Pos, [_|Cauda]),
    reverse(Cauda,Posicoes).
    
posicoes_entre_aux(X,X,[]).

posicoes_entre_aux((X,Y1),(X,Y2),[(X,New_Y)|Posicoes]):-
    Y2>Y1,
    New_Y is Y1+1,
    posicoes_entre_aux((X,New_Y),(X,Y2),Posicoes).

posicoes_entre_aux((X,Y1),(X,Y2),[(X,New_Y)|Posicoes]):-
    Y2<Y1,
    New_Y is Y2+1,
    posicoes_entre_aux((X,Y1),(X,New_Y),Posicoes).

posicoes_entre_aux((X1,Y),(X2,Y),[(New_X,Y)|Posicoes]):-
    X2>X1,
    New_X is X1+1,
    posicoes_entre_aux((New_X,Y),(X2,Y),Posicoes).

posicoes_entre_aux((X1,Y),(X2,Y),[(New_X,Y)|Posicoes]):-
    X2<X1,
    New_X is X2+1,
    posicoes_entre_aux((X1,Y),(New_X,Y),Posicoes).


%2.6
%Construtor
cria_ponte(Pos1,Pos2,ponte(Menor,Maior)):-
    sort([Pos1,Pos2],[Menor,Maior]).

%2.7

caminho_livre(Pos1,Pos2,_,I,Vz):-
    coords_ilha(I,Pos1),coords_ilha(Vz,Pos2).

caminho_livre(Pos1,Pos2,_,I,Vz):-
    coords_ilha(I,Pos2),coords_ilha(Vz,Pos1).

caminho_livre(_,_,Posicoes1,I,Vz):-
    coords_ilha(I,Coord_I),
    coords_ilha(Vz,Coord_Vz),
    posicoes_entre(Coord_I,Coord_Vz,Posicoes2),
    length(Posicoes2,Len1),
    subtract(Posicoes2,Posicoes1,Intersected),
    length(Intersected,Len1).

%2.8

actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[Ilha,Vizinhas|Resto],[Ilha,Nova_vizinhas|Resto]):-
    aux(Pos1,Pos2,Posicoes,Ilha,Vizinhas,Nova_vizinhas).

aux(_,_,_,_,[],[]).

aux(Pos1,Pos2,Posicoes,Ilha,[Atual_Vizinha|Resto],[Atual_Vizinha|Nova_vizinhas]):-
    caminho_livre(Pos1,Pos2,Posicoes,Ilha,Atual_Vizinha),
    aux(Pos1,Pos2,Posicoes,Ilha,Resto,Nova_vizinhas).

aux(Pos1,Pos2,Posicoes,Ilha,[Atual_Vizinha|Resto],Nova_vizinhas):-
    \+caminho_livre(Pos1,Pos2,Posicoes,Ilha,Atual_Vizinha),
    aux(Pos1,Pos2,Posicoes,Ilha,Resto,Nova_vizinhas).

    

%2.9

actualiza_vizinhas_apos_pontes([],_,_,[]).

actualiza_vizinhas_apos_pontes([Entrada|Resto],Pos1,Pos2,[Nova_entrada|Novo_estado]):-
    posicoes_entre(Pos1,Pos2,Posicoes),
    actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,Entrada,Nova_entrada),
    actualiza_vizinhas_apos_pontes(Resto,Pos1,Pos2,Novo_estado).

%2.10

ilhas_terminadas([],[]).

ilhas_terminadas([[Ilha,_,Pontes]|Resto_Estado],[Ilha|Ilhas_terminadas]):-
    obter_pontes(Ilha,P),
    length(Pontes,Len),
    P\=='X',
    P==Len,
    ilhas_terminadas(Resto_Estado,Ilhas_terminadas).

ilhas_terminadas([[Ilha,_,_]|Resto_Estado],Ilhas_terminadas):-
    obter_pontes(Ilha,P),
    P=='X',
    ilhas_terminadas(Resto_Estado,Ilhas_terminadas).

ilhas_terminadas([[Ilha,_,Pontes]|Resto_Estado],Ilhas_terminadas):-
    obter_pontes(Ilha,P),
    length(Pontes,Len),
    P\=='X',
    P\==Len,
    ilhas_terminadas(Resto_Estado,Ilhas_terminadas).

%2.11

tira_ilhas_terminadas_entrada(Ilhas_term,[Ilha,Vizinhas|Resto],[Ilha,Nova_vizinhas|Resto]):-
    subtract(Vizinhas,Ilhas_term,Nova_vizinhas).

%2.12

tira_ilhas_terminadas(Estado,Ilhas_term,Novo_estado):-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado,Novo_estado).
    
%2.13

marca_ilhas_terminadas_entrada(Ilhas_term,[Ilha|Resto],[ilha('X',Coords)|Resto]):-
    member(Ilha,Ilhas_term),
    coords_ilha(Ilha,Coords).

marca_ilhas_terminadas_entrada(Ilhas_term,[Ilha|Resto],[Ilha|Resto]):-
    \+member(Ilha,Ilhas_term).

%2.14

marca_ilhas_terminadas(Estado,Ilhas_term,Novo_estado):-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term),Estado, Novo_estado).

%2.15

trata_ilhas_terminadas(Estado,Novo_estado):-
    ilhas_terminadas(Estado,Ilhas_term),
    tira_ilhas_terminadas(Estado,Ilhas_term,Novo),
    marca_ilhas_terminadas(Novo,Ilhas_term,Novo_estado).

%2.16

junta_pontes(Estado,Num_Pontes,Ilha1,Ilha2,Novo_estado4):-
    coords_ilha(Ilha1,Coord1),
    coords_ilha(Ilha2,Coord2),
    cria_ponte(Coord1,Coord2,Ponte),
    adiciona_pontes_estado(Estado,Num_Pontes,Ponte,Ilha1,Novo_estado1),
    adiciona_pontes_estado(Novo_estado1,Num_Pontes,Ponte,Ilha2,Novo_estado2),
    actualiza_vizinhas_apos_pontes(Novo_estado2,Coord1,Coord2,Novo_estado3),
    trata_ilhas_terminadas(Novo_estado3,Novo_estado4).

% %Auxiliares 2.16

adiciona_pontes_estado([],_,_,_,[]).

adiciona_pontes_estado([[Ilha1,Vizinhas,Pontes]|Resto],Num_Pontes,Ponte,Ilha1,[Nova_entrada|Novo_estado]):-
    adiciona_pontes_entrada([Ilha1,Vizinhas,Pontes],Num_Pontes,Ponte,Nova_entrada),
    adiciona_pontes_estado(Resto,Num_Pontes,Ponte,Ilha1,Novo_estado).

adiciona_pontes_estado([[Ilha1,Vizinhas,Pontes]|Resto],Num_Pontes,Ponte,Ilha,[[Ilha1,Vizinhas,Pontes]|Novo_estado]):-
    Ilha1\==Ilha,
    adiciona_pontes_estado(Resto,Num_Pontes,Ponte,Ilha,Novo_estado).

adiciona_pontes_entrada([Ilha,Vizinhas,Pontes],Num_Pontes,Ponte,[Ilha,Vizinhas,Pontes_Final]):-
    adiciona_pontes(Num_Pontes,Ponte,Nova_Pontes),
    append([Pontes,Nova_Pontes],Pontes_Final).

adiciona_pontes(0,_,[]).

adiciona_pontes(Num_Pontes,Ponte,[Ponte|Nova_Pontes]):-
    Num_Pontes\==0,
    Novo_N is Num_Pontes-1,
    adiciona_pontes(Novo_N,Ponte,Nova_Pontes).
