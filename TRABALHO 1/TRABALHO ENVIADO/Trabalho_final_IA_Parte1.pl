% -*- Prolog -*-

:- use_module(library(lists)).

% Operadores para invariantes
:- op(1150, fx, +).
:- op(1150, fx, -).
:- op(1100, xfx, ::).

:- discontiguous (+)/1.
:- discontiguous (-)/1.
:- discontiguous teste/1.
:- discontiguous idade_valida/1.
:- discontiguous evolucao/1.
:- discontiguous insercao/1.
:- discontiguous si/2.
:- discontiguous remove_negativo_tensao/6.

% Tornar dinâmicos os predicados que podem ser alterados
:- dynamic paciente/8.
:- dynamic consulta/8.
:- dynamic tensao_arterial_negativa/6.
:- dynamic excecao/1.
:- dynamic interdito/1.
:- dynamic si/2.  
:- dynamic('-'/1).

:- op(900, xfy, '::').
:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).

% ------------------------------------------------------------------------
% Conversao de data para numero (para comparar datas)
% ------------------------------------------------------------------------
date_to_num(date(D,M,Y), N) :- N is Y*10000 + M*100 + D.

% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
% --- Pressuposto Mundo Fechado ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Definição do operador 'nao'
nao(Q) :- Q, !, fail.
nao(_).

% PMC para paciente/8
-paciente(Id, Nome, Data, Sexo, Morada, Altura, Peso, Hist) :-
    nao(paciente(Id, Nome, Data, Sexo, Morada, Altura, Peso, Hist)),
    nao(excecao(paciente(Id, Nome, Data, Sexo, Morada, Altura, Peso, Hist))).

% PMC para consulta/8
-consulta(IdC, IdP, Data, Obs, Sist, Diast, Pulso, Estado) :-
    nao(consulta(IdC, IdP, Data, Obs, Sist, Diast, Pulso, Estado)),
    nao(excecao(consulta(IdC, IdP, Data, Obs, Sist, Diast, Pulso, Estado))).

% PMC para tensao_arterial/6
-tensao_arterial(Id, Class, SistMin, SistMax, DiastMin, DiastMax) :-
    nao(tensao_arterial(Id, Class, SistMin, SistMax, DiastMin, DiastMax)),
    nao(excecao(tensao_arterial(Id, Class, SistMin, SistMax, DiastMin, DiastMax))).

% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
% --- Conhecimento perfeito positivo -----------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pacientes Femininos (f)
paciente(1, maria, date(20, 5, 1985), f, braga, 165, 62.5, nulo_val).
paciente(2, ana, date(15, 8, 2001), f, porto, 158, 55.2, nulo_val).
paciente(3, joana, date(1, 4, 2010), f, lisboa, 140, 40.0, nulo_val).
paciente(4, luisa, date(19, 1, 1953), f, coimbra, 160, 70.3, nulo_val).
paciente(5, nadia, date(5, 12, 2005), f, faro, 150, 48.6, nulo_val).

% Pacientes Masculinos (m)
paciente(6, joao, date(3, 11, 1972), m, aveiro, 178, 85.0, nulo_val).
paciente(7, jose, date(28, 2, 1960), m, setubal, 172, 92.1, nulo_val).
paciente(8, antonio, date(10, 9, 1995), m, viseu, 185, 75.8, nulo_val).
paciente(9, manuel, date(25, 7, 1988), m, leiria, 175, 79.9, nulo_val).
paciente(10, carlos, date(17, 6, 1977), m, santarem, 180, 88.7, nulo_val).

% Consultas Realizadas
consulta(1001, 1, date(12, 3, 2024), nulo_val, 118, 76, 72, realizada).
consulta(1002, 2, date(5, 4, 2024), nulo_val, 110, 70, 68, realizada).
consulta(1003, 3, date(22, 5, 2024), nulo_val, 102, 66, 85, realizada).
consulta(1004, 4, date(14, 6, 2024), nulo_val, 130, 84, 78, realizada).
consulta(1005, 5, date(2, 7, 2024), nulo_val, 115, 72, 74, realizada).

% Consultas Agendadas (sem medições)
consulta(1006, 6, date(19, 8, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).
consulta(1007, 7, date(9, 9, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).
consulta(1008, 8, date(1, 10, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).

% Consultas Canceladas (sem medições)
consulta(1009, 9, date(20, 11, 2024), nulo_val, nulo_val, nulo_val, nulo_val, cancelada).
consulta(1010, 10, date(12, 12, 2024), nulo_val, nulo_val, nulo_val, nulo_val, cancelada).

% Tensão Arterial - Classificações
tensao_arterial(2001, otima,                       0, 119,   0,  79).
tensao_arterial(2002, normal,                    120, 129,  80,  84).
tensao_arterial(2003, normal_alta,               130, 139,  85,  89).
tensao_arterial(2004, hipertensao_grau1,         140, 159,  90,  99).
tensao_arterial(2005, hipertensao_grau2,         160, 179, 100, 109).
tensao_arterial(2006, hipertensao_grau3,         180, sem_limite, 110, sem_limite).
tensao_arterial(2007, hipertensao_sistolica_isolada, 140, sem_limite, 0, 89).

% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
% --- Conhecimento perfeito negativo -----------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Pacientes que não existem ou não pertencem à clínica
-paciente(11, sofia, date(10, 3, 1999), f, braga, 162, 58.2, nulo_val).
-paciente(12, ricardo, date(7, 9, 1980), m, porto, 175, 90.1, nulo_val).

% Consultas que não aconteceram (clínica fechada, feriados, ausência médica)
-consulta(1011, 1, date(25, 12, 2024), nulo_val, nulo_val, nulo_val, nulo_val, realizada).
-consulta(1012, 3, date(1, 1, 2025), nulo_val, nulo_val, nulo_val, nulo_val, realizada).

% Consultas que não foram agendadas
-consulta(1013, 6, date(10, 7, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).
-consulta(1014, 9, date(18, 8, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).

% Tensões arteriais inválidas ou impossíveis
-tensao_arterial(2008, invalida, 500, 600, 400, 500).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes Estruturais e Referenciais para o predicado consulta
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

% i. Nao permitir a insercao de conhecimento repetido
+consulta(ID, ID_Pac, Data, Idade, Sist, Diast, FC, Estado) :: 
    (findall((ID, ID_Pac, Data, Idade, Sist, Diast, FC, Estado),
             consulta(ID, ID_Pac, Data, Idade, Sist, Diast, FC, Estado), 
             S),
     length(S, N), 
     N == 0).

% ii. Nao permitir a insercao de consultas para pacientes inexistentes (usa si/2)
+consulta(_, ID_Pac, _, _, _, _, _, _) :: 
    paciente_existe_si(ID_Pac, verdadeiro).

% iii. Validar valores de pressão e frequência cardíaca
% (invariante de insercao: deve começar com +)
+consulta(_, _, _, _, Sist, Diast, FC, Estado) :: 
    ((Estado == realizada ->
        valida_medicoes_si(Sist, Diast, FC, verdadeiro)
     ;
        Sist == nulo_val, Diast == nulo_val, FC == nulo_val)).

% iv. Validar o estado da consulta (usa si/2 de forma mínima)
 +consulta(_, _, _, _, _, _, _, Estado) ::
     valida_estado_si(Estado, verdadeiro).

% v. Garantir que a data da consulta é posterior ao nascimento do paciente
+consulta(_, ID_Pac, date(DiaC, MesC, AnoC), _, _, _, _, _) :: 
    (paciente(ID_Pac, _, date(DiaN, MesN, AnoN), _, _, _, _, _),
     compara_datas_nasc(date(DiaC, MesC, AnoC), date(DiaN, MesN, AnoN))).

% vi. Garantir que cada consulta tem um ID único (não repetido)
+consulta(ID, _, _, _, _, _, _, _) ::
    \+ consulta(ID, _, _, _, _, _, _, _).

% vii. Garantir que um paciente só pode ter uma consulta agendada de cada vez
+consulta(_, ID_Paciente, _Data, _, _, _, _, Estado) :: 
    (findall(EstadoExistente,
             (consulta(_, ID_Paciente, _, _, _, _, _, EstadoExistente),
              EstadoExistente == agendada),
             L),
     length(L, N),
     (Estado == cancelada ; Estado == realizada ; N == 0)).

% viii. Impedir que o mesmo paciente tenha duas consultas na mesma data
+consulta(_, ID_Paciente, Data, _, _, _, _, Estado) ::
    (findall(EstadoExistente,
             (consulta(_, ID_Paciente, Data, _, _, _, _, EstadoExistente),
              pertence(EstadoExistente, [agendada, realizada])),
             L),
     length(L, N),
     (Estado == cancelada ; N == 0)).

% ix. Impedir alterações a consultas realizadas
+consulta(ID, ID_Paciente, _, _, _, _, _, _) ::
    (
        findall(E, consulta(ID, ID_Paciente, _, _, _, _, _, E), ListaEstados),
        (ListaEstados == []  
        ; (ListaEstados = [EstadoAntigo], EstadoAntigo \= realizada)) 
    ).

% x. Validar coerência entre pressão sistólica e diastólica
+consulta(_, _, _, _, Sist, Diast, _, Estado) :: 
    ((Estado == realizada -> Sist > Diast ; true)).

% xi. Impedir alteração de consultas canceladas
+consulta(ID, ID_Paciente, _, _, _, _, _, _Estado) ::
    (findall(EstadoAntigo,
             consulta(ID, ID_Paciente, _, _, _, _, _, EstadoAntigo),
             ListaEstados),
     (ListaEstados == [] ;
      (ListaEstados = [EstadoAntigo], EstadoAntigo \= cancelada))).

% xii. Impedir a remoção de consultas realizadas 
-consulta(_, _, _, _, _, _, _, Estado) :: 
    valida_remocao_si(Estado, verdadeiro).

% xiii. Impedir regressão de estados 
+consulta(ID, ID_Pac, _, _, _, _, _, EstadoNovo) ::
    (findall(EstadoAntigo,
             consulta(ID, ID_Pac, _, _, _, _, _, EstadoAntigo),
             Lista),
     (Lista == [] ;
      (Lista = [EstadoAntigo],
       transicao_valida(EstadoAntigo, EstadoNovo)))).

transicao_valida(agendada, cancelada).
transicao_valida(agendada, realizada).
transicao_valida(agendada, agendada).

% xiv. Impedir medições clínicas em consultas agendadas ou canceladas
+consulta(_, _, _, _, Sist, Diast, FC, Estado) ::
    (
        (Estado == realizada,
         valida_medicoes_si(Sist, Diast, FC, verdadeiro))
        ;
        (pertence(Estado, [agendada, cancelada]),
         Sist == nulo_val, Diast == nulo_val, FC == nulo_val)
    ).

% xv. Garantir que a idade na consulta corresponde à idade real do paciente
consulta(_, ID_Pac, date(Dc, Mc, Ac), Idade, _, _, _, _) ::
    idade_corresponde_si(ID_Pac, date(Dc, Mc, Ac), Idade, verdadeiro).


compara_datas_nasc(date(Dc,Mc,Ac), date(Dn,Mn,An)) :-
    ( Ac > An ) ;
    ( Ac == An, Mc > Mn ) ;
    ( Ac == An, Mc == Mn, Dc >= Dn ).


calcula_idade(date(Dn, Mn, An), date(Dc, Mc, Ac), Idade) :-
    IdadeTemp is Ac - An,
    ( (Mc < Mn) -> Idade is IdadeTemp - 1
    ; (Mc == Mn, Dc < Dn) -> Idade is IdadeTemp - 1
    ; Idade is IdadeTemp ).

% ---------------------------------------------
% Predicado unificado para editar consultas
% ---------------------------------------------
editar_consulta(ID_Consulta, NovaData, NovoEstado) :-
    pertence(NovoEstado, [agendada, cancelada]),
    consulta(ID_Consulta, ID_Paciente, _, _, _, _, _, EstadoAnt),
    EstadoAnt == agendada,
    paciente(ID_Paciente, _, DataNasc, _, _, _, _, _),
    calcula_idade(DataNasc, NovaData, NovaIdade),
    Sist = nulo_val,
    Diast = nulo_val,
    FC = nulo_val,
    retract(consulta(ID_Consulta, ID_Paciente, _, _, _, _, _, EstadoAnt)),
    evolucao_consulta(consulta(ID_Consulta, ID_Paciente, NovaData, NovaIdade, Sist, Diast, FC, NovoEstado)).

% ---------------------------------------------
% Caso para marcar como realizada (medições obrigatórias)
% ---------------------------------------------
editar_consulta(ID_Consulta, NovaData, Sist, Diast, FC, realizada) :-
    number(Sist), Sist > 0,
    number(Diast), Diast > 0, Sist > Diast,
    number(FC), FC > 0,
    consulta(ID_Consulta, ID_Paciente, _, _, _, _, _, EstadoAnt),
    EstadoAnt == agendada,
    paciente(ID_Paciente, _, DataNasc, _, _, _, _, _),
    calcula_idade(DataNasc, NovaData, NovaIdade),
    retract(consulta(ID_Consulta, ID_Paciente, _, _, _, _, _, EstadoAnt)),
    evolucao_consulta(consulta(ID_Consulta, ID_Paciente, NovaData, NovaIdade, Sist, Diast, FC, realizada)).


si_paciente(ID, Resultado) :-
    si(paciente(ID, _, _, _, _, _, _, _), Resultado).

si_consulta(ID, Resultado) :-
    si(consulta(ID, _, _, _, _, _, _, _), Resultado).

valida_estado_si(Estado, Resultado) :-
    si(pertence(Estado, [agendada, realizada, cancelada]), Resultado).

valida_medicoes_si(Sist, Diast, FC, Resultado) :-
    ( si((number(Sist), number(Diast), number(FC), Sist > 0, Diast > 0, FC > 0, Sist > Diast), verdadeiro) -> Resultado = verdadeiro
    ; si((Sist == nulo_val, Diast == nulo_val, FC == nulo_val), verdadeiro) -> Resultado = verdadeiro
    ; Resultado = desconhecido ).

idade_corresponde_si(ID_Pac, date(Dc,Mc,Ac), Idade, Resultado) :-
    paciente(ID_Pac, _, date(Dn,Mn,An), _, _, _, _, _) ->
        calcula_idade(date(Dn,Mn,An), date(Dc,Mc,Ac), IdadeReal),
        (Idade == IdadeReal -> Resultado = verdadeiro ; Resultado = falso)
    ; Resultado = desconhecido.


paciente_existe_si(ID, Resultado) :-
    si(paciente(ID, _, _, _, _, _, _, _), Resultado).

valida_remocao_si(Estado, verdadeiro) :-
    Estado \= realizada, !.
valida_remocao_si(Estado, falso) :-
    Estado == realizada, !.
valida_remocao_si(_, desconhecido).

pertence(X, [X|_]).
pertence(X, [_|T]) :-
    pertence(X, T).

teste([]).
teste([R|LR]) :- R, teste(LR).


evolucao(Termo) :-
    findall(Invar,+Termo::Invar,L),
    teste(L),          
    insercao(Termo).  

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

evolucao_consulta(Termo) :-
    evolucao(Termo).

involucao(Termo) :-
    findall(Invar,-Termo::Invar,Lista),
    teste(Lista),
    retract(Termo),
    !.

remocao_consulta(consulta(ID, ID_Pac, Data, Idade, Sist, Diast, FC, Estado)) :-
    Estado \= realizada,
    retract(consulta(ID, ID_Pac, Data, Idade, Sist, Diast, FC, Estado)).

idade_valida(Idade) :-
    number(Idade),
    Idade >= 0,
    Idade =< 150.

:- use_module(library(listing)).

nnatural(X) :- integer(X), X >= 0.

comprimento(S, N) :- length(S, N). 


% INVARIANTE ESTRUTURAL (+paciente/8): Não permitir a inserção de pacientes duplicados (baseado no ID).
+paciente(_, Nome, DataNasc, _, _, Altura, Peso, _) :: (
    Nome \= nulo_val,
    DataNasc \= nulo_val,
    Altura \= nulo_val,
    Peso \= nulo_val
).

% INVARIANTE REFERENCIAL (-paciente/8): Não permitir remover paciente se tiver consultas registadas.
-paciente(ID, _, _, _, _, _, _, _) :: (
    findall(C_ID, consulta(C_ID, ID, _, _, _, _, _, _), S_Consultas),
    comprimento(S_Consultas, N),
    N == 0
).

% INVARIANTE REFERENCIAL (+consulta/7): Validação da idade na consulta (máx. 150 anos).
+consulta(_, _, _, Idade, _, _, _, _) :: 
    (Idade == nulo_val ; idade_valida(Idade)).

% INVARIANTE ESTRUTURAL (+paciente/8): Nao permitir que um paciente tenha idade superior a 150 anos
paciente_idade_valida(date(_, _, AnoNasc)) :-
    AnoAtual = 2025,
    Idade is AnoAtual - AnoNasc,
    idade_valida(Idade).

+paciente(_, _, DataNasc, _, _, _, _, _) :: paciente_idade_valida(DataNasc).

idade_valida(Idade) :- integer(Idade), Idade >= 0, Idade =< 150.

% Invariant para evitar pacientes duplicados
+paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico) ::
    ( \+ paciente(ID, _, _, _, _, _, _, _) ).

si(Termo, verdadeiro) :-
    Termo.

si(Termo, falso) :-
    \+ Termo.

% ------------------------------------------------------------------------------------------------
% MECANISMOS DE EVOLUÇÃO E INVOLUÇÃO
% ------------------------------------------------------------------------------------------------

remove_paciente(ID) :-
    paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico), 
    involucao(paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico)).

% ---------------------------------------------------
% EDITAR PACIENTE (atualizar campos sem mudar ID)
% ---------------------------------------------------
editar_paciente(ID, nome, Novo) :-
    paciente(ID, _, D, S, M, A, P, H),
    retract(paciente(ID, _, D, S, M, A, P, H)),
    assert(paciente(ID, Novo, D, S, M, A, P, H)).

editar_paciente(ID, sexo, Novo) :-
    (Novo == m ; Novo == f),
    paciente(ID, N, D, _, M, A, P, H),
    retract(paciente(ID, N, D, _, M, A, P, H)),
    assert(paciente(ID, N, D, Novo, M, A, P, H)).

editar_paciente(ID, data_nasc, date(D,M,A)) :-
    nnatural(D), D>0, D=<31,
    nnatural(M), M>0, M=<12,
    nnatural(A), A>=1875, A=<2025,
    paciente(ID, N, _, S, Mrd, Alt, Peso, H),
    retract(paciente(ID, N, _, S, Mrd, Alt, Peso, H)),
    assert(paciente(ID, N, date(D,M,A), S, Mrd, Alt, Peso, H)).

editar_paciente(ID, morada, Novo) :-
    paciente(ID, N, D, S, _, Alt, Peso, H),
    retract(paciente(ID, N, D, S, _, Alt, Peso, H)),
    assert(paciente(ID, N, D, S, Novo, Alt, Peso, H)).

editar_paciente(ID, altura, Novo) :-
    number(Novo), Novo>0,
    paciente(ID, N, D, S, M, _, Peso, H),
    retract(paciente(ID, N, D, S, M, _, Peso, H)),
    assert(paciente(ID, N, D, S, M, Novo, Peso, H)).

editar_paciente(ID, peso, Novo) :-
    number(Novo), Novo>0,
    paciente(ID, N, D, S, M, Alt, _, H),
    retract(paciente(ID, N, D, S, M, Alt, _, H)),
    assert(paciente(ID, N, D, S, M, Alt, Novo, H)).

editar_paciente(ID, historico, Novo) :-
    paciente(ID, N, D, S, M, Alt, Peso, _),
    retract(paciente(ID, N, D, S, M, Alt, Peso, _)),
    assert(paciente(ID, N, D, S, M, Alt, Peso, Novo)).

% Calcula idade atual a partir de uma data date(Dia,Mes,Ano)
idade_atual(date(D,M,A), Idade) :-
    get_time(T),
    stamp_date_time(T, date(AY,AM,AD,_,_,_,_,_,_), local),
    (AM > M ; (AM == M, AD >= D)), !,
    Idade is AY - A.
idade_atual(date(_,_,A), Idade) :-
    get_time(T),
    stamp_date_time(T, date(AY,_,_,_,_,_,_,_,_), local),
    Idade is AY - A - 1.



% novo_paciente_com_id(+ID, +Nome, +DataNasc, +Sexo, +Altura, +Peso, +Morada, +Historico)
novo_paciente_com_id(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico) :-
    integer(ID), ID >= 0,
    Nome \== nulo_val, 
    DataNasc \== nulo_val,
    number(Altura), Altura > 0,
    number(Peso), Peso > 0,
    
    \+ paciente(ID, _, _, _, _, _, _, _),
    
    evolucao(paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico)),
    format('Paciente inserido com ID: ~w~n', [ID]).

evolucao(Termo) :-
    findall(Invar,+Termo::Invar,L),
    teste(L),
    insercao(Termo).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

teste([]).
teste([true|T]) :- teste(T).
teste([F|_]) :- F \= true, !, fail.

escolhe_max([(N,IDc,Date,Idade,Sis,Dis,Freq,Estado)], (N,IDc,Date,Idade,Sis,Dis,Freq,Estado)).
escolhe_max([(N1,ID1,Date1,Idade1,Sis1,Dis1,Freq1,Estado1)|T], Max) :-
    escolhe_max(T, TempMax),
    TempMax = (N2,_,_,_,_,_,_,_),
    (N1 >= N2 -> Max = (N1,ID1,Date1,Idade1,Sis1,Dis1,Freq1,Estado1) ; Max = TempMax).

% ------------------------------------------------------------------------
% Consulta mais recente de um paciente
% ------------------------------------------------------------------------

consulta_mais_recente_paciente(IDc, ID_P, Date, Idade, Sis, Dis, Freq, Estado) :-
    findall((Num, IDc0, Date0, Idade0, Sis0, Dis0, Freq0, Estado0),
            ( consulta(IDc0, ID_P, Date0, Idade0, Sis0, Dis0, Freq0, Estado0),
              Estado0 == realizada,
              number(Sis0), number(Dis0),
              date_to_num(Date0, Num) ),
            L),
    L \= [],
    escolhe_max(L, (_, IDc, Date, Idade, Sis, Dis, Freq, Estado)), !.
consulta_mais_recente_paciente(_, _, _, _, _, _, _, _) :- fail.

% ------------------------------------------------------------------------
% Verifica se um valor esta dentro de um intervalo
% ------------------------------------------------------------------------
in_range(Value, _, _) :-
    ( var(Value) ; Value == nulo_val ; Value == sem_limite ), !, fail.
in_range(_, Low, _) :-
    ( var(Low) ; Low == nulo_val ), !, fail.
in_range(_, _, High) :-
    ( var(High) ; High == nulo_val ), !, fail.
in_range(Value, Low, High) :-
    High == sem_limite, !, number(Value), Value >= Low.
in_range(Value, Low, High) :-
    number(Value), number(Low), number(High),
    Value >= Low, Value =< High.

% ------------------------------------------------------------------------
% Verifica se a tensao arterial bate com uma classificacao
% ------------------------------------------------------------------------
matches_classificacao(Sis, Dis, Class) :-
    number(Sis), number(Dis),
    tensao_arterial(_, Class, Sis_inf, Sis_sup, Dis_inf, Dis_sup),
    in_range(Sis, Sis_inf, Sis_sup),
    in_range(Dis, Dis_inf, Dis_sup).
matches_classificacao(_, _, _) :- fail.  % fallback

prioridade_classificacao([hipertensao_grau3, hipertensao_grau2, hipertensao_grau1,
                          hipertensao_sistolica_isolada, normal_alta, normal, otima]).

escolhe_prioridade([P|_], Lista, P) :- member(P, Lista), !.
escolhe_prioridade([_|T], Lista, C) :- escolhe_prioridade(T, Lista, C).


classificar_por_prioridade([P|_], Sis, Dis, P) :-
    matches_classificacao(Sis, Dis, P), !.
classificar_por_prioridade([_|T], Sis, Dis, Class) :-
    classificar_por_prioridade(T, Sis, Dis, Class).


classificar_por_tensao(Sis, Dis, incerto) :-
    Sis == nulo_val, Dis == nulo_val, !.
classificar_por_tensao(Sis, Dis, incerto) :-
    ( var(Sis) ; var(Dis) ; Sis == nulo_val ; Dis == nulo_val ), !.
classificar_por_tensao(Sis, Dis, incerto) :-
    \+ number(Sis) ; \+ number(Dis), !.
classificar_por_tensao(Sis, Dis, ClassFinal) :-
    findall((C, SisInf, SisSup, DisInf, DisSup),
            tensao_arterial(_, C, SisInf, SisSup, DisInf, DisSup),
            ListaClassificacoes),
    (encontrar_indice_valor(Sis, sistolica, ListaClassificacoes, IS) -> true ; IS = 0),
    (encontrar_indice_valor(Dis, diastolica, ListaClassificacoes, ID) -> true ; ID = 0),
    (IS =:= ID, IS > 0 ->
        nth1(IS, ListaClassificacoes, (ClassFinal, _, _, _, _))
    ; IS > 0, ID > 0 ->
        Delta is IS - ID,
        AbsD is abs(Delta),
        ( AbsD >= 2 ->
            ClassFinal = incerto
        ;
            MaxI is max(IS, ID),
            nth1(MaxI, ListaClassificacoes, (WorstClass, _, _, _, _)),
            ClassFinal = impreciso(WorstClass)
        )
    ;
        ClassFinal = incerto
    ).

encontrar_indice_valor(Valor, sistolica, Lista, Indice) :-
    encontrar_indice_sis(Valor, Lista, 1, Indice).

encontrar_indice_valor(Valor, diastolica, Lista, Indice) :-
    encontrar_indice_dis(Valor, Lista, 1, Indice).

% Procura sistólica
encontrar_indice_sis(Valor, [(_, SisInf, SisSup, _, _)|_], Idx, Idx) :-
    in_range(Valor, SisInf, SisSup), !.
encontrar_indice_sis(Valor, [_|T], Idx, Resultado) :-
    NextIdx is Idx + 1,
    encontrar_indice_sis(Valor, T, NextIdx, Resultado).

% Procura diastólica
encontrar_indice_dis(Valor, [(_, _, _, DisInf, DisSup)|_], Idx, Idx) :-
    in_range(Valor, DisInf, DisSup), !.
encontrar_indice_dis(Valor, [_|T], Idx, Resultado) :-
    NextIdx is Idx + 1,
    encontrar_indice_dis(Valor, T, NextIdx, Resultado).
% ------------------------------------------------------------------------
% Atualizacao do diagnostico do paciente com base na consulta mais recente
% ------------------------------------------------------------------------
atualizar_diagnostico_paciente_por_tensao(ID_P) :-
    consulta_mais_recente_paciente(_IDc, ID_P, Data, _Idade, Sis, Dis, _Freq, realizada),
    number(Sis), number(Dis), !,
    paciente(ID_P, Nome, DataNasc, Sexo, Morada, Altura, Peso, OldHist),
    (classificar_por_tensao(Sis, Dis, Class) -> true ; Class = indefinido),
    ( OldHist == nulo_val -> NewHist = [[Data, Class]]
    ; NewHist = [[Data, Class]|OldHist] ),
    retract(paciente(ID_P, Nome, DataNasc, Sexo, Morada, Altura, Peso, OldHist)),
    assert(paciente(ID_P, Nome, DataNasc, Sexo, Morada, Altura, Peso, NewHist)), !.
atualizar_diagnostico_paciente_por_tensao(_) :- fail.

atualizar_todos_pacientes_por_tensao :-
    findall(ID, paciente(ID,_,_,_,_,_,_,_), IDs),
    atualizar_lista_pacientes(IDs),
    true.  

atualizar_lista_pacientes([]).
atualizar_lista_pacientes([H|T]) :-
    ( atualizar_diagnostico_paciente_por_tensao(H) -> true ; true ),
    atualizar_lista_pacientes(T).

% Predicado principal
si(Objetivo, Resultado) :-
    avaliar(Objetivo, Resultado), !.
si(_, desconhecido).

% Avaliação dos objetivos
avaliar((A,B), Res) :- !,
    siC(A, B, Res).
avaliar((A;B), Res) :- !,
    siD(A, B, Res).
avaliar(nao(A), Res) :- !,
    avaliar(\+A, Res).
avaliar(\+A, Res) :- !,
    si(A, R), negar(R, Res).

avaliar(A = B, desconhecido) :-
    ( nonvar(A), A = impreciso(_) ; nonvar(B), B = impreciso(_) ), !.
avaliar(A == B, desconhecido) :-
    ( nonvar(A), A = impreciso(_) ; nonvar(B), B = impreciso(_) ), !.

avaliar(Objetivo, verdadeiro) :-
    catch(call(Objetivo), _, fail), !.

avaliar(Objetivo, falso) :-
    clause(-(Objetivo), _), !.

avaliar(Objetivo, incerto) :-
    ( excecao(Objetivo) -> true
    ; ( Objetivo = paciente(ID,_,_,_,_,_,_,_), paciente(ID,_,_,_,interdita,_,_,_) ) ), !.

avaliar(Objetivo, falso) :-
    \+ catch(call(Objetivo), _, fail), !.
avaliar(_, desconhecido).

negar(verdadeiro, falso) :- !.
negar(falso, verdadeiro) :- !.
negar(impreciso(C), impreciso(C)) :- !.
negar(incerto, incerto) :- !.
negar(desconhecido, incerto) :- !.
negar(_, incerto).

%-----------------------------------------------------------------------
% Extensão do meta-predicado siC: Questao1, Questao2, Resposta -> {V,F,D}
%-----------------------------------------------------------------------
siC( Q1, Q2, verdadeiro) :- 
	si(Q1, verdadeiro), 
	si(Q2, verdadeiro).
siC( Q1, Q2, falso) :- 
	si(Q1, verdadeiro), 
	si(Q2, falso).
siC( Q1, Q2, desconhecido) :- 
	si( Q1, verdadeiro), 
	si( Q2, desconhecido).
siC( Q1, Q2, falso) :- 
	si( Q1, falso), 
	si( Q2, verdadeiro).
siC( Q1, Q2, falso) :-
	si( Q1, falso),
	si( Q2, falso).
siC( Q1, Q2, falso) :-
	si( Q1, falso),
	si( Q2, desconhecido).
siC( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, verdadeiro).
siC( Q1, Q2, falso) :-
	si( Q1, desconhecido),
	si( Q2, falso).
siC( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, desconhecido).

%-----------------------------------------------------------------------
% Extensão do meta-predicado siD: Questao1, Questao2, Resposta -> {V,F,D}
%-----------------------------------------------------------------------
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
	si( Q2, verdadeiro).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
	si( Q2, falso).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
    si( Q2, desconhecido).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, falso),
	si( Q2, verdadeiro).
siD( Q1, Q2, falso) :-
	si( Q1, falso),
	si( Q2, falso).
siD( Q1, Q2, desconhecido) :-
	si( Q1, falso),
	si( Q2, desconhecido).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, desconhecido),
	si( Q2, verdadeiro).
siD( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, falso).
siD( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, desconhecido).

% ------------------------------------------------------------------------
% Pesquisar pacientes ou consultas (integrado ao estilo base)
% ------------------------------------------------------------------------
paciente_campos([
    id_paciente,
    nome,
    data_nascimento,
    sexo,
    morada,
    altura_cm,
    peso_kg,
    historico
]).

consulta_campos([
    id_consulta,
    id_paciente,
    data,
    idade,
    medicao_sistolica_mmHg,
    medicao_diastolica_mmHg,
    frequencia_cardiaca_bpm,
    estado
]).

has_excecao_paciente(ID) :- excecao(paciente(ID, _, _, _, _, _, _, _)).
has_excecao_consulta(ID) :- excecao(consulta(ID, _, _, _, _, _, _, _)).

decide_valor(Val, ID, paciente, Res) :-
        ( Val == interdita -> Res = incerto
        ; ( var(Val) ; Val == nulo_val ; Val == nulo_diag ; Val == nulo_historico )
            -> ( has_excecao_paciente(ID) -> Res = incerto ; Res = desconhecido )
        ; Res = Val ).

decide_valor(Val, ID, consulta, Res) :-
    ( var(Val) ; Val == nulo_val ; Val == nulo_diag )
    -> ( has_excecao_consulta(ID) -> Res = incerto ; Res = desconhecido )
    ; Res = Val.

% ------------------------------------------------------------------------
% Protecao da tabela tensao_arterial e utilitarios para conhecimento negativo
% ------------------------------------------------------------------------

+tensao_arterial(_, _, _, _, _, _) ::
    ( write('Erro: tabela tensao_arterial e imutavel. Insercao rejeitada.'), nl, fail ).

declara_negativo_tensao(ID, Class, SisInf, SisSup, DisInf, DisSup) :-
    \+ tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup),
    assertz(tensao_arterial_negativa(ID, Class, SisInf, SisSup, DisInf, DisSup)),
    assertz(-(tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup))).

remove_negativo_tensao(ID, Class, SisInf, SisSup, DisInf, DisSup) :-
    retract(tensao_arterial_negativa(ID, Class, SisInf, SisSup, DisInf, DisSup)),
    retract(-(tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup))).

% ------------------------------------------------------------------------
% Conhecimento negativo para paciente/consulta
% ------------------------------------------------------------------------

-(paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico)) :-
    \+ paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico),
    assertz(-(paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico))).

% Remove negação explícita para paciente
remove_negativo_paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico) :-
    retract(-(paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico))).


-(consulta(IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado)) :-
    \+ consulta(IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado),
    assertz(-(consulta(IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado))).

% Remove negação explícita para consulta
remove_negativo_consulta(IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado) :-
    retract(-(consulta(IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado))).

% ------------------------------------------------------------------------
% Utilitários para inspecionar e remover conhecimento negativo de consultas
% ------------------------------------------------------------------------
listar_negativos_consultas :-
    findall((IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado),
            clause(-(consulta(IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado)), _),
            L),
    ( L == [] -> write('Nenhum conhecimento negativo de consultas.'), nl
    ; write('Consultas negativas:'), nl, print_neg_cons(L) ).

print_neg_cons([]).
print_neg_cons([(A,B,C,D,E,F,G,H)|T]) :-
    format(' - ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w~n', [A,B,C,D,E,F,G,H]),
    print_neg_cons(T).


remove_negativo_consulta_id(IDc) :-
    ( clause(-(consulta(IDc, _, _, _, _, _, _, _)), _) ->
        retractall(-(consulta(IDc, _, _, _, _, _, _, _))),
        format('Removidos todos os negativos para consulta ID ~w.~n', [IDc])
    ; format('Nenhum negativo encontrado para consulta ID ~w.~n', [IDc]) ).

:- initialization(catch(retractall(-(consulta(1011, _, _, _, _, _, _, _))), _, true)).


-(tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup)) :-
    \+ tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup),
    assertz(tensao_arterial_negativa(ID, Class, SisInf, SisSup, DisInf, DisSup)),
    assertz(-(tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup))).

remove_negativo_tensao(ID, Class, SisInf, SisSup, DisInf, DisSup) :-
    retract(tensao_arterial_negativa(ID, Class, SisInf, SisSup, DisInf, DisSup)),
    retract(-(tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup))).

marcar_morada_interdita(ID) :-
    paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico),
    ( Morada == interdita ->
        true
    ;
        retract(paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico)),
        assertz(paciente(ID, Nome, DataNasc, Sexo, interdita, Altura, Peso, Historico)),
        assertz(excecao(paciente(ID, Nome, DataNasc, Sexo, interdita, Altura, Peso, Historico)))
    ),
    ( interdito(morada(ID)) -> true ; assertz(interdito(morada(ID))) ).

morada_interdita(ID) :- interdito(morada(ID)).

% ------------------------------------------------------------------------
% Invariantes para evitar contradição entre conhecimento positivo e negativo
% ------------------------------------------------------------------------
+paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico) ::
    ( \+ clause(-(paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico)), _) ).

+consulta(IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado) ::
    ( \+ clause(-(consulta(IDc, ID_Pac, Data, Idade, Sist, Diast, FC, Estado)), _) ).


% --- Implementações auxiliares para obter campos de paciente/consulta ---

get_paciente_field(ID, id_paciente, ID) :-
    paciente(ID, _, _, _, _, _, _, _).
get_paciente_field(ID, nome, Nome) :-
    paciente(ID, Nome, _, _, _, _, _, _).
get_paciente_field(ID, data_nascimento, DataNasc) :-
    paciente(ID, _, DataNasc, _, _, _, _, _).
get_paciente_field(ID, sexo, Sexo) :-
    paciente(ID, _, _, Sexo, _, _, _, _).
get_paciente_field(ID, morada, Morada) :-
    paciente(ID, _, _, _, Morada, _, _, _).
get_paciente_field(ID, altura_cm, Altura) :-
    paciente(ID, _, _, _, _, Altura, _, _).
get_paciente_field(ID, peso_kg, Peso) :-
    paciente(ID, _, _, _, _, _, Peso, _).
get_paciente_field(ID, historico, Historico) :-
    paciente(ID, _, _, _, _, _, _, Historico).

get_consulta_field(ID, id_consulta, ID) :-
    consulta(ID, _, _, _, _, _, _, _).
get_consulta_field(ID, id_paciente, ID_P) :-
    consulta(ID, ID_P, _, _, _, _, _, _).
get_consulta_field(ID, data, Data) :-
    consulta(ID, _, Data, _, _, _, _, _).
get_consulta_field(ID, idade, Idade) :-
    consulta(ID, _, _, Idade, _, _, _, _).
get_consulta_field(ID, medicao_sistolica_mmHg, Sist) :-
    consulta(ID, _, _, _, Sist, _, _, _).
get_consulta_field(ID, medicao_diastolica_mmHg, Dis) :-
    consulta(ID, _, _, _, _, Dis, _, _).
get_consulta_field(ID, frequencia_cardiaca_bpm, FC) :-
    consulta(ID, _, _, _, _, _, FC, _).
get_consulta_field(ID, estado, Estado) :-
    consulta(ID, _, _, _, _, _, _, Estado).

% ------------------------------------------------------------------------
% Pesquisar paciente/consulta por ID e campo
% ------------------------------------------------------------------------

pesquisar(paciente, ID, all, Res) :-
    ( paciente(ID, _, _, _, _, _, _, _) ->
        paciente_campos(Fields),
        findall(C-V,
                ( pertence(C, Fields),
                  get_paciente_field(ID, C, Val0),
                  decide_valor(Val0, ID, paciente, V)
                ),
                Res)
    ; ( has_excecao_paciente(ID) -> Res = incerto ; Res = desconhecido )
    ).

pesquisar(consulta, ID, all, Res) :-
    ( consulta(ID, _, _, _, _, _, _, _) ->
        consulta_campos(Fields),
        findall(C-V,
                ( pertence(C, Fields),
                  get_consulta_field(ID, C, Val0),
                  decide_valor(Val0, ID, consulta, V)
                ),
                Res)
    ; ( has_excecao_consulta(ID) -> Res = incerto ; Res = desconhecido )
    ).

pesquisar(Donde, _, _, desconhecido) :- \+ (Donde == paciente ; Donde == consulta).

% ========================================================================
% RELATÓRIO DE PACIENTE
% ========================================================================

relatorio_paciente(ID_Pac, ID, Nome, Sexo, UltimoDiagnostico) :-
    paciente(ID_Pac, Nome, _, Sexo, _, _, _, _),
    ID = ID_Pac,
    
    (   consulta_mais_recente_paciente(_, ID_Pac, _Data, _, Sis, Dis, _, realizada)
    ->  
        (   classificar_por_tensao(Sis, Dis, UltimoDiagnostico)
        ->  true
        ;   UltimoDiagnostico = indefinido
        )
    ;   
        UltimoDiagnostico = sem_diagnostico
    ).

listar_relatorios_pacientes :-
    findall(ID, paciente(ID, _, _, _, _, _, _, _), IDs),
    (   IDs = []
    ->  write('Nenhum paciente registado')
    ;   write('========== RELATÓRIOS DE TODOS OS PACIENTES =========='), nl,
        listar_relatorios_aux(IDs)
    ).

listar_relatorios_aux([]).
listar_relatorios_aux([ID|T]) :-
    relatorio_paciente(ID, _, Nome, Sexo, Diag),
    format('ID: ~w | Nome: ~w | Sexo: ~w | Diagnóstico: ~w~n', [ID, Nome, Sexo, Diag]),
    listar_relatorios_aux(T).