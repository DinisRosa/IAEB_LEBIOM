% -*- Prolog -*-

:- use_module(library(lists)).

% Operadores para invariantes
:- op(1150, fx, +).
:- op(1150, fx, -).
:- op(1100, xfx, ::).

:- discontiguous (+)/1.
:- discontiguous (-)/1.
:- discontiguous teste/1.

% Tornar dinâmicos os predicados que podem ser alterados
:- dynamic paciente/8.
:- dynamic consulta/8.
:- dynamic tensao_arterial/6.
:- dynamic excecao/1.
:- dynamic interdito/1.
:- dynamic si/2.  % já tens

:- op(900, xfy, '::').
:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).

% ------------------------------------------------------------------------
% Conversao de data para numero (para comparar datas)
% ------------------------------------------------------------------------
date_to_num(date(D,M,Y), N) :- N is Y*10000 + M*100 + D.
% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Dados de Pacientes em Prolog
%
% Estrutura: paciente(ID_Paciente, Nome, Data_Nascimento, Sexo, Distrito, Altura_cm, Peso_kg, Historico_Diagnostico)
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

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

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Estrutura: consulta(ID_Consulta, ID_Paciente, DataConsulta, Idade, Medicao_Sistolica_mmHg, Medicao_Diastolica_mmHg, Frequencia_Cardiaca_bpm, Estado)
% Estado = {realizada, agendada, cancelada}
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

% Consultas Realizadas
consulta(2001, 1, date(12, 3, 2024), nulo_val, 118, 76, 72, realizada).
consulta(2002, 2, date(5, 4, 2024),  nulo_val, 110, 70, 68, realizada).
consulta(2003, 3, date(22, 5, 2024), nulo_val, 102, 66, 85, realizada).
consulta(2004, 4, date(14, 6, 2024), nulo_val, 130, 84, 78, realizada).
consulta(2005, 5, date(2, 7, 2024),  nulo_val, 115, 72, 74, realizada).

% Consultas Agendadas (sem medições)
consulta(2006, 6, date(19, 8, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).
consulta(2007, 7, date(9, 9, 2025),  nulo_val, nulo_val, nulo_val, nulo_val, agendada).
consulta(2008, 8, date(1, 10, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).

% Consultas Canceladas (sem medições)
consulta(2009, 9, date(20, 11, 2024), nulo_val, nulo_val, nulo_val, nulo_val, cancelada).
consulta(2010, 10, date(12, 12, 2024), nulo_val, nulo_val, nulo_val, nulo_val, cancelada).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Dados de Tensão Arterial
%
% Estrutura: tensao_arterial(Id_ta, Classificacao, Sis_inf, Sis_sup, Dis_inf, Dis_sup)
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

tensao_arterial(3001, otima,                       0, 119,   0,  79).
tensao_arterial(3002, normal,                    120, 129,  80,  84).
tensao_arterial(3003, normal_alta,               130, 139,  85,  89).
tensao_arterial(3004, hipertensao_grau1,         140, 159,  90,  99).
tensao_arterial(3005, hipertensao_grau2,         160, 179, 100, 109).
tensao_arterial(3006, hipertensao_grau3,         180, sem_limite, 110, sem_limite).
tensao_arterial(3007, hipertensao_sistolica_isolada, 140, sem_limite, 0, 89).

% Invariantes para tensao_arterial
% Impede qualquer insercao de novos registos na tabela de tensao_arterial.
+tensao_arterial(Id, Class, SisInf, SisSup, DisInf, DisSup) :: fail.
% Impede qualquer remocao de registos da tabela de tensao_arterial.
-tensao_arterial(Id, Class, SisInf, SisSup, DisInf, DisSup) :: fail.


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

% ii. Nao permitir a insercao de consultas para pacientes inexistentes
+consulta(_, ID_Pac, _, _, _, _, _, _) :: 
    paciente(ID_Pac, _, _, _, _, _, _, _).

% iii. Validar valores de pressão e frequência cardíaca
+consulta(_, _, _, _, Sist, Diast, FC, Estado) :: 
    ((Estado == realizada ->
        number(Sist), number(Diast), number(FC),
        Sist > 0, Diast > 0, FC > 0,
        Sist > Diast;
        Sist == nulo_val, Diast == nulo_val, FC == nulo_val)).

% iv. Validar o estado da consulta
+consulta(_, _, _, _, _, _, _, Estado) ::
    pertence(Estado, [agendada, realizada, cancelada]).

% v. Garantir que a data da consulta é posterior ao nascimento do paciente
+consulta(_, ID_Pac, date(DiaC, MesC, AnoC), _, _, _, _, _) :: 
    (paciente(ID_Pac, _, date(DiaN, MesN, AnoN), _, _, _, _, _),
     compara_datas_nasc(date(DiaC, MesC, AnoC), date(DiaN, MesN, AnoN))).

% vi. Garantir que cada consulta tem um ID único (não repetido)
+consulta(ID, _, _, _, _, _, _, _) ::
    \+ consulta(ID, _, _, _, _, _, _, _).

% vii. Garantir que um paciente só pode ter uma consulta agendada de cada vez
%     Permitir novas consultas se as anteriores estiverem realizadas ou canceladas
+consulta(_, _ID_Pac, _Data, _, _, _, _, _Estado) :: 
    (findall(EstadoExistente,
             (consulta(_, ID_Paciente, _, _, _, _, _, EstadoExistente),
              EstadoExistente == agendada),
             L),
     length(L, N),
     (Estado == cancelada ; Estado == realizada ; N == 0)).

% viii. Impedir que o mesmo paciente tenha duas consultas na mesma data
%    Permitir remarcar se as consultas anteriores estiverem canceladas

+consulta(_, ID_Paciente, Data, _, _, _, _, Estado) ::
    (findall(EstadoExistente,
             (consulta(_, ID_Paciente, Data, _, _, _, _, EstadoExistente),
              pertence(EstadoExistente, [agendada, realizada])),
             L),
     length(L, N),
     (Estado == cancelada ; N == 0)).

% ix. Impedir alterações a consultas realizadas
+consulta(_, _, _, _, _, _, _, Estado) ::
    (
        % Verifica se já existe uma consulta com este ID e paciente
        findall(E, consulta(ID, ID_Paciente, _, _, _, _, _, E), ListaEstados),
        (ListaEstados == []  % Se não existe, inserção é permitida
        ; (ListaEstados = [EstadoAntigo], EstadoAntigo \= realizada))  % Se existe, não permite edição de realizada
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
    (Estado \= realizada).

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
         number(Sist), number(Diast), number(FC),
         Sist > 0, Diast > 0, FC > 0)
        ;
        (pertence(Estado, [agendada, cancelada]),
         Sist == nulo_val, Diast == nulo_val, FC == nulo_val)
    ).

% xv. Garantir que a idade na consulta corresponde à idade real do paciente
+consulta(_, ID_Pac, date(Dc, Mc, Ac), Idade, _, _, _, _) ::
    (
        paciente(ID_Pac, _, date(Dn, Mn, An), _, _, _, _, _),
        calcula_idade(date(Dn, Mn, An), date(Dc, Mc, Ac), IdadeReal),
        Idade == IdadeReal
    ).

% compara_datas_nasc(DataConsulta, DataNasc) -> verdadeiro se DataConsulta > DataNasc
compara_datas_nasc(date(Dc,Mc,Ac), date(Dn,Mn,An)) :-
    ( Ac > An ) ;
    ( Ac == An, Mc > Mn ) ;
    ( Ac == An, Mc == Mn, Dc >= Dn ).

% --------------------------------------
% Predicado: calcula_idade(DataNasc, DataConsulta, Idade)
% Calcula a idade de uma pessoa na data da consulta
% --------------------------------------
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

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence: Elemento,Lista -> {V,F}
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

pertence(X, [X|_]).
pertence(X, [_|T]) :-
    pertence(X, T).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado evolucao_consulta: Termo -> {V,F}
% Permite a insercao controlada de novas consultas, respeitando invariantes
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

% Negação como falha
nao(Questao) :- Questao, !, fail.
nao(_).

% Teste de lista
teste([]).
teste([R|LR]) :- R, teste(LR).

% Evolução básica (uma única definição)
evolucao(Termo) :-
    findall(Invar,+Termo::Invar,L),
    insercao(Termo),
    teste(L).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

% Involucao/remocao
involucao(Termo) :-
    findall(Invar,-Termo::Invar,Lista),
    teste(Lista),
    retract(Termo),
    !.

remocao_consulta(consulta(ID, ID_Pac, Data, Idade, Sist, Diast, FC, Estado)) :-
    % Impede remoção se a consulta foi realizada
    Estado \= realizada,
    % Remove a consulta
    retract(consulta(ID, ID_Pac, Data, Idade, Sist, Diast, FC, Estado)).

idade_valida(Idade) :-
    number(Idade),
    Idade >= 0,
    Idade =< 150.


:- use_module(library(listing)).

nnatural(X) :- integer(X), X >= 0.

% ------------------------------------------------------------------------------------------------
% UTILIÁRIOS
% ------------------------------------------------------------------------------------------------

% Comprimento de lista [9, 11].
comprimento(S, N) :- length(S, N). 

% Encontra o valor máximo numa lista.
max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, MaxT),
    (H > MaxT -> Max = H ; Max = MaxT).

% Calcula o maior ID existente para atribuição sequencial.
% ATUALIZAÇÃO: Ajuste para paciente/8.
max_id(MaxID) :-
    findall(ID, paciente(ID, _, _, _, _, _, _, _), ListaIDs), % Procura IDs na nova estrutura de 8 argumentos.
    (ListaIDs = [] -> MaxID = 0 ; max_list(ListaIDs, MaxID)).

% ------------------------------------------------------------------------------------------------
% INVARIANTES DE CONSISTÊNCIA
% ------------------------------------------------------------------------------------------------

% INVARIANTE ESTRUTURAL (+paciente/8): Não permitir a inserção de pacientes duplicados (baseado no ID).
+paciente(_, Nome, DataNasc, _, _, Altura, Peso, _) :: (
    Nome \= '',
    DataNasc \= nulo,
    Altura \= nulo,
    Peso \= nulo
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
+paciente(_, _, date(_, _, AnoNasc), _, _, _, _, _) :: (
    AnoAtual = 2025,
    Idade is AnoAtual - AnoNasc,
    idade_valida(Idade)
).

% ------------------------------------------------------------------------------------------------
% MECANISMOS DE EVOLUÇÃO E INVOLUÇÃO
% ------------------------------------------------------------------------------------------------

% remove_paciente(+ID)
% Remove o paciente com o ID especificado, se não tiver consultas associadas.
remove_paciente(ID) :-
    paciente(ID, Nome, DataNasc, Sexo, Morada, Altura, Peso, Historico), % Procura paciente pelo ID
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

% ------------------------------------------------------------------------------------------------
% FUNÇÃO DE INSERÇÃO DE NOVO PACIENTE (AGORA COM NOME)
% ------------------------------------------------------------------------------------------------

% novo_paciente aceita agora 7 argumentos de entrada.
% ATUALIZAÇÃO: Assinatura alterada de /6 para /7 (Nome é o primeiro argumento de entrada).
novo_paciente(Nome, DataNasc, Sexo, Altura, Peso, Morada, Historico) :-
    Nome \== nulo_val, DataNasc \== nulo_val,
    Altura \== nulo_val, Peso \== nulo_val,
    max_id(MaxID),
    NovoID is MaxID + 1,
    write('A tentar inserir paciente com ID sequencial: '), write(NovoID), nl,                                                      %<====
    evolucao(paciente(NovoID, Nome, DataNasc, Sexo, Altura, Peso, Morada, Historico)).

% Versão mínima: apenas campos obrigatórios
novo_paciente(Nome, DataNasc, Altura, Peso) :-
    novo_paciente(Nome, DataNasc, desconhecido, Altura, Peso, desconhecida, nulo_val).

% -----------------------------------------------------------------------------
% FUNÇÕES (do naval.pl)
% -----------------------------------------------------------------------------

% ------------------------------------------------------------------------
% Seleciona o maximo de uma lista de consultas
% ------------------------------------------------------------------------

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
    escolhe_max(L, (_, IDc, Date, Idade, Sis, Dis, Freq, Estado)).

% ------------------------------------------------------------------------
% Verifica se um valor esta dentro de um intervalo
% ------------------------------------------------------------------------
in_range(Value, Low, High) :- High == sem_limite, !, Value >= Low.
in_range(Value, Low, High) :- number(Value), Value >= Low, Value =< High.

% ------------------------------------------------------------------------
% Verifica se a tensao arterial bate com uma classificacao
% ------------------------------------------------------------------------
matches_classificacao(Sis, Dis, Class) :-
    tensao_arterial(_, Class, Sis_inf, Sis_sup, Dis_inf, Dis_sup),
    in_range(Sis, Sis_inf, Sis_sup),
    in_range(Dis, Dis_inf, Dis_sup).

prioridade_classificacao([hipertensao_grau3, hipertensao_grau2, hipertensao_grau1,
                          hipertensao_sistolica_isolada, normal_alta, normal, otima]).

escolhe_prioridade([P|_], Lista, P) :- member(P, Lista), !.
escolhe_prioridade([_|T], Lista, C) :- escolhe_prioridade(T, Lista, C).

% Classifica a tensão arterial usando os limites definidos
% Predicado que escolhe a classificação correta baseada na prioridade
% Verifica as classificações na ordem de prioridade
classificar_por_prioridade([P|_], Sis, Dis, P) :-
    matches_classificacao(Sis, Dis, P), !.
classificar_por_prioridade([_|T], Sis, Dis, Class) :-
    classificar_por_prioridade(T, Sis, Dis, Class).

% Classificação flexível - VERSÃO CORRIGIDA
classificar_por_tensao(Sis, Dis, ClassFinal) :-
    % Obter lista de todas as classificações (em ordem)
    findall((C, SisInf, SisSup, DisInf, DisSup),
            tensao_arterial(_, C, SisInf, SisSup, DisInf, DisSup),
            ListaClassificacoes),
    
    % Encontrar índices
    (encontrar_indice_valor(Sis, sistolica, ListaClassificacoes, IS) -> true ; IS = 0),
    (encontrar_indice_valor(Dis, diastolica, ListaClassificacoes, ID) -> true ; ID = 0),
    
    % Decidir classificação final
    (IS =:= ID, IS > 0 ->
        nth1(IS, ListaClassificacoes, (ClassFinal, _, _, _, _))
    ; IS > 0, ID > 0 ->
        MaxI is max(IS, ID),
        MinI is min(IS, ID),
        Diff is MaxI - MinI,
        (Diff =:= 1 ->
            nth1(MaxI, ListaClassificacoes, (ClassFinal, _, _, _, _))
        ; Diff >= 2 ->
            ClassFinal = indefinido
        )
    ;
        ClassFinal = indefinido
    ).

% Encontra o índice de um valor na lista (sistólica ou diastólica)
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
    % Obter a consulta mais recente realizada
    consulta_mais_recente_paciente(_IDc, ID_P, Data, _Idade, Sis, Dis, _Freq, realizada),
    number(Sis), number(Dis),
    paciente(ID_P, Nome, DataNasc, Sexo, Morada, Altura, Peso, OldHist),
    % Tenta classificar a tensão; se não houver classificação, usa 'indefinido'
    (classificar_por_tensao(Sis, Dis, Class) -> true ; Class = indefinido),
    % Atualiza histórico
    ( OldHist == nulo_val -> NewHist = [[Data, Class]]
    ; NewHist = [[Data, Class]|OldHist] ),
    % Remove o registro antigo e insere o novo
    retract(paciente(ID_P, Nome, DataNasc, Sexo, Morada, Altura, Peso, OldHist)),
    assert(paciente(ID_P, Nome, DataNasc, Sexo, Morada, Altura, Peso, NewHist)).

atualizar_todos_pacientes_por_tensao :-
    findall(ID, paciente(ID,_,_,_,_,_,_,_), IDs),
    atualizar_lista_pacientes(IDs),
    true.  % garante que o predicado devolve true no final

atualizar_lista_pacientes([]).
atualizar_lista_pacientes([H|T]) :-
    ( atualizar_diagnostico_paciente_por_tensao(H) -> true ; true ),
    atualizar_lista_pacientes(T).

%-----------------------------------------------------------------------
% Meta-predicado si/2
% ------------------------------------------------------------------------
si(Questao, verdadeiro) :- Questao.
si(Questao, falso) :- -Questao.
si(Questao, desconhecido) :- nao(Questao), nao(-Questao).

% ------------------------------------------------------------------------
% Pesquisar pacientes ou consultas (integrado ao estilo base)
% ------------------------------------------------------------------------
paciente_campos([
    id_paciente,
    nome,
    data_nascimento,
    sexo,
    distrito,
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
    ( var(Val) ; Val == nulo_val )
    -> ( has_excecao_paciente(ID) -> Res = incerto ; Res = desconhecido )
    ; Res = Val.

decide_valor(Val, ID, consulta, Res) :-
    ( var(Val) ; Val == nulo_val )
    -> ( has_excecao_consulta(ID) -> Res = incerto ; Res = desconhecido )
    ; Res = Val.

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