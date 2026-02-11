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
:- dynamic tensao_arterial_negativa/6.
:- dynamic excecao/1.
:- dynamic interdito/1.
:- dynamic si/2.  % já tens

% Tornar dinâmico o "predicado" de negação unária (-)/1 para representar
% conhecimento negativo do tipo -tensao_arterial(...).
:- dynamic('-'/1).

:- op(900, xfy, '::').
:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
% NOTE: Avoid setting `unknown` globally in the user module because
% it can interfere with the development environment. If you need
% `unknown = fail` use it in a module or set it at runtime in the
% specific session.
% :- set_prolog_flag(unknown, fail).

% ------------------------------------------------------------------------
% Conversao de data para numero (para comparar datas)
% ------------------------------------------------------------------------
date_to_num(date(D,M,Y), N) :- N is Y*10000 + M*100 + D.
% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Dados de Pacientes em Prolog
%
% Estrutura: paciente(ID, Nome, DataNasc, Sexo, Morada, Altura_cm, Peso_kg, Diagnostico)
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

% Pacientes Femininos (f)
% Todas as moradas estão interditas (não usadas / consideradas desconhecidas)
paciente(1, maria, date(20, 5, 1985), f, interdita, 165, 62.5, nulo_val).
paciente(2, ana, date(15, 8, 2001), f, interdita, 158, 55.2, nulo_val).
paciente(3, joana, date(1, 4, 2010), f, interdita, 140, 40.0, nulo_val).
paciente(4, luisa, date(19, 1, 1953), f, interdita, 160, 70.3, nulo_val).
paciente(5, nadia, date(5, 12, 2005), f, interdita, 150, 48.6, nulo_val).

% Pacientes Masculinos (m)
paciente(6, joao, date(3, 11, 1972), m, interdita, 178, 85.0, nulo_val).
paciente(7, jose, date(28, 2, 1960), m, interdita, 172, 92.1, nulo_val).
paciente(8, antonio, date(10, 9, 1995), m, interdita, 185, 75.8, nulo_val).
paciente(9, manuel, date(25, 7, 1988), m, interdita, 175, 79.9, nulo_val).
paciente(10, carlos, date(17, 6, 1977), m, interdita, 180, 88.7, nulo_val).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Estrutura: consulta(ID_Consulta, ID_Paciente, DataConsulta, Idade, Medicao_Sistolica_mmHg, Medicao_Diastolica_mmHg, Frequencia_Cardiaca_bpm, Estado)
% Estado = {realizada, agendada, cancelada}
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

% Consultas Realizadas
consulta(1001, 1, date(12, 3, 2024), nulo_val, 118, 76, 72, realizada).
consulta(1002, 2, date(5, 4, 2024),  nulo_val, 110, 70, 68, realizada).
consulta(1003, 3, date(22, 5, 2024), nulo_val, 102, 66, 85, realizada).
consulta(1004, 4, date(14, 6, 2024), nulo_val, 130, 84, 78, realizada).
consulta(1005, 5, date(2, 7, 2024),  nulo_val, 115, 72, 74, realizada).

% Consultas Agendadas (sem medições)
consulta(1006, 6, date(19, 8, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).
consulta(1007, 7, date(9, 9, 2025),  nulo_val, nulo_val, nulo_val, nulo_val, agendada).
consulta(1008, 8, date(1, 10, 2025), nulo_val, nulo_val, nulo_val, nulo_val, agendada).

% Consultas Canceladas (sem medições)
consulta(1009, 9, date(20, 11, 2024), nulo_val, nulo_val, nulo_val, nulo_val, cancelada).
consulta(1010, 10, date(12, 12, 2024), nulo_val, nulo_val, nulo_val, nulo_val, cancelada).

% -------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Dados de Tensão Arterial
%
% Estrutura: tensao_arterial(Id_ta, Classificacao, Sis_inf, Sis_sup, Dis_inf, Dis_sup)
% -------------------------------- - - - - - - - - - -  -  -  -  -   -

tensao_arterial(2001, otima,                       0, 119,   0,  79).
tensao_arterial(2002, normal,                    120, 129,  80,  84).
tensao_arterial(2003, normal_alta,               130, 139,  85,  89).
tensao_arterial(2004, hipertensao_grau1,         140, 159,  90,  99).
tensao_arterial(2005, hipertensao_grau2,         160, 179, 100, 109).
tensao_arterial(2006, hipertensao_grau3,         180, sem_limite, 110, sem_limite).
tensao_arterial(2007, hipertensao_sistolica_isolada, 140, sem_limite, 0, 89).

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
%     Permitir novas consultas se as anteriores estiverem realizadas ou canceladas
% Corrigido: ligar a variável ID_Paciente no head para a usar no corpo
+consulta(_, ID_Paciente, _Data, _, _, _, _, Estado) :: 
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
% Corrigido: ligar ID e ID_Paciente no head para verificar o estado existente
+consulta(ID, ID_Paciente, _, _, _, _, _, _) ::
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

% xii. Impedir a remoção de consultas realizadas (usa si/2)
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
% (invariante de insercao: deve começar com +)
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

% ---------------------------------------------
% --- Pequenos wrappers e utilitários usando si/2 (alteração mínima) ----
% Verifica se um paciente existe/é verdadeiro/falso/desconhecido
si_paciente(ID, Resultado) :-
    si(paciente(ID, _, _, _, _, _, _, _), Resultado).

% Verifica se uma consulta existe/é verdadeiro/falso/desconhecido
si_consulta(ID, Resultado) :-
    si(consulta(ID, _, _, _, _, _, _, _), Resultado).

% Valida um estado de consulta (agendada, realizada, cancelada) usando si/2
valida_estado_si(Estado, Resultado) :-
    si(pertence(Estado, [agendada, realizada, cancelada]), Resultado).

% Valida medições (sistólica, diastólica, FC) usando si/2
valida_medicoes_si(Sist, Diast, FC, Resultado) :-
    ( si((number(Sist), number(Diast), number(FC), Sist > 0, Diast > 0, FC > 0, Sist > Diast), verdadeiro) -> Resultado = verdadeiro
    ; si((Sist == nulo_val, Diast == nulo_val, FC == nulo_val), verdadeiro) -> Resultado = verdadeiro
    ; Resultado = desconhecido ).

% Verifica se a idade dada corresponde à calculada para a data da consulta
idade_corresponde_si(ID_Pac, date(Dc,Mc,Ac), Idade, Resultado) :-
    ( paciente(ID_Pac, _, date(Dn,Mn,An), _, _, _, _, _) ->
        calcula_idade(date(Dn,Mn,An), date(Dc,Mc,Ac), IdadeReal),
        si(Idade == IdadeReal, Resultado)
    ; Resultado = desconhecido ).

% Wrapper mínimo para verificar existência do paciente usando si/2
paciente_existe_si(ID, Resultado) :-
    si(paciente(ID, _, _, _, _, _, _, _), Resultado).

% Valida remoção: permite remover apenas se a consulta não foi realizada
valida_remocao_si(Estado, verdadeiro) :-
    Estado \= realizada, !.
valida_remocao_si(Estado, falso) :-
    Estado == realizada, !.
valida_remocao_si(_, desconhecido).

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
% FIXED: Test invariants BEFORE insertion to ensure atomicity
% OTIMIZAÇÃO: Com catch para evitar travamentos
evolucao(Termo) :-
    findall(Invar,+Termo::Invar,L),
    teste(L),           % Test FIRST to ensure all invariants pass
    insercao(Termo).    % Then insert ONLY if all invariants passed

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

% Wrapper específico para consultas (usa o mecanismo genérico de evolucao)
evolucao_consulta(Termo) :-
    evolucao(Termo).

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



% Matrix 








:- use_module(library(listing)).

nnatural(X) :- integer(X), X >= 0.

% ------------------------------------------------------------------------------------------------
% UTILIÁRIOS
% ------------------------------------------------------------------------------------------------

% Comprimento de lista [9, 11].
comprimento(S, N) :- length(S, N). 

% Encontra o valor máximo numa lista (versão otimizada com acumulador).
% Renomeado para evitar conflito com library(lists):max_list/2
max_list_local(List, Max) :- max_list_acc(List, 0, Max).

max_list_acc([], Acc, Acc) :- !.
max_list_acc([H|T], Acc, Max) :-
    (H > Acc -> NewAcc = H ; NewAcc = Acc),
    max_list_acc(T, NewAcc, Max).

% Calcula o maior ID existente para atribuição sequencial.
% ATUALIZAÇÃO: Ajuste para paciente/8.
% OTIMIZAÇÃO: Usar findall com max_member se disponível, senão fallback
max_id(MaxID) :-
    findall(ID, paciente(ID, _, _, _, _, _, _, _), ListaIDs),
    (ListaIDs = [] -> MaxID = 0 
    ; catch(max_member(MaxID, ListaIDs), _, max_list_local(ListaIDs, MaxID))).

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
% OTIMIZAÇÃO: Validação de entrada antes de max_id para evitar processamento desnecessário
novo_paciente(Nome, DataNasc, Sexo, Altura, Peso, Morada, Historico) :-
    % Validações obrigatórias PRIMEIRO (fail rápido)
    (   Nome \== nulo_val, 
        DataNasc \== nulo_val,
        Altura \== nulo_val, 
        Peso \== nulo_val
    -> true 
    ; (write('Erro: Parâmetros obrigatórios inválidos'), nl, fail)
    ),
    
    % Validações adicionais
    (   number(Altura), Altura > 0,
        number(Peso), Peso > 0
    -> true
    ; (write('Erro: Altura e Peso devem ser números positivos'), nl, fail)
    ),
    
    % Apenas agora calcular o ID (operação cara)
    max_id(MaxID),
    NovoID is MaxID + 1,
    
    format('A tentar inserir paciente com ID sequencial: ~w~n', [NovoID]),
    
    % Chamar evolucao com o novo paciente
    evolucao(paciente(NovoID, Nome, DataNasc, Sexo, Altura, Peso, Morada, Historico)).

% Versão mínima: apenas campos obrigatórios
% CORREÇÃO: Agora com validação de tipo
novo_paciente(Nome, DataNasc, Altura, Peso) :-
    novo_paciente(Nome, DataNasc, desconhecido, Altura, Peso, interdita, nulo_val).





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

% Classifica a tensão arterial usando os limites definidos
% Predicado que escolhe a classificação correta baseada na prioridade
% Verifica as classificações na ordem de prioridade
classificar_por_prioridade([P|_], Sis, Dis, P) :-
    matches_classificacao(Sis, Dis, P), !.
classificar_por_prioridade([_|T], Sis, Dis, Class) :-
    classificar_por_prioridade(T, Sis, Dis, Class).

% Classificação flexível - VERSÃO CORRIGIDA
classificar_por_tensao(Sis, Dis, indefinido) :-
    ( var(Sis) ; var(Dis) ; Sis == nulo_val ; Dis == nulo_val ), !.
classificar_por_tensao(Sis, Dis, indefinido) :-
    \+ number(Sis) ; \+ number(Dis), !.
classificar_por_tensao(Sis, Dis, ClassFinal) :-
    % resto do teu código original
    findall((C, SisInf, SisSup, DisInf, DisSup),
            tensao_arterial(_, C, SisInf, SisSup, DisInf, DisSup),
            ListaClassificacoes),
    (encontrar_indice_valor(Sis, sistolica, ListaClassificacoes, IS) -> true ; IS = 0),
    (encontrar_indice_valor(Dis, diastolica, ListaClassificacoes, ID) -> true ; ID = 0),
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
    ( var(Val) ; Val == nulo_val ; Val == nulo_diag ; Val == nulo_historico ; Val == interdita )
    -> ( has_excecao_paciente(ID) -> Res = incerto ; Res = desconhecido )
    ; Res = Val.

decide_valor(Val, ID, consulta, Res) :-
    ( var(Val) ; Val == nulo_val ; Val == nulo_diag )
    -> ( has_excecao_consulta(ID) -> Res = incerto ; Res = desconhecido )
    ; Res = Val.

% ------------------------------------------------------------------------
% Protecao da tabela tensao_arterial e utilitarios para conhecimento negativo
% ------------------------------------------------------------------------
% Impede a insercao de novos registos em tensao_arterial (invariante de insercao)
+tensao_arterial(_, _, _, _, _, _) ::
    ( write('Erro: tabela tensao_arterial e imutavel. Insercao rejeitada.'), nl, fail ).

% Declara conhecimento negativo para um registo de tensao arterial.
% Usa o operador -/1 declarado acima: por exemplo
%   declara_negativo_tensao(3000, desconhecido, 0, 0, 0, 0).
% Vai afirmar -tensao_arterial(...).
declara_negativo_tensao(ID, Class, SisInf, SisSup, DisInf, DisSup) :-
    \+ tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup),
    assertz(tensao_arterial_negativa(ID, Class, SisInf, SisSup, DisInf, DisSup)).

remove_negativo_tensao(ID, Class, SisInf, SisSup, DisInf, DisSup) :-
    retract(tensao_arterial_negativa(ID, Class, SisInf, SisSup, DisInf, DisSup)).

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
% relatorio_paciente(+ID_Paciente, -ID, -Nome, -Sexo, -UltimoDiagnostico)
% Retorna informações do paciente: ID, nome, sexo e o último diagnóstico
% da última consulta realizada
% ========================================================================

relatorio_paciente(ID_Pac, ID, Nome, Sexo, UltimoDiagnostico) :-
    % Verificar se o paciente existe
    paciente(ID_Pac, Nome, _, Sexo, _, _, _, _),
    ID = ID_Pac,
    
    % Obter a última consulta realizada
    (   consulta_mais_recente_paciente(_, ID_Pac, _Data, _, Sis, Dis, _, realizada)
    ->  % Se existe consulta realizada, classificar a tensão arterial
        (   classificar_por_tensao(Sis, Dis, UltimoDiagnostico)
        ->  true
        ;   UltimoDiagnostico = indefinido
        )
    ;   % Se não existe consulta realizada, retornar 'sem_diagnostico'
        UltimoDiagnostico = sem_diagnostico
    ).

% Versão que lista todos os relatórios de pacientes
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