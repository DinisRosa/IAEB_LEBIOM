# Explicação das Invariantes do predicado `paciente/8`

Ficheiro de origem: `TRABALHO 1/code/Trabalho todo 2 copy.pl`

Este documento descreve as invariantes (restrições) definidas para o predicado `paciente/8` na base de conhecimento. A finalidade é explicar, em português, o que cada invariante garante e como afeta operações de inserção e remoção.

---

## 1) Invariante estrutural — inserção

Código (trecho):

```prolog
% INVARIANTE ESTRUTURAL (+paciente/8): Não permitir a inserção de pacientes duplicados (baseado no ID).
+paciente(_, Nome, DataNasc, _, _, Altura, Peso, _) :: (
    Nome \= '',
    DataNasc \= nulo,
    Altura \= nulo,
    Peso \= nulo
).
```

Explicação:
- Tipo: invariante de inserção (começa com `+`).
- O objetivo é garantir que, ao inserir um novo `paciente`, campos essenciais não estejam vazios ou marcados como `nulo`.
- Validações aplicadas: `Nome` não pode ser uma string vazia; `DataNasc`, `Altura` e `Peso` não podem ser `nulo`.
- Efeito prático: impede inserções incompletas / inválidas que deixariam o registo sem os campos mínimos necessários.

Observação: o comentário original menciona "duplicados (baseado no ID)", mas a condição actual valida apenas campos obrigatórios; a verificação de duplicados pelo ID é feita noutro lugar (por exemplo, quando se calcula `max_id` e se usa `evolucao/1`).

---

## 2) Invariante referencial — remoção

Código (trecho):

```prolog
% INVARIANTE REFERENCIAL (-paciente/8): Não permitir remover paciente se tiver consultas registadas.
-paciente(ID, _, _, _, _, _, _, _) :: (
    findall(C_ID, consulta(C_ID, ID, _, _, _, _, _, _), S_Consultas),
    comprimento(S_Consultas, N),
    N == 0
).
```

Explicação:
- Tipo: invariante de remoção (começa com `-`).
- O objetivo é garantir integridade referencial entre `paciente` e `consulta`.
- A condição recolhe todas as consultas associadas ao `ID` do paciente e exige que o número de consultas (`N`) seja igual a 0 para permitir a remoção.
- Efeito prático: evita apagar um paciente que ainda tem consultas gravadas; obriga a remover (ou limpar) primeiro as consultas associadas.

---

## 3) Invariante estrutural — idade máxima (inserção)

Código (trecho):

```prolog
% INVARIANTE ESTRUTURAL (+paciente/8): Nao permitir que um paciente tenha idade superior a 150 anos
+paciente(_, _, date(_, _, AnoNasc), _, _, _, _, _) :: (
    AnoAtual = 2025,
    Idade is AnoAtual - AnoNasc,
    idade_valida(Idade)
).
```

Explicação:
- Tipo: invariante de inserção.
- O objetivo é validar a data de nascimento de forma a que a idade resultante seja plausível.
- Usa `idade_valida/1`, definida no ficheiro, que verifica `0 =< Idade =< 150`.
- Efeito prático: impede inserir pacientes com data de nascimento que resulte numa idade fora do intervalo aceitável.

Observação: `AnoAtual` está fixado em 2025 no código — se o projecto for usado em anos diferentes, convém parametrizar isto (ou obter o ano corrente dinamicamente), caso contrário a checagem pode tornar-se obsoleta.

---

## Observações finais e uso

- As invariantes são verificadas pelo mecanismo `evolucao/1` e `involucao/1` presentes no ficheiro. Ao tentar inserir `evolucao(paciente(...))`, o sistema recolhe todas as invariantes `+paciente(...) :: Cond` e testa `Cond` antes de inserir.
- Para remover um paciente o sistema usa `involucao(paciente(...))` que testa as invariantes `-paciente(...) :: Cond`.
- Se quiser que eu:
  - adicione comentários uniformes (mais detalhados) directamente acima de cada invariante no ficheiro fonte (`Trabalho todo 2 copy.pl`), faço isso;
  - crie testes Prolog mínimos (ex.: tentar inserir pacientes válidos/inválidos e mostrar resultados), também posso implementar um pequeno ficheiro `testes_paciente.pl` para validar as invariantes automaticamente.

---

Ficheiro gerado automaticamente: `docs/explicaçãoInvariantesPaciente.md`
