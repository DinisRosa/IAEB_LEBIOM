# Explicação das Invariantes e Proteções para `tensao_arterial`

Ficheiro de origem: `TRABALHO 1/code/Trabalho todo 2 copy.pl`

Este documento descreve as invariantes (restrições) e mecanismos relacionados com a tabela `tensao_arterial` no ficheiro fonte. Inclui as invariantes de inserção/proteção e utilitários para declarar conhecimento negativo.

---

## 1) Invariante de proteção (inserção) — tabela imutável

Código (trecho):

```prolog
% Impossibilidade de insercao de novos registos em tensao_arterial (invariante de insercao)
+tensao_arterial(_, _, _, _, _, _) ::
    ( write('Erro: tabela tensao_arterial e imutavel. Insercao rejeitada.'), nl, fail ).
```

Explicação:
- Tipo: invariante de inserção (`+`).
- O objetivo é proteger a tabela `tensao_arterial` contra inserções: a tabela é considerada estática/imutável no contexto da aplicação.
- A condicional executa um `write/1` para informar o utilizador e, em seguida, falha (`fail`), impedindo a assert de qualquer novo fact.
- Efeito prático: qualquer tentativa de `evolucao(tensao_arterial(...))` será rejeitada com a mensagem de erro; a tabela só deve ser alterada manualmente no código fonte.

---

## 2) Predicados auxiliares para conhecimento negativo

Apesar de a tabela ser protegida contra inserções, o ficheiro também disponibiliza um mecanismo para declarar conhecimento negativo (registos que se sabe não existirem) sem alterar a tabela original.

Código (trecho):

```prolog
% Declara conhecimento negativo para um registo de tensao arterial.
% Usa o operador -/1 declarado acima: por exemplo
%   declara_negativo_tensao(3000, desconhecido, 0, 0, 0, 0).
% Vai afirmar -tensao_arterial(...).
declara_negativo_tensao(ID, Class, SisInf, SisSup, DisInf, DisSup) :-
    \+ tensao_arterial(ID, Class, SisInf, SisSup, DisInf, DisSup),
    assertz(tensao_arterial_negativa(ID, Class, SisInf, SisSup, DisInf, DisSup)).

remove_negativo_tensao(ID, Class, SisInf, SisSup, DisInf, DisSup) :-
    retract(tensao_arterial_negativa(ID, Class, SisInf, SisSup, DisInf, DisSup)).
```

Explicação:
- `declarar_negativo_tensao/6` verifica primeiro que o registo não existe positivamente na base (`\+ tensao_arterial(...)`) e depois afirma um registo na tabela de negativos `tensao_arterial_negativa/6`.
- `remove_negativo_tensao/6` reverte essa afirmação removendo o facto `tensao_arterial_negativa(...)`.
- Repare que `tensao_arterial_negativa/6` foi declarada dinâmica no início do ficheiro, permitindo assert/retract.
- Este padrão permite representar conhecimento negativo ("sabemos que não existe este registo") sem alterar a tabela fonte de valores corretos.

---

## 3) Declarações relevantes no ficheiro fonte

Para que o mecanismo funcione, existem algumas declarações espalhadas pelo ficheiro:

- `:- dynamic tensao_arterial_negativa/6.` — torna a tabela de negativos dinâmicas para permitir modificações em tempo de execução.
- `:- dynamic('-'/1).` e `:- op(1150, fx, -).` — permitem usar o operador unário `-` para representar conhecimento negativo no estilo do projeto.

Estes itens não são invariantes por si só, mas são necessários ao funcionamento do sistema de conhecimento negativo e à proteção da tabela.

---

## Observações e recomendações

- A proteção da tabela (`+tensao_arterial(...) :: ... write(...), fail`) é uma estratégia simples e eficaz quando a tabela representa constantes definidas no código (como limites de classificação) que não devem ser alteradas em tempo de execução.
- Se, no futuro, precisar de atualizar essa tabela dinamicamente (por exemplo, carregar limites personalizados), recomendo:
  - remover/ajustar a invariante de proteção;
  - ou fornecer uma API segura para atualizações que faça validações antes de permitir alterações.
- Ao declarar conhecimento negativo com `declarar_negativo_tensao/6`, é boa prática garantir que as consultas/razonamentos que leem a tabela considerem tanto `tensao_arterial/6` quanto `tensao_arterial_negativa/6` para evitar conclusões contraditórias.

---

## Próximos passos opcionais

- Posso acrescentar este ficheiro como documentação no repositório (já criado em `code/docs/explicaçãoInvariantesTensaoArterial.md`).
- Posso também inserir comentários explicativos diretamente no ficheiro fonte (`Trabalho todo 2 copy.pl`) imediatamente acima da invariante e das funções auxiliares.
- Se desejar, crio testes Prolog que tentem inserir `tensao_arterial(...)` (devendo falhar) e declarar/retirar negativos para verificar comportamento.

---

Ficheiro gerado automaticamente: `code/docs/explicaçãoInvariantesTensaoArterial.md`
