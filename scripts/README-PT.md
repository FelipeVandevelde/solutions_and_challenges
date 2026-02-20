# Scripts

| Script | Código | Explicação |
| --- | --- | --- | 
| `update_readmes.py` | [Ver código](update_readmes.py) | [Ver explicação](#update_readmespy) |

---

## `update_readmes.py`

Atualiza automaticamente os arquivos README dentro de `./src`, recriando tabelas, contadores e blocos de código das soluções.

### O que ele atualiza

- **READMEs de problemas** (`README.md` e `README-PT.md` dentro de cada pasta de problema)
  - Substitui tudo após a linha marcador `<!-- solutions -->` por um Markdown gerado que incorpora o conteúdo de cada arquivo da pasta `solutions/`.
- **READMEs de categoria** (`README.md` e `README-PT.md` dentro de cada pasta de categoria)
  - Atualiza o contador do cabeçalho H1 que bate com `# <título> (N/M)`, substituindo **N** pela quantidade de diretórios de problemas encontrados.
  - Substitui o corpo da tabela após a linha separadora `|---|---|---|` por uma lista gerada de problemas e links.
- **READMEs da plataforma** (`README.md` e `README-PT.md` dentro de cada pasta de plataforma)
  - Atualiza a linha de cada categoria com a nova contagem documentada e a porcentagem.
  - Recalcula a linha **Total** somando todas as categorias.

### Estrutura esperada do repositório

O script assume este layout:

- `src/<plataforma>/<categoria>/<problema>/`
  - `README.md`
  - `README-PT.md`
  - `solutions/` (diretório contendo um ou mais arquivos de solução)

Os diretórios de problemas devem ter um nome no formato:

- `<id> - <nome do problema>` (exemplo: `1000 - Hello World!`)

### Marcadores obrigatórios nos READMEs

- **Nos READMEs de problemas**: a linha `<!-- solutions -->` deve existir.
- **Nos READMEs de categoria**: a linha separadora de tabela `|---|---|---|` deve existir.
- **Nos READMEs de categoria**: o primeiro cabeçalho H1 deve bater com `# <qualquer coisa> (N/M)` para o script conseguir atualizar o **N**.

### Descrição do gatilho

O script é iniciado automaticamente sempre que as seguintes condições forem atendidas:
- **Alterações no diretório `src`**: Quando houver um novo _commit_ que modifique arquivos dentro da pasta `src`, exceto pelos arquivos `README.md` e `README-PT.md`.
- **Alterações no Script**: Quando houver modificações diretas no próprio arquivo de script.

### Observações

- O script sobrescreve o conteúdo **após** os marcadores descritos acima. Garanta que os marcadores permaneçam no arquivo.
- As listas de diretórios e arquivos são ordenadas para manter a saída gerada determinística.