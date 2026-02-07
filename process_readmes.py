import os
import re
from pathlib import Path

# Diretório base
base_dir = Path("src/leetcode")

# Processar todos os README-PT.md
for readme_pt in base_dir.rglob("README-PT.md"):
    # Pular o README-PT.md do diretório raiz
    if readme_pt.parent == base_dir:
        continue
    
    try:
        with open(readme_pt, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Remover o tópico sobre português (linha que começa com ##### Este é o Readme do projeto _em português_)
        content = re.sub(r'^##### Este é o Readme do projeto _em português_, caso prefira acompanhar o Readme em Inglês, \[clique aqui\]\(\./README\.md\)\n', '', content, flags=re.MULTILINE)
        
        # Remover a linha separadora "---" antes do "**Total de problemas:**"
        content = re.sub(r'\n---\n\n\*\*Total de problemas:\*\* \d+\n?$', '', content, flags=re.MULTILINE)
        
        # Remover o parágrafo "**Total de problemas:**" se ainda existir
        content = re.sub(r'\n\*\*Total de problemas:\*\* \d+\n?$', '', content, flags=re.MULTILINE)
        
        # Garantir que termina com a tabela (sem linha em branco extra no final)
        content = content.rstrip() + '\n'
        
        with open(readme_pt, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Processado: {readme_pt}")
    except Exception as e:
        print(f"Erro ao processar {readme_pt}: {e}")

# Processar todos os README.md
for readme in base_dir.rglob("README.md"):
    # Pular o README.md do diretório raiz
    if readme.parent == base_dir:
        continue
    
    try:
        with open(readme, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Verificar se já tem o tópico sobre inglês
        has_topic = re.search(r'^##### Este é o Readme do projeto _em inglês_', content, re.MULTILINE)
        
        # Remover todas as tabelas (pode estar no início da linha ou após quebra de linha)
        # Padrão mais flexível que captura tabelas em qualquer posição, incluindo \r\n
        # Primeiro, normalizar quebras de linha para \n
        content_normalized = content.replace('\r\n', '\n').replace('\r', '\n')
        
        # Padrão para encontrar tabelas (com ou sem quebra de linha antes)
        table_pattern = r'(?:\r?\n)?\| # \| Título \|  Solução \|\s*\r?\n\|---\|---\|\s*\r?\n'
        # Remover todas as ocorrências da tabela
        content_without_tables = re.sub(table_pattern, '', content_normalized)
        
        # Se o conteúdo mudou, significa que havia tabelas
        has_table = content_normalized != content_without_tables
        content = content_without_tables
        
        # Restaurar quebras de linha originais se necessário
        if '\r\n' in content and content.count('\r\n') < content.count('\n'):
            # Se havia \r\n no original, restaurar
            pass  # Manter \n por enquanto, será normalizado ao salvar
        
        # Adicionar tópico se não existir
        if not has_topic:
            # Encontrar o nome da categoria do diretório
            category = readme.parent.name
            # Adicionar no início do arquivo
            topic_line = f"##### Este é o Readme do projeto _em inglês_, caso prefira acompanhar o Readme em Português-BR, [clique aqui](./README-PT.md)\n"
            content = topic_line + content
        
        # Adicionar tabela se não existir
        if not has_table:
            # Adicionar tabela antes do final do arquivo
            # Garantir que há uma quebra de linha antes da tabela
            content = content.rstrip()
            if not content.endswith('\n'):
                content += '\n'
            table = "\n| # | Título |  Solução |\n|---|---|---|\n"
            content = content + table
        else:
            # Se a tabela já existe, garantir que há quebra de linha antes
            # Verificar se a tabela está na mesma linha que o texto anterior
            content = re.sub(r'([^\n])\| # \| Título \|  Solução \|', r'\1\n| # | Título |  Solução |', content)
        
        with open(readme, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Processado: {readme}")
    except Exception as e:
        print(f"Erro ao processar {readme}: {e}")

print("\nProcessamento concluído!")
