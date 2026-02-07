# 1000 - Hello World!

## [Descrição](https://judge.beecrowd.com/pt/problems/view/1000)

## Solução

Todo programador conhece o ritual: para começar com o pé direito, precisamos saudar o mundo! No beecrowd, o segredo não é apenas o que você escreve, mas **como** você termina a frase.

**A Estratégia**
* **O Conteúdo:** Imprimir exatamente a frase `Hello World!`.
* **O Detalhe de Ouro:** Não esqueça da quebra de linha (`\n`) ao final. Sem ela, o juiz online pode retornar um erro de "Presentation Error".

### C99

```c
#include <stdio.h>

int main(){
    printf("Hello World!\n");

    return 0;
}
```

### C++20

```cpp
#include <iostream>

using namespace std;

int main(){
    cout << "Hello World!" << endl;

    return 0;
}
```

### C#

```cs
using System;

class URI {
    static void Main(string[] args) {
        Console.WriteLine("Hello World!");
    }
}
```

### Java 19
```java
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        System.out.println("Hello World!");
    }
}
```

### JavaScript 12.18

```javascript
console.log("Hello World!");
```

### Python 3.9
```python
print("Hello World!")
```