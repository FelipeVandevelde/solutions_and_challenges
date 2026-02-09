##### Este é o Readme do projeto _em inglês_, caso prefira acompanhar o Readme em Português-BR, [clique aqui](./README-PT.md)
# 1000 - Hello World!

## [Description](https://judge.beecrowd.com/en/problems/view/1000)

## Solution

Every programmer knows the ritual: to start on the right foot, we must greet the world! In beecrowd, the secret isn't just what you write, but **how** you end the sentence.

**The Strategy**
* **The Content:** Print exactly the phrase `Hello World!`.
* **The Golden Tip:** Don't forget the end-of-line character (`\n`) at the end. Without it, the online judge might return a "Presentation Error."

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