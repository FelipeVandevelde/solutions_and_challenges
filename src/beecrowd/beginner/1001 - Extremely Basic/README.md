##### Este é o Readme do projeto _em inglês_, caso prefira acompanhar o Readme em Português-BR, [clique aqui](./README-PT.md)
# 1001 - Extremely Basic

## [Description](https://judge.beecrowd.com/en/problems/view/1001)

## Solution

This exercise is like an entry point into programming, challenging the user to read two input values ​​(integers), calculate the sum between them and present the result on the console.

**The Strategy**
* **The Content:** The main objective is to perform the arithmetic operation and obtain the result of the sum to the variable **X**.
* **The Golden Tip:** Pay attention to the output formatting! It is essential to use the letter "X" in **capital letters** and ensure the inclusion of a **space before and after the equals sign** (ex: `X = 30`).
    - Note: Without these formatting details, the Beecrowd may return an error such as "Presentation error",
even if the mathematical calculation is correct.

---

### C99
```c
#include <stdio.h>

int main() {
    int A, B, X;

    scanf("%d", &A);
    scanf("%d", &B);

    X=A+B;

    printf("X = %d\n", X);

    return 0;
}
```

### C++20

```cpp
#include <iostream>

using namespace std;

int main() {
    int A, B, X;

    cin >> A >> B;
    X = A + B;
    cout << "X = " << X << endl;

    return 0;
}
```

### C#

```cs
using System;

class beecrowd {

    static void Main(string[] args) {
        int A = Int32.Parse( System.Console.ReadLine().Trim());
        int B = Int32.Parse( System.Console.ReadLine().Trim());

        int X = A + B;

        Console.Write("X = {0}\n", X);
    }
}
```

### Java 19
```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Main {

    public static void main(String[] args) throws IOException {
        InputStreamReader ir = new InputStreamReader(System.in);
        BufferedReader in = new BufferedReader(ir);

        int A, B, X;

        A = Integer.parseInt(in.readLine());
        B = Integer.parseInt(in.readLine());

        X = A + B;

        System.out.printf("X = %d\n", X);
    }
}
```

### JavaScript 12.18

```javascript
var input = require('fs').readFileSync('/dev/stdin', 'utf8');
var lines = input.split('\n');

var a = parseInt(lines.shift());
var b = parseInt(lines.shift());

console.log('X = ' + (a+b));
```

### Python 3.9
```python
a = int(input())
b = int(input())

x = a + b

print('X =', x)
```

### PHP
```php
<?php
$a = trim(fgets(STDIN));
$b = trim(fgets(STDIN));
$soma = $a + $b;

echo "X = " . $soma . PHP_EOL;
?>
```

#### Dart 3.0
```dart
import 'dart:io';

void main()
{
  int a = int.parse(stdin.readLineSync()!);
  int b = int.parse(stdin.readLineSync()!);

  int soma = a + b;

  print("X = $soma");
}
```

#### Kotlin
```kt
import java.util.*

fun main(args: Array<String>) {
    val sc = Scanner(System.`in`);

    val a: String = sc.next();
    val b: String = sc.next();

    println("X = " + (a.toInt() + b.toInt()));
}
```

#### Ruby 2.7
```rb
a = gets.to_i
b = gets.to_i

x = a + b

puts "X = #{x}"
```

#### Go 1.20
```go
package main

import ("fmt")

func main() {
    var a,b int
    var x int

    fmt.Scanf("%d", &a)
    fmt.Scanf("%d", &b)
    x = a + b

    fmt.Printf("X = %d\n", x)
}
```

#### Pascal
```pp
var
    a : int64;
    b : int64;
    x : int64;

begin
    read(a);
    read(b);
    x := a + b;
    writeln('X = ', x);
end.
```

#### Rust
```rs
use std::io;

fn main() {
    let mut inputA = String::new();
    let mut inputB = String::new();

    io::stdin().read_line(&mut inputA);
    io::stdin().read_line(&mut inputB);

    let a: i32 = inputA.trim().parse().unwrap();
    let b: i32 = inputB.trim().parse().unwrap();

    println!("X = {}", a + b);
}
```

#### Lua 5.4
```lua
X = tonumber(io.read(), 10)
Y = tonumber(io.read(), 10)

print('X = ' .. X + Y)
```

#### TypeScript 5.1
```ts
process.stdin.resume();
process.stdin.setEncoding('utf8');

let input = '';
let lineCount = 0;

process.stdin.on('data', (chunk) => {
    input += chunk;
});

process.stdin.on('end', () => {
  const lines = input.trim().split('\n');
  const [a, b] = lines.map((x) => parseInt(x));

  const sum = a + b;

  console.log(`X = ${sum}`);
});
```

#### R
```r
# Read multiple lines from stdin (must use 'r')
input <- file('stdin', 'r')

a <- as.integer(readLines(input, n=1))
b <- as.integer(readLines(input, n=1))

soma = a + b

# Write to stdout (must use '')
write(paste("X =", soma), '')
```

#### Swift 5.8
```swift
let a = Int(readLine(strippingNewline: true)!)!
let b = Int(readLine(strippingNewline: true)!)!

let soma = a + b
print("X = " + String(soma))
```

#### Clojure
```clj
(defn parse-int [s]
(Integer. (re-find #"\d+" s )))

(def a (read-string (read-line)))
(def b (read-string (read-line)))
(println "X =" (+ a b))
```

#### Haskell
```hs
main :: IO ()
main = do
a <- readLn :: IO Int
b <- readLn :: IO Int
putStrLn $ id ("X = " ++ show(a + b))
```

#### Scala
```scala
object Main {
    def main(args: Array[String]) {
        val A = io.StdIn.readLine().toInt
        val B = io.StdIn.readLine().toInt

        println("X = " + (A + B))
    }
}
```

#### Elixir
```exs
a = IO.gets("") |> String.trim |> String.to_integer
b = IO.gets("") |> String.trim |> String.to_integer
x = a + b
IO.puts "X = #{x}"
```

#### OCaml
```ml
Scanf.scanf "%d %d" (fun a b ->
    Printf.printf "X = %d\n" (a + b)
)
```

#### Julia
```jl
a = parse(Int, readline())
b = parse(Int, readline())

soma = a + b

println("X = $soma")
```