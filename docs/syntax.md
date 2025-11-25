# Scribe Syntax Cheatsheet

## Comments
```
# single line comment
```

## Variables
```
let count be number = 0
let mutable total: int64 = 0
```

## Records
```
record User(id: int64, name: text)
let u = User(id = 1, name = "Ada")
```

## Conditionals
```
if count is greater than 10:
    print("too many")
else:
    print("ok")
```

Alternative syntax with `otherwise`:
```
if count > 10:
    print("too many")
otherwise:
    print("ok")
```

Natural language comparisons:
```
if count is greater than 10:
    ...
elif count is less than 5:
    ...
else:
    ...
```

Comparison operators: `is greater than`, `is less than`, `is at least`, `is at most`, `is equal to`, `is not`

## Loops
```
for i in 0 to 10:
    print(i)

for each name in names:
    print(name)
```

## Functions
```
fn add(a: number, b: number) -> number:
    a + b
```

## Async
```
async fn fetch(url: text) -> Result[Response, NetError]:
    let resp = await http.get(url)
    resp
```

## Blocks and Indentation

Scribe uses Python-style indentation and colons to define blocks. Indentation levels are used to start and end blocks:
- Use `:` (colon) after the statement keyword to start a block
- Indent the next line to indicate the block body
- Dedent to the previous level to end the block

No explicit `end` keyword is needed - indentation controls block scope.
