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
if count is greater than 10 then
    print("too many")
otherwise
    print("ok")
end
```

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
