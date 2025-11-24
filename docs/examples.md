# Example Programs

## Sum List
```
fn sum_list(values: list of int64) -> int64:
    let mutable s = 0
    for each v in values:
        s = s + v
    s
```

## Async Fetch
```
async fn fetch_post(id: int64) -> Result[Post, NetError]:
    let url = "https://example.com/posts/" + to_text(id)
    let resp = await http.get(url)
    check resp.ensure_status(200)
    json.parse[Post](resp.body)
```

## HTTP Server
```
fn main() -> int32:
    let app = server.new()
    app.get("/health", handle_health)
    app.listen(port = 8080)
    0
```
