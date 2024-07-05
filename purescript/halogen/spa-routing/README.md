Run with:

```sh
# Terminal 1
rg --files | entr -c spago bundle

# Terminal 2
live-server --port=1234 --no-browser --watch=./index.html,./index.js --entry-file=./index.html
```

Run tests with:

```
spago test
```
