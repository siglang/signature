# Sanetaka programming language contributing guide

## Commit

Unless it's a markdown modification, etc. it commits as a unit (directory).

---

for example:

```
/foo/ (modified)
└─ /bar/ (modified)
   ├─ /baz/ (modified)
   │  ├─ foo.txt (modified)
   │  └─ bar.txt
   └─ /qux/
      ├─ foo.txt
      └─ bar.txt
```

```console
$ git add ./foo/bar/baz
$ git commit -m `update`: updated `baz` crate.
```

### Commit message

Commit message should be written in English.

message should be written in the following format:

```md
<type> [add|update|fix|remove|refactor|docs|?]: <subject> (#<issue|pr:number>)
```

file names are wrapped in backticks.

---

for examples:

```md
`add`: added new feature.
`update`: updated `README.md`.
`fix`: fixed bug (#1).
`remove`: removed `foo` function.
`refactor`: refactored `bar` function.
`docs`: updated `README.md`.

...
```
