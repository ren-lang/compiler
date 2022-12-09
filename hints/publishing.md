# Publishing

If you've made some changes to the compiler and are ready to publish a new version
of the package, follow the steps below:

```bash
npm run switch:lib
elm bump
git add elm.json
git commit -m ":bookmark: Bump compiler version to <new package version>"
git push
git tag <new package version>
git push --tags origin main
elm publish
```

---

## Breakdown

```bash
npm run switch:lib
```

* We have two different elm configurations: one for the compiler package and one
for the CLI. We switch between them using `switch:lib` and `switch:app` npm scripts.

```bash
elm bump
```

* Elm's compiler automatically works out the correct SemVer based on the changes
you've made. You shouldn't ever modify the package version inside the `elm.json`
manually.

```bash
git add elm.json
git commit -m ":bookmark: Bump compiler version to <new package version>"
git push
```

* Stage, commit, and push our version changes.

```bash
git tag <new package version>
git push --tags origin main
```

* Create a new tag and push it to remote. It's crucial that the tag matches the
version number returned by `elm bump`.

```bash
elm publish
```

* Let the Elm compiler handle the rest!
