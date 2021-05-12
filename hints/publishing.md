If you've made some changes to the compiler and are ready to publish a new version
of the package, follow the steps below:

```
$ elm bump
# update package.json version...
$ git add elm.json package.json
$ git commit -m ":bookmark: Bump compiler version to <new package version>"
$ git push
$ git tag <new package version>
$ git push --tags origin main
$ elm publish
```

---

## Breakdown

```
$ elm bump
```

* Elm's compiler automatically works out the correct SemVer based on the changes
you've made. You shouldn't ever modify the package version inside the `elm.json`
manually.

* What you _should_ do, however, is manually update the version inside `package.json`
to match whatever `elm bump` returned.

```
$ git add elm.json package.json
$ git commit -m ":bookmark: Bump compiler version to <new package version>"
$ git push
```

* Stage, commit, and push our version changes.

```
$ git tag <new package version>
$ git push --tags origin main
```

* Create a new tag and push it to remote. It's crucial that the tag matches the
version number returned by `elm bump`.

```
$ elm publish
```

* Let the Elm compiler handle the rest!
