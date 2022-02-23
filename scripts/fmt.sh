find . -type f -name "*.elm" -not -path "./node_modules/*" | xargs ./scripts/elm-format.js
