set shell := ["nu", "-c"]

watch:
   watch . --glob=**/*.hs {|| stack run } 
newModule name:
  cat app/Template.hs | str replace --all Template {{name}} | save app/{{name}}.hs
