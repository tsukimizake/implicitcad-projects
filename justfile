set shell := ["nu", "-c"]

default:
  stack run --rts-options "-s"
watch:
  watch --debounce-ms 20000 . --glob=app/*.hs {|| just default } 

newModule name:
  cat app/Template.hs | str replace --all Template {{name}} | save app/{{name}}.hs
