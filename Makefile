.PHONY: watch
watch:
	elm-live client-src/Main.elm --proxyPrefix=api --proxyHost=http://localhost:8180 -- --debug --output=main.js

watch-lite:
	elm-live client-src/Main.elm --proxyPrefix=api --proxyHost=http://localhost:8180 -- --output=main.js