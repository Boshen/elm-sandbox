programs =
  space: Elm.Space

application = null

for prog, app of programs
  console.log window.location.search, window.location.search.indexOf(prog) > -1
  if window.location.search.indexOf(prog) > -1
    application = app

for link in document.querySelectorAll('.link')
  if application?
    link.style.display = 'none'

if application?
  Elm.fullscreen(application)
