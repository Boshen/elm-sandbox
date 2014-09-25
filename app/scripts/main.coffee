programs =
  dodger: Elm.Dodger
  tracer: Elm.Tracer
  life: Elm.Life
  snake: Elm.Snake
  man_down: Elm.ManDown
  fractal: Elm.Fractal

application = null

for prog, app of programs
  if window.location.search.indexOf(prog) > -1
    application = app

for link in document.querySelectorAll('.link')
  if application?
    link.style.display = 'none'

if application?
  document.body.style.cursor = 'none'
  Elm.fullscreen(application)
