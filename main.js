(function() {
  var programs = {
    dodger: Elm.Dodger,
    tracer: Elm.Tracer,
    life: Elm.Life,
    snake: Elm.Snake,
    man_down: Elm.ManDown,
    lsystem: Elm.Lsystem,
    fractal: Elm.Fractal
  };

  var application = null;

  for (var prog in programs) {
    var app = programs[prog];
    if (window.location.search.indexOf(prog) > -1) {
      application = app;
    }
  }

  var ref = document.querySelectorAll('.link');
  for (var i = 0, len = ref.length; i < len; i++) {
    var link = ref[i];
    if (application != null) {
      link.style.display = 'none';
    }
  }

  if (application != null) {
    document.body.style.cursor = 'none';
    Elm.fullscreen(application);
  }
})();
