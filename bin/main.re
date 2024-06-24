open! Incr_dom;
module App = Reason_ui.App;

Start_app.start(
  (module App),
  ~bind_to_element_with_id="app",
  ~initial_model=App.initial_model,
);
