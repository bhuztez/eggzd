{application, eggzd,
 [{description, "Erlang GGZ Server"},
  {vsn, "0.0.1"},
  {modules, [eggzd_app]},
  {registered, [eggzd_app]},
  {applications, [kernel, stdlib]},
  {mod, {eggzd_app, []}},
  {env,
    [{port, 5688},
     {name, "EGGZD Server"}]}
 ]}.
