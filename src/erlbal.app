{application, erlbal,
 [{description, "erlbal"},
  {vsn, "0.01"},
  {modules, [
    erlbal,
    erlbal_app,
    erlbal_sup,
    erlbal_web,
    erlbal_deps
  ]},
  {registered, []},
  {mod, {erlbal_app, []}},
  {env, [
        {backends, [
          {balancer, my_balancer, [[{http, "127.0.0.1:80"}]]}
        ]},
        {default_backend, {balancer, my_balancer}}
        ]},
  {applications, [kernel, stdlib, crypto]}]}.
