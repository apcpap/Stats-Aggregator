{application, stats_aggregator,
 [{description, "Event Stats Aggregator"},
  {vsn, "0.1.0"},
  {modules, [
             sa_app,
             sa_sup
            ]},
  {registered, [sa_sup]},
  {applications, [kernel, stdlib]},
  {mod, {sa_app, []}}
]}.