benchmem
========

A rebar3 plugin to profile memory consumption of your existing benches.
Based on awesome [rebar3_bench](https://github.com/seriyps/rebar3_bench) plugin and compatible with benches written for it.

Requirements
------------

OTP 26+ (uses its call_memory trace feature)

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {project_plugins, [
        {rebar3_benchmem, {git, "https://github.com/Yozhig/rebar3_benchmem.git", {tag, "0.0.1"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 benchmem
    ===> Analyzing applications...
    ===> Compiling rebar3_benchmem
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling kv_battle
    ===> Testing bench_kv:bench_maps()
    ===> Total words allocated: 7

    ===> Testing bench_kv:bench_dict()
    ===> Total words allocated: 7

    ===> Testing bench_kv:bench_gb_tree()
    ===> Total words allocated: 7

    ===> Testing bench_kv:bench_orddict()
    ===> Total words allocated: 7

    ===> Testing bench_kv:bench_keyfind()
    ===> Total words allocated: 4

    ===> Testing bench_kv:bench_proplists()
    ===> Total words allocated: 4

    ===> Testing bench_kv:bench_ets()
    ===> Total words allocated: 9
