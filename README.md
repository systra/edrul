Erlang & jBoss Drools sample integration
========================================

This is sample rating application aiming for testing integration of Erlang with JVM using JInterface and jBoss rules.

This can be use as base for other applications using Erlang <-> JVM communication, thought better (more generic) coding/encoding between Java Objects and Erlang terms would be needed.

JVM node is started by Erlang application (using shell script) and monitored (using process linking), which
enables restarting JVM in case of its crash and stopping it when Erlang application terminates.

The application performs rating of simple event type services, but with the support for balance cascades.
Erlang node passes to JVM a list of parameters (proplist) which can be used in Drools rules to set the rate.

Current implementation supports only Key-Value parameters of types:

*-type rating_parameter() :: {Key :: string() | binary(), Value :: string() | binary() | integer() | float() | atom() | boolean()}.*

Rating function is defined as follows:

*-spec rate([rating_parameter()]) -> {ok, Result :: rating_result()} | {error, Reason :: term()}.*

*-type rating_step() :: {rating_step, Priority::non_neg_integer(), BalanceName::binary(), Units::integer()}.*

*-type rating_result() :: {rating_result, Steps::[rating_step()], RatingGroup::binary()}.*

#### Rating configuration:

Sample rating rules are placed inside *priv/java/conf/rating_rules.drl* file and defines rates for resource usage:

| Resource              | Profile | Cascade                               |
| :--------------------- :-------- :------------------------------------- |
| /messaging/send_sms   | vip     | 1. 1 unit/event from "SMS Package"    |
|                       |         | 2. 10 units/event from "General Cash" |
| /messaging/send_sms   | !vip    | 1. 1 unit/event from "SMS Package"    |
|                       |         | 2. 12 units/event from "General Cash" |
| /messaging/send_mms   |         | 1. 25 unit/event from "General Cash"  |
| /messaging/send_ussd  |         | 1. 10 unit/event from "General Cash"  |
| /messaging/setup_call |         | 1. 5 unit/event from "General Cash"   |
| other                 |         | Free                                  |

Rule engine is currently configured to reload rules every 120 sec or manually using `edrul:reload()` function.

#### Quick start:


To build and run application, you need to have Java, Erlang rebar and make tools installed.

Quick run guide:

        $ make init
        $ make
        $ make run

and then you can invoke sample rating:

    ...
    > edrul:rate([{<<"resource">>,<<"/messaging/send_sms">>},{<<"profile">>,<<"vip">>}]).
    {ok,{rating_result,[{rating_step,1,<<"SMS Package">>,1},
                        {rating_step,0,<<"General Cash">>,10}],
                                           <<"SMS_MO_VIP">>}}
    ...

#### Known issues:

- JMV <-> Erlang communication is ready before Drools rules loading is finished causing timeout of first rating request.

#### Future improvements:

- more configuration options
- add support for rating of time/volume based services
- better checking of JVM node status before starting/stoppping JVM node
- support for stateful Drools sessions

Feel free to contact me if you found this project interesting or found any ideas for future improvements.

#### Credits:

- to Ingo Schramm ([ingo]((https://github.com/nerlo/nerlo.git)) for JInterface reference


[![endorse](http://api.coderwall.com/systra/endorse.png)](http://coderwall.com/systra)
