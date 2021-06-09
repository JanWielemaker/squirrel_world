/* Copyright 2021 SWI-Prolog Solutions b.v.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http:  www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

:- module(squirrel_client,
          [ create_client/1,            % -Squirrel
            restart/0,
            restart/1,                  % -Squirrel
            command/3                   % +Command, +Squirrel, -Reply
          ]).
:- use_module(library(ros)).
:- use_module(library(ros/services)).
:- use_module(library(ros/types)).

/** <module> ROS client for the squirrel world

This module implements the client  side   for  squirrel_server.pl. It is
used by squirrel_teleop.pl and is  intended  to   be  used  to make your
intelligent squirrel control program talk to the simulation.
*/

:- ros_import_type('squirrel_world/srv/Command').

:- dynamic
    service/2,
    connected/0.

%!  create_client(-Squirrel) is det.
%
%   Setup the connection and return the squirrel we have been assigned.

create_client(Squirrel) :-
    with_mutex(squirrel_client, connect),
    connect(Squirrel).

connect :-
    connected,
    !.
connect :-
    ros_client('/squirrel/connect',
               'squirrel_world/srv/Connect',
               SrvConnect, []),
    ros_client('/squirrel/simulation',
               'squirrel_world/srv/Simulation',
               SrvSimulation, []),
    assertz(service(connect, SrvConnect)),
    assertz(service(simulation, SrvSimulation)),
    ros_subscribe('/squirrel/event', on_event,
                  [ message_type('squirrel_world/msg/Event')
                  ]),
    ros_spin([thread(spinner)]),
    asserta(connected).

on_event(Ev), _{port:[Port]} :< Ev =>
    ansi_format(comment, '~NEvent: Server ready on port ~p~n', [Port]).
on_event(Ev), _{seed:[Seed]} :< Ev =>
    ansi_format(comment, '~NEvent: Server seed is ~p~n', [Seed]).
on_event(Ev), _{status:["closed"]} :< Ev =>
    ansi_format(comment, '~NEvent: Server stopped~n', []).
on_event(Ev), _{squirrel:[Squirrel], event:[Event]} :< Ev =>
    ansi_format(comment, '~NEvent: Squirrel ~p ~p~n', [Squirrel, Event]).
on_event(Ev) =>
    ansi_format(warning, '~NEvent: ~p~n', [Ev]).

%!  connect(-Squirrel)
%
%   Connect to a squirrel, returning its   name. Squirrel is a lowercase
%   atom, e.g., `wally`.

connect(Squirrel) :-
    with_mutex(squirrel_client, connect_sync(Squirrel)).

connect_sync(Squirrel) :-
    debug(squirrel(simulation), 'Connecting ... ', []),
    service(connect, SrvConnect),
    (   ros_call(SrvConnect, _{}, Response, [timeout(20)])
    ->  squirrel_prolog_name(Response.squirrel, Squirrel),
        debug(squirrel(simulation), 'Got squirrel ~p', [Squirrel])
    ;   throw(time_limit_exceeded)
    ).

%!  restart is det.
%!  restart(-Squirrel) is det.
%
%   Restart the simulation and connect to a squirrel

restart :-
    connect,
    service(simulation, SrvSimulation),
    debug(squirrel(simulation), 'Restarting squirrel world ...', []),
    ros_call(SrvSimulation,
             _{restart:true},
             _Reply,
             [timeout(20)]).

restart(Squirrel) :-
    restart,
    connect(Squirrel).

:- thread_local
    cmd_service/1.

%!  command(+Command, +Squirrel, -Reply) is det.
%
%   Send a command for Squirrel. Note   that ros_call/4 is blocking. For
%   this reason we create  a  service   instance  per  thread that calls
%   command/3. As a result, this library   allows  for creating multiple
%   threads that each drive a squirrel to operate fully concurrently.

command(Command, PlSquirrel, Reply) :-
    squirrel_prolog_name(Squirrel, PlSquirrel),
    command_service(SrvCommand),
    (   ros_call(SrvCommand,
                 _{squirrel:Squirrel, command:Command},
                 Reply0,
                 [timeout(20)])
    ->  clean_reply(Command, Reply0, Reply1),
        (   is_dict(Reply1, error)
        ->  throw(Reply1)
        ;   Reply = Reply1
        )
    ;   throw(time_limit_exceeded)
    ).

command_service(Service) :-
    cmd_service(Service),
    !.
command_service(Service) :-
    ros_client('/squirrel/command',
               'squirrel_world/srv/Command',
               Service, []),
    asserta(cmd_service(Service)).

:- dynamic
    name_store/2.

squirrel_prolog_name(Squirrel, PlSquirrel) :-
    nonvar(Squirrel),
    !,
    downcase_atom(Squirrel, PlSquirrel),
    (   name_store(Squirrel, PlSquirrel)
    ->  true
    ;   assertz(name_store(Squirrel, PlSquirrel))
    ).
squirrel_prolog_name(Squirrel, PlSquirrel) :-
    name_store(Squirrel, PlSquirrel),
    !.
squirrel_prolog_name(Squirrel, PlSquirrel) :-
    sub_atom(PlSquirrel, 0, 1, _, C0),
    upcase_atom(C0, U0),
    sub_atom(PlSquirrel, 1, _, 0, Rest),
    atom_concat(U0, Rest, Squirrel).


clean_reply(_, Dict, Error),  Dict.error  = [Value] => Error  = error{msg:Value}.
clean_reply(_, Dict, Bool),   Dict.status = [Value] => Bool   = Value.
clean_reply(_, Dict, Energy), Dict.energy = [Value] => Energy = Value.
clean_reply(_, Dict, Seeing), Dict.seeing = [Value] => seeing(Value, Seeing).
clean_reply(smell, Dict, Smelling), Dict.smelling = [Acorns, Squirrels] =>
    Smelling = smelling{acorns:Acorns, squirrels:Squirrels}.
clean_reply(smell, Dict, Smelling), Dict.smelling == [] =>
    Smelling = smelling{acorns:0, squirrels:0}.
clean_reply(listen, Dict, Hearing),  Dict.hearing = [LR, FB] =>
    Hearing = hearing{lr:LR, fb:FB}.
clean_reply(listen, Dict, Hearing),  Dict.hearing == [] =>
    Hearing = hearing{}.

seeing(Value, Seeing), ros_constant(_, 'SEE_WALL',    Value) => Seeing = wall.
seeing(Value, Seeing), ros_constant(_, 'SEE_NOTHING', Value) => Seeing = nothing.
