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

:- module(squirrel_teleop,
          []).
:- use_module(rclswi_setup).
:- use_module(library(ros)).
:- use_module(library(ros/debug), []).
:- use_module(library(util/teleop)).
:- use_module(library(main)).
:- use_module(library(ansi_term)).

:- use_module(squirrel_client).

/** <module> Squirrel teleop

This module implements a  keyboard   controller  for  squirrel_server.pl
based on the module squirrel_client.pl
*/

:- ros_set_defaults(
       [ node(squirrel_teleop, [])
       ]).

:- initialization(main, main).

main(_Argv) :-
    create_client(Squirrel),
    format('Connected to Squirrel ~p~n', [Squirrel]),
    teleop(on_key, Squirrel, _).

on_key(Key, Squirrel, Squirrel) :-
    command(Key, Command),
    !,
    format(user_error, 'Command: ~p --> ', [Command]),
    catch(command(Command, Squirrel, Reply), E, true),
    (   var(E)
    ->  format(user_error, '~p~n', [Reply])
    ;   print_message(warning, E)
    ).
on_key('R', _Squirrel, Squirrel) :-
    !,
    restart(Squirrel).
on_key('h', Squirrel, Squirrel) :-
    !,
    help.
on_key('?', Squirrel, Squirrel) :-
    !,
    help.
on_key(Key, Squirrel, Squirrel) :-
    format(user_error, 'Unknown command: ~p~n', [Key]).

help :-
    format('~N~`\u2015t~*|~n', [50]),
    ansi_format(bold, '~t~w~6|~t~w~15| ~t~w~20| ~w~n',
                ['Key', 'Command', 'Cost', 'Comment']),
    format('~N~`\u2015t~*|~n', [50]),
    (   command(Key, Command),
        command(Command, Cost, _Type, Comment),
        ansi_format(bold, '~t~w~6|~t~w~15| ', [Key, Command]),
        format('~t~w~20| ', [Cost]),
        ansi_format(comment, '~w~n', [Comment]),
        fail
    ;   true
    ),
    format('~N~`\u2015t~*|~n', [50]),
    (   server_command(Key, Comment),
        ansi_format(bold, '~t~w~6| ', [Key]),
        ansi_format(comment, '~w~n', [Comment]),
        fail
    ;   true
    ).


command(left,  left).
command(right, right).
command(up,    forward).
command(p,     pick).
command(d,     drop).
command(e,     eat).
command(b,     build).
command(f,     feel).
command(l,     look).
command(s,     smell).
command(i,     listen).

command(left,    1,  bool,                  "Turn left").
command(right,   1,  bool,                  "Turn right").
command(forward, 1,  bool,                  "Walk forward").
command(pick,    30, bool,                  "Pick one acorn").
command(drop,    30, bool,                  "Drop one acorn").
command(eat,     60, bool,                  "Eat one acorn").
command(build,   50, bool,                  "Build a wall in front of me").
command(feel,    1,  between(0,100),        "Get energy level (0..100)").
command(look,    10, oneof([wall,nothing]), "Look for a wall in front of me").
command(smell,   4,  list,                  "Smell acorns and squirrels here").
command(listen,  40, list,                  "Listen for squirrels nearby").

server_command('R', "Restart the squirrel world").
server_command('h,?', "This message").
