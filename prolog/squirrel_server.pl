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

:- module(squirrel_server,
          []).
:- use_module(rclswi_setup).
:- use_module(library(main)).
:- use_module(library(ros)).
:- use_module(library(ros/pkg)).
:- use_module(library(ros/debug)).
:- use_module(library(ros/logging)).
:- use_module(library(ros/types), []).
:- use_module(library(ros/services)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(socket)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug), [debug/3]).

/** <module> Embed squirrel world into a ROS2 node

This module defines a Prolog  process   that  controls  the racket based
squirrel  world  simulation  and  provides    ROS  services  that  allow
(re)starting the simulation,  connecting  to   a  squirrel  and  sending
commands to the squirrel.
*/

:- initialization(main, main).

%!  main(+Argv)
%
%   Start the racket process, create the ROS services and spin.

main(_Argv) :-
    run_sw_server,
    create_services.

:- ros_set_defaults(
       [ node(squirrel_world, [])
       ]).
:- ros_import_type('squirrel_world/srv/Command').

create_services :-
    ros_service('/squirrel/connect',
                'squirrel_world/srv/Connect',
                ConnectSrv, []),
    ros_service('/squirrel/command',
                'squirrel_world/srv/Command',
                CommandSrv, []),
    ros_service('/squirrel/simulation',
                'squirrel_world/srv/Simulation',
                SimulationSrv, []),
    ros_service_spin(SimulationSrv, srv_simulation),
    ros_service_spin(ConnectSrv,    srv_connect),
    ros_service_spin(CommandSrv,    srv_command, [threaded(true)]),
    ros_spin.

:- dynamic
    squirrel/2.

%!  srv_simulation(+Request, -Reply)
%
%   (Re)start the simulation.

srv_simulation(_{restart:false,seed:_},
               _{seed:Seed}) :-
    sw_server(seed, Seed),
    !.
srv_simulation(_{restart:_,seed:ReqSeed},
               _{seed:Seed}) :-
    stop_server,
    run_sw_server(ReqSeed),
    wait_server_start(Seed).

%!  srv_connect(+Request, -Reply)
%
%   Connect to a squirrel and return its  name. Further commands use the
%   name of the squirrel to talk to the right squirrel.

srv_connect(_Request, _{squirrel:Squirrel}) :-
    connect(Socket, Squirrel),
    asserta(squirrel(Squirrel, Socket)).

%!  srv_command(+Request, -Reply)
%
%   Handle a command to a named squirrel.   Note  that the racket server
%   responds in a time that relates  to   the  cost  of the action. This
%   service is registered using `[threaded(true)]`, causing each request
%   to be handled in a temporary thread. This ensures multiple squirrels
%   can process commands concurrently.

srv_command(Request, Response) :-
    atom_string(Squirrel, Request.squirrel),
    atom_string(Command, Request.command),
    ros_log(info, 'Command ~p for squirrel ~p', [Command, Squirrel]),
    squirrel(Squirrel, Socket),
    command(Socket, Command, Reply),
    parse_reply(Command, Reply, Response).

parse_reply(_, end_of_file, _{error:[end_of_file]}) :- !.
parse_reply(_, "ok",        _{status:[true]}) :- !.
parse_reply(_, "fail",      _{status:[false]}) :- !.
parse_reply(_, "nothing",   _{seeing:[Nothing]}) :-
    !,
    ros_constant(_, 'SEE_NOTHING', Nothing).
parse_reply(_, "wall",      _{seeing:[Wall]}) :-
    !,
    ros_constant(_, 'SEE_WALL', Wall).
parse_reply(_, Integer,     _{energy:[Percentage]}) :-
    number_string(Percentage, Integer),
    !.
parse_reply(smell, SExp,    _{smelling:Numbers}) :-
    !,
    sexp_numbers(SExp, Numbers).
parse_reply(listen, SExp,   _{hearing:Numbers}) :-
    !,
    sexp_numbers(SExp, Numbers).

sexp_numbers(String, Numbers) :-
    string_codes(String, Codes),
    phrase(sexp_numbers(Numbers), Codes).

sexp_numbers(Numbers) -->
    whites, "(", whites,
    numbers(Numbers),
    whites, ")", whites.

numbers([H|T]) --> integer(H), !, whites, numbers(T).
numbers([]) --> [].


:- dynamic
    sw_server/2.

%!  command(+Socket, +String, -Reply)
%
%   Send a command and read its reply.  Commands:
%
%      | Command | Cost | Reply              |
%      |---------|------|--------------------|
%      | left    |    1 | ok                 |
%      | right   |    1 | ok                 |
%      | forward |    1 | ok/fail            |
%      | pick    |   30 | ok/fail            |
%      | drop    |   30 | ok/fail            |
%      | eat     |   60 | ok/fail            |
%      | build   |   50 | ok/fail            |
%      | feel    |    1 | 0..100             |
%      | look    |   10 | wall/nothing       |
%      | smell   |    4 | (acorns squirrels) |
%      | listen  |   40 | (lr, fb)           |
%
%   Listen is in the direction the squirrel   looks. lr is possitive for
%   right, negative for left and fb is  possitive for front and negative
%   for back.

command(Socket, String, Reply) :-
    format(Socket, '~w~n', [String]),
    flush_output(Socket),
    read_line_to_string(Socket, Reply).

%!  connect(-Socket, -Squirrel) is det.

connect(Socket, Squirrel) :-
    between(1, 5, Retry),
      catch(tcp_connect('127.0.0.1':8123, Socket, []), Error, true),
      (   var(Error)
      ->  read_line_to_string(Socket, Reply),
          atom_string(Squirrel, Reply)
      ;   Error = error(socket_error(econnrefused,_), _)
      ->  Wait is 0.1 * (1<<Retry),
          sleep(Wait),
          fail
      ;   throw(Error)
      ).

%!  run_sw_server is det.
%!  run_sw_server(+Options) is det.
%
%   Start a racket process tha runs the squirrel world.

run_sw_server :-
    current_prolog_flag(argv, Argv),
    argv_options(Argv, Options, _),
    run_sw_server(Options).

run_sw_server(0) :-
    !,
    run_sw_server([]).
run_sw_server(Seed) :-
    integer(Seed),
    !,
    run_sw_server([seed(Seed)]).
run_sw_server(Options) :-
    (   option(seed(Seed), Options)
    ->  Extra = [Seed]
    ;   Extra = []
    ),
    sw_server(Server),
    process_create(path(racket), ['-fm', Server|Extra],
                   [ process(PID),
                     stdout(pipe(Stdout))
                   ]),
    debug(squirrel(server), 'Created process ~p', [PID]),
    asserta(sw_server(pid, PID)),
    thread_create(watch_sw_server(PID, Stdout), _, [detached(true)]).

sw_server(Source) :-
    ros_package_share_directory(squirrel_world, Dir),
    directory_file_path(Dir, 'Squirrels/sw-server.scm', Source),
    (   exists_file(Source)
    ->  true
    ;   existence_error(file, Source)
    ).


%!  watch_sw_server(+PID, +Stdout)
%
%   Watch the output of the child process.   Requires this extra line in
%   `sw-server.scm` at the start of ``(main)``:
%
%      (file-stream-buffer-mode (current-output-port) 'line)

watch_sw_server(PID, Stdout) :-
    ros_publisher('/squirrel/event',
                  [ message_type('squirrel_world/msg/Event')
                  ]),
    watch_sw_server_loop(PID, Stdout).

watch_sw_server_loop(PID, Stdout) :-
    catch(read_line_to_string(Stdout, Line), Error, true),
    (   var(Error)
    ->  (   Line == end_of_file
        ->  format(user_error, 'SW Server: closed~n', []),
            cleanup_server(PID)
        ;   (   server_reply(Line, Message)
            ->  ros_publish('/squirrel/event', Message)
            ;   format(user_error, 'SW Server: ~p~n', [Line])
            ),
            watch_sw_server_loop(PID, Stdout)
        )
    ;   format(user_error, 'SW Server: I/O error~n', []),
        cleanup_server(PID)
    ).

server_reply(Line, Message) :-
    string_codes(Line, Codes),
    phrase(server_reply(Message), Codes).

server_reply(_{port:[Port]}) -->
    "Listening for connections on port ", integer(Port),
    !,
    { asserta(sw_server(port, Port)) }.
server_reply(_{seed:[Seed]}) -->
    "Seed for this run was ", integer(Seed),
    !,
    { asserta(sw_server(seed, Seed)) }.
server_reply(_{squirrel:[Squirrel], event:[Action]}) -->
    leadin, "Squirrel ", nonblanks(Codes), " ", nonblanks(ACodes),
    !,
    { atom_codes(Squirrel, Codes),
      atom_codes(Action, ACodes),
      format(user_error, "Squirrel ~p ~p~n", [Squirrel, Action])
    }.
server_reply(_{status:[closed]}) -->
    "Network connection going down".

leadin --> "*", !, leadin.
leadin --> white, !, leadin.
leadin --> "".


cleanup_server(PID) :-
    process_wait(PID, _Status),
    retractall(sw_server(_, _)).

%!  stop_server
%
%   Stop a running server and wait  for   it  to  die. The racket server
%   occasionally seems to ignore signals, so we retry until it dies.

stop_server :-
    sw_server(pid, PID),
    !,
    between(1, 5, _),
      (   catch(process_kill(PID), _, fail)
      ->  debug(squirrel(server), 'Killed ~p.  Waiting ...', [PID]),
          thread_wait(\+ sw_server(pid, _),
                      [ wait_preds([ sw_server/2 ]),
                        timeout(5)
                      ])
      ;   debug(squirrel(server), 'Process ~p seems gone', [PID]),
          retractall(sw_server(_, _))
      ),
    !,
    debug(squirrel(server), 'Server is dead', []).
stop_server.

%!  wait_server_start
%
%   Wait for a server to be started.

wait_server_start(Seed) :-
    debug(squirrel(server), 'Waiting for server to become online', []),
    thread_wait(sw_server(seed, Seed),
                [ wait_preds([ sw_server/2 ]),
                  timeout(20)
                ]),
    debug(squirrel(server), 'Online!', []).
