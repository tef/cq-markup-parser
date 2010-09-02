#!/usr/bin/swipl -q -t start_script -f 

%% introduction (then quickstart and outline of code) 

% code quarterly markup parser

% running the program:
% note: tests are run every time this program is loaded or run.

%%%% running from the shell

% run it as ./filename.pro which reads files from arguments or from stdin.
% or (preferred) load it into a swi prolog interpeter.

%%%% quick start for using swipl

% $ swipl
% ....
% ?-    % this is the prompt.
% ?- consult('filename.pro').

% this tells it to load the file, use it again to reload, which will run tests
% remember: in prolog, 'filename.pro' is an atom, "foo" is a string.

% then call exec("(+ 1 2)",X). 
% the full stop is important. in prolog it terminates expressions.

% see the swiprolog manual for things like firing up your editor
% or debugging commands.

%%%% making a binary
% swipl --goal=start_compile --stand_alone=true -o binary -c filename.pro

%% outline of code:

    % basic runtime skeleton
    % test harness
    % parsing rules
    % transformation
    % init code, auxillary functions
    % testrunner

%% runtime settings
% useful setting, but this breaks norgg's older swi prolog
% :- set_prolog_flag(double_quotes,string).

%% basic runtime skeleton

% we start by defining how to run a string.

% run a given string, with an evironment
% exec(+Input,-Output)
exec(X,O) :- ( 
        parse(X,S);
        write('Syntax Error'),
        nl,
        fail
    ), !, (
        transform(S,O) *-> [] ;
        (write('Runtime Error'),nl,fail)
    ).

% note: A *-> B; Cis the soft cut operator, if A succeeds, it is A,B 
%                                           if A fails, it is \+A,C 
% either B or C will be run, never both.

% a less noisy version of the above
exec_s(X,O) :- parse(X,S),!,transform(S,O).



% now we define a number of useful testing predicates
% in terms of exec.
expect_fail(Code) :- findall(X,exec_s(Code,X),Output), \+ Output = []-> (writef('"%s" gave "%w" not failure\n',[Code, Output]), !, fail);[].
expect(Code,Output) :-  exec(Code,X),!, (\+var(X),(Output = X) -> []; writef('"%s" is %w not %w\n',[Code, X, Output])).
expect(Code,Output) :-  !, writef('"%s" failed, not %w\n',[Code, Output]).

% we can define test(name, -Output) where Output is pass or fail
% these can be defined anywhere, and there is a test runner at the
% bottom
:- discontiguous test/2.

% here is an example test.
test(example, O) :- (
    []
    ) -> O = pass; O = fail.


%% parser skeleton

% there are some helper methods defined later for
% whitespace (ws=/ +/ ws0=/ */) newlines and lookahead

% we define a simple parser skeleton
parse(X,S) :- phrase(document(S),X),!. 

:- discontiguous document/1.

% expressions
document([]) --> !.



%% transform
transform(X,X).


% placeholder test
test(placeholder, O) :- (
    []
    ) -> O = pass; O = fail. 

%% language engine starts here.

% useful functions for parsing:
% lookahead a token.
lookahead(X),X --> X.

% common parse tokens.
% whitespace helpers ws means specific whitespace, ws0 means any.
ws --> [X], {code_type(X, white)}, ws0.
ws0 --> ws.
ws0 --> [].

% hello cr lf.
newline --> [10], linefeed. 
linefeed --> [13]; [].

% helper functions to run the interpreter
% from shell - either loads a file or reads from stdin
% and calls exec(String, _).

start_script :-
    catch(main_script,E,(print_message(error,E),fail)),
    halt.
start_script :-
    halt.

start_compile :-
    catch(main_compile,E,(print_message(error,E),fail)),
    halt.
start_compile :-
    halt.

% remove all swi prolog arguments
clean_arguments([--],H,[H,[],[]]).
clean_arguments([--|T],H,[H|T]).
clean_arguments([H|T],_,O) :-
    clean_arguments(T,H,O).

% pulling them together
main_script :-
    current_prolog_flag(argv,X),
    clean_arguments(X,[],[_,File|_]),
    run_file(File).

main_compile :-
    current_prolog_flag(argv,[_,X|_]),
    run_file(X).

% run a given file
run_file(File) :- 
    prompt(_,''),
    (\+File = [] -> (
        string_to_atom(File,Name),
        open(Name,read,I)
        );
        current_input(I)
    ),
    read_file(I,[],Code),
    exec(Code,Output),
    !,
    write(Output).


read_file(I,Li,Lo) :-
    get_code(I,C),(
        (C = -1,!,Lo=[]);
        Lo=[C|L1], 
        read_file(I,Li,L1)
    ).  

% test runner is the last thing to run

test_out([]).
test_out([[_,pass]|T]) :- test_out(T).
test_out([[N,O]|T]) :- writef('%w %w\n',[O,N]), test_out(T).
:- findall([T,O],test(T,O), L),  test_out(L).
