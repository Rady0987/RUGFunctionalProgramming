% some facts
child(john,sue).  % fact saying that sue is a child of john.
child(john,sam).  % fact saying that sam is a child of john.
child(jane,sue).  % fact saying that sue is a child of jane.
male(john).       % fact saying that john is a male.
% some rules
parent(Y,X) :- child(X,Y).            % if Y is a child of X, then X is a parent of Y
father(Y,X) :- child(X,Y), male(Y).   % if Y is a child of X and Y is a male then Y is the father of X.
% some queries
?- child(john,sue).
?- child(john,john).
?- male(john).
?- male(robert).
?- male(jane).
?- male(eve).
?- male(X).
?- child(john,X).
?- parent(sue,X).
?- father(X,Y).
?- child(X,X).
