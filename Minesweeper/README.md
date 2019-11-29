# Minesweeper

Functionally implemented version of Minesweeper

Currently is a console UI with interaction through commands.

Commands in the form of:
[action] "/" [row no.] "/" [col no.]
where actions are:
'r' for reveal
'f' for flag
'a' to autoplay a move n.b you will need to provide dummy coordinates with this command but they will be ignored

and
0 <= [row no.] <= 9
0 <= [col no.] <= 9

E.g. r/1/1
