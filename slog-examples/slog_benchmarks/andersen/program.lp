pointsTo(Y,X):-addressOf(Y,X).
pointsTo(Y,X):-assign(Y,Z),pointsTo(Z,X).
pointsTo(Y,W):-load(Y,X),pointsTo(X,Z),pointsTo(Z,W).
pointsTo(Z,W):-store(Y,X),pointsTo(Y,Z),pointsTo(X,W).

pointsTo(X,Y)?
