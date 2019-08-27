
/*insert into version("commit", major, minor, patch, description) select R.current_commit, 1, 0, 0, 'taken over' from challenge C, repo R where C.private_repo = R.id ;

update challenge set version = (select R.current_commit from repo R where private_repo = R.id);*/

update submission set version = (select R.current_commit from repo R, challenge C where C.private_repo = R.id and challenge = C.id);
