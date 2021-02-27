
select count(*) from evaluation A, evaluation B where A.test = B.test and A.checksum = B.checksum and A.version = B.version and A.id < B.id ;
select A.score, B.score from evaluation A, evaluation B where A.test = B.test and A.checksum = B.checksum and A.version = B.version and A.id < B.id  and A.score <> B.score;

select A.score, B.score from evaluation A, evaluation B where A.test = B.test and A.checksum = B.checksum and A.version is null and B.version is not null and A.id <> B.id  and A.score <> B.score;
